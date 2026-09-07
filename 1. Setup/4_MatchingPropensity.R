#' ---
#' title: "Matching & Propensity Scoring"
#' format: 
#'   docx: default
#'   gfm: default
#' toc: true
#' execute:
#'   cache: true
#'   echo: true
#'   eval: false
#'   warning: false
#'   message: false
#' editor: visual
#' ---
#' 
## -----------------------------------------------------------------------------
#| include: false 
#| eval: true
#| cache: false
knitr::opts_chunk$set(eval = TRUE)

knitr::purl(input = knitr::current_input(),
            output = paste0(
              tools::file_path_sans_ext(knitr::current_input()), ".R"),
            documentation = 2)

# render using: quarto render 4_MatchingPropensity.qmd --to all

#' 
#' ## Setup
#' 
#' If you are not already connected to your FRAME cohort, this should do it for you. Check that the *db_path* is correct before running it. *getwd()* will return the directory that you have saved this file to so if your DuckDB is in a different directory, change it first.
#' 
## -----------------------------------------------------------------------------
# Attach package libraries
req_packages <- c("bit64", "tidyverse", "dbplyr", "odbc", "DBI", "dtplyr", 
                  "data.table", "writexl", "duckdb", "myomoptools")

invisible(suppressPackageStartupMessages(
  lapply(req_packages, library, character.only = TRUE, logical.return = TRUE)))

rm(req_packages)

if (!exists("frm", inherits = FALSE)) {
  db_path <- file.path(getwd(), "frm_cdm.duckdb")
  
  frm <- dbConnect(duckdb(), 
                   dbdir = db_path, 
                   config = 
                     list("temp_directory" = 
                            normalizePath(tempdir(), winslash = "/")))
}

if (!exists("frame", inherits = FALSE)) {
  # Get a list of duckdb lazytables
  duck_tables <- dbListTables(frm)
  
  frame <- lapply(duck_tables, function(table_name) {
    tbl(frm, table_name)
  })
  names(frame) <- duck_tables
  
  rm(duck_tables, db_path)
}

#' 
#' ## Propensity table
#' 
#' Trim the *base_stats* table to keep only required variables.
#' 
## -----------------------------------------------------------------------------
proptab <- frame$base_stats %>% 
  distinct(condition_occurrence_id, frst_preg_drug, gest_age_group,
           gestage_frst_rx, htn_type, respiratory, cvd, singleton,
           either_diab, ethnicity, imd) %>% 
  mutate(outcome = if_else(frst_preg_drug == 'nifedipine', 1L, 0L),
         ethnicity = case_when(
           ethnicity %in% c("Mixed", "Other", "Missing") ~ 'White',
           TRUE ~ ethnicity
         )) %>% 
  collect()

#' 
#' ## Fit logistic regression (propensity score model)
#' 
#' The outcome is the *frst_preg_drug* (binary) representing choosing nifedipine over the first line treatment of labetalol
#' 
## -----------------------------------------------------------------------------
model <- glm(
  outcome ~ gest_age_group
  + htn_type 
  + respiratory 
  + cvd,
  data = proptab,
  family = "binomial"
)

#' 
#' Retain the linear predictors (log-odds) as the propensity score
#' 
## -----------------------------------------------------------------------------
proptab <- proptab %>% 
  mutate(response = predict(model, type = "link"))

#' 
#' ## Split dataset
#' 
#' Need to split into the intervention group frst_preg_drug == 'nifedipine' and the control group *frst_preg_drug* == 'labetalol'.
#' 
## -----------------------------------------------------------------------------
intervention <- proptab %>% filter(outcome  == 1L)
control      <- proptab %>% filter(outcome  == 0L)


#' 
#' ## Exact matching with nearest propensity score
#' 
#' Prepare a slimmed-down pool of available controls to conserve RAM.
#' 
## -----------------------------------------------------------------------------
available_controls <- control %>% 
  select(control_id = condition_occurrence_id, singleton, either_diab,
         ethnicity, control_response = response)

#' 
#' Create empty list to store matched pairs
#' 
## -----------------------------------------------------------------------------
final_matches_list <- list()

#' 
#' ## Iterative matching process
#' 
## -----------------------------------------------------------------------------
for (i in seq_len(nrow(intervention))) {
  current_case <- intervention[i, ]
  
  # Filter availabel controls for exact matches on criteria
  match_candidates <- available_controls %>% 
    filter(
      singleton   == current_case$singleton,
      either_diab == current_case$either_diab,
      ethnicity   == current_case$ethnicity
    )
  
  # If an exact match exists, find the one with the closest propensity score
  if (nrow(match_candidates) > 0) {
    
    best_match <-  match_candidates %>% 
      mutate(value_abs = abs(control_response - current_case$response)) %>% 
      slice_min(order_by = value_abs, n = 1, with_ties = FALSE)
    
    # Store the result
    final_matches_list[[i]] <- tibble(
      condition_occurrence_id = current_case$condition_occurrence_id,
      control_id              = best_match$control_id,
      response                = current_case$response,
      control_response        = best_match$control_response,
      value_abs               = best_match$value_abs
    )
    
    # Remove the matched control from the pool to ensure no replacement
    available_controls <- available_controls %>% 
      filter(control_id != best_match$control_id)
  }
}

#' 
#' ## Bind the list
#' 
## -----------------------------------------------------------------------------
matched_data <- bind_rows(final_matches_list)

#' 
#' ## Merge back to original characteristics for final review
#' 
## -----------------------------------------------------------------------------
final_data <- matched_data %>% 
  select(
    intervention_id = condition_occurrence_id, 
    control_id,
    intervention_response = response,
    control_response,
    value_abs
  ) %>% 
  left_join(proptab %>% 
              select(-response, -outcome),
            by = c("intervention_id" = "condition_occurrence_id")) %>% 
  left_join(proptab %>% 
              select(-response, -outcome),
            by = c("control_id" = "condition_occurrence_id"),
            suffix = c("_intervention", "_control"))

#' 
#' ## Export data
#' 
## -----------------------------------------------------------------------------
write.csv(final_data, "Propensity_data.csv", row.names = FALSE)

dbWriteTable(
  conn = frm,
  name = "matched_propensity",
  value = final_data,
  overwrite = TRUE
)

frame[["matched_propensity"]] <- tbl(frm, "matched_propensity")

#' 
#' ## Get matched counts for google doc
#' 
#' Reshape the final_data into long format and get counts per group.
#' 
## -----------------------------------------------------------------------------
matched_long <- bind_rows(
  final_data %>% 
    select(ends_with("_intervention")) %>% 
    rename_with(~str_remove(., "_intervention$")) %>% 
    mutate(group = "Matched nifedipine group"),
  
  final_data %>% 
    select(ends_with("_control")) %>% 
    rename_with(~ str_remove(., "_control$")) %>% 
    mutate(group = "Matched labetalol group")
)

# Helper function for table formatting
fmt_n <- function(n, total) {
  sprintf("%d (%.1f%%)", n, (n/total) * 100)
}

# Helper to make formatted count rows across intervention and controls
get_cat_row <- function(df, col, value, label_text) {
  df %>% 
    group_by(group) %>% 
    summarise(
      val_str = fmt_n(sum({{col}} == value, na.rm = TRUE), n()),
      .groups = "drop"
    ) %>% 
    pivot_wider(names_from = group, values_from = val_str) %>% 
    mutate(Variable = label_text)
}

# Empty header row generator
make_header <- function(title) {
  tibble(Variable = title,
         `Matched nifedipine group` = "",
         `Matched labetalol group`  = "")
}

## Build summary table sections
# Total Sample Size
row_number <- matched_long %>% 
  group_by(group) %>% 
  summarise(val_str = as.character(n()), .groups = "drop") %>% 
  pivot_wider(names_from = group, values_from = val_str) %>% 
  mutate(Variable = "Number")

# Ethnicity
rows_ethnicity <- bind_rows(
  make_header("Ethnicity"),
  get_cat_row(matched_long, ethnicity, "White", "White"),
  get_cat_row(matched_long, ethnicity, "Black", "Black"),
  get_cat_row(matched_long, ethnicity, "Asian", "Asian")
)

# Pregnancy
rows_pregnancy <- bind_rows(
  make_header("Pregnancy"),
  get_cat_row(matched_long, singleton, 1, "Singleton pregnancy"),
  get_cat_row(matched_long, singleton, 0, "Twin/triplet/multiple pregnancy")
)

# Diabetes
rows_diabetes <- get_cat_row(matched_long,
                             either_diab, 1, "Diabetes present")

# Gestational age (mean +/- SD)
row_gest_age_num <- matched_long %>% 
  group_by(group) %>% 
  summarise(
    val_str = sprintf("%.1f (%.1f)",
                      mean(gestage_frst_rx, na.rm = TRUE),
                      sd(gestage_frst_rx, na.rm = TRUE)),
    .groups = "drop") %>% 
  pivot_wider(names_from = group, values_from = val_str) %>% 
  mutate(Variable =
           "Average gestational age at initiation of meds (Mean (SD))")

# Gest age by category
row_gest_age_cat <- bind_rows(
  make_header("Gestational age categories"),
  get_cat_row(matched_long, gest_age_group, "0-10 weeks",  "0-10 weeks"),
  get_cat_row(matched_long, gest_age_group, "11-19 weeks", "11-19 weeks"),
  get_cat_row(matched_long, gest_age_group, "20-27 weeks", "20-27 weeks"),
  get_cat_row(matched_long, gest_age_group, "28-34 weeks", "28-34 weeks"),
  get_cat_row(matched_long, gest_age_group, ">=35 weeks",  ">=35 weeks"))

# Deprivation
rows_deprivation <- bind_rows(
  make_header("Deprivation quintile"),
  get_cat_row(matched_long, imd, "Q1-2", "Q1-2"),
  get_cat_row(matched_long, imd, "Q3-5", "Q3-5"),
  get_cat_row(matched_long, imd, "Missing", "Missing"),
)

# Assembled table
summary_table <- bind_rows(
  row_number,
  make_header("Matching variables"),
  rows_ethnicity,
  rows_pregnancy,
  rows_diabetes,
  row_gest_age_num,
  row_gest_age_cat,
  rows_deprivation
) %>% 
  select(Variable, `Matched nifedipine group`, `Matched labetalol group`)


writexl::write_xlsx(
  x = summary_table,
  path = file.path(getwd(), "Matched Details.xlsx")
)


#' 
#' ## Clean up
#' 
## -----------------------------------------------------------------------------
rm(proptab, model, intervention, control, available_controls,
   final_matches_list, current_case, match_candidates, best_match, i, fmt_n,
   get_cat_row, make_header, matched_data, matched_long, row_number,
   rows_ethnicity, rows_pregnancy, rows_diabetes, row_gest_age_num,
   row_gest_age_cat, rows_deprivation, summary_table, final_data)
gc()

#' 
