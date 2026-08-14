################################################################################
##                       FRAME cohort selection                               ##
################################################################################
## Run setup
# source(file.path(getwd(), "1_FRAME_setup.R"))
################################################################################
## Run script to get drug_concept_id for all nifedipine/labetalol oral products
# source(file.path(getwd(), "2_FRAME_drug_identification.R"))
################################################################################
## Run script to create the FRAME duckdb
# source(file.path(getwd(), "3_FRAME_duckdb_creation.R"))
################################################################################
req_pkgs <- c("DBI", "duckdb", "myomoptools", "tidyverse", "dbplyr", "dtplyr",
             "data.table", "bit64", "writexl")

invisible(suppressPackageStartupMessages(
  lapply(req_pkgs, library, character.only = FALSE, logical.return = TRUE)))
rm(req_pkgs)
################################################################################
# connect to frame duckdb
con <- dbConnect(duckdb::duckdb(), dbdir = target_db_path,
                 config = 
                   list("temp_directory" = 
                          normalizePath(tempdir(), winslash = "/")))

# Get a list of duckdb lazytables
duck_tables <- dbListTables(con)

frame <- lapply(duck_tables, function(table_name) {
  tbl(con, table_name)
})
names(frame) <- duck_tables

rm(duck_tables)
################################################################################
# Identify concepts
pregconcept <- myomoptools::find_concepts(
  frame,
  keyword = "^Pregnancy$", 
  domain = "Condition",
  vocab_id = "SNOMED",
  standard_only = TRUE) %>% 
  pull(concept_id)

# Diabetes in pregnancy        
# 4058243 = Diabetes mellitus during pregnancy, childbirth and the puerperium
diabpreg <- myomoptools::get_descendants(frame, 4058243) %>%
  pull(descendant_concept_id)
diabpreg <- c(4058243, diabpreg)

# Other diabetes
# 201820 = Diabetes mellitus
diabetes <- setdiff(
  myomoptools::get_descendants(frame, 201820) %>% 
    pull(descendant_concept_id),
  diabpreg)

# Index of multiple deprivation observation codes by country
#        Wales     Scotland  England
imd <- c(35812898, 35812898, 35812898)

# Unfiltered - all respiratory disorders       
respiratory <- myomoptools::get_descendants(frame, 317009) %>% 
  union_all(myomoptools::get_descendants(frame, 42873168)) %>% 
  union_all(myomoptools::get_descendants(frame, 4063381)) %>% 
  union_all(myomoptools::get_descendants(frame, 974952)) %>% 
  union_all(myomoptools::get_descendants(frame, 1340232)) %>% 
  union_all(myomoptools::get_descendants(frame, 4224259)) %>% 
  union_all(myomoptools::get_descendants(frame, 42539027)) %>% 
  union_all(myomoptools::get_descendants(frame, 313459)) %>% 
  union_all(myomoptools::get_descendants(frame, 4044240)) %>% 
  union_all(myomoptools::get_descendants(frame, 43531585)) %>% 
  union_all(myomoptools::get_descendants(frame, 37110295)) %>% 
  union_all(myomoptools::get_descendants(frame, 40483220)) %>% 
  union_all(myomoptools::get_descendants(frame, 4247108)) %>% 
  union_all(myomoptools::get_descendants(frame, 317971)) %>% 
  union_all(myomoptools::get_descendants(frame, 4005302)) %>% 
  union_all(myomoptools::get_descendants(frame, 196724)) %>% 
  union_all(myomoptools::get_descendants(frame, 37395588)) %>% 
  union_all(myomoptools::get_descendants(frame, 4049972)) %>% 
  union_all(myomoptools::get_descendants(frame, 4028286)) %>% 
  union_all(myomoptools::get_descendants(frame, 4060429)) %>% 
  pull(descendant_concept_id)
respiratory <- c(317009,   42873168, 4063381, 974952,  1340232, 
                 4224259,  42539027, 313459,  4044240, 43531585,
                 37110295, 40483220, 4247108, 317971,  4005302,
                 196724,   37395588, 4049972, 4028286, 4060429, respiratory)

# All hypertension
# 316866 = Hypertensive disorder
hypertension <- myomoptools::get_descendants(frame, 316866) %>% 
  pull(descendant_concept_id)
hypertension <- c(316866, hypertension)

# pre-eclampsia
# 439393 = Pre-eclampsia
preeclampsia <- myomoptools::get_descendants(frame, 439393) %>% 
  pull(descendant_concept_id)
preeclampsia <- c(439393, preeclampsia)

# eclampsia
# 443700
eclampsia <- myomoptools::get_descendants(frame, 443700) %>% 
  pull(descendant_concept_id)
eclampsia <- c(443700, eclampsia)

# Other cardiac conditions except hypertension      
# 134057 = Disorder of cardiovascular system
cvd <- myomoptools::get_descendants(frame, 134057) %>% 
  pull(descendant_concept_id)
cvd <- c(134057, cvd)

conditions <- list(pregconcept, diabpreg, diabetes, imd, 
                   respiratory, hypertension, preeclampsia,
                   eclampsia, cvd)
names(conditions) <- c("pregconcept", "diabpreg", "diabetes", "imd", 
                       "respiratory", "hypertension", "preeclampsia",
                       "eclampsia", "cvd")

rm(pregconcept, diabpreg, diabetes, imd, 
   respiratory, hypertension, preeclampsia,
   eclampsia, cvd)
################################################################################
################################################################################
# Table for writing variable counts
deets <- data.frame(
  variable   = as.character(),
  cats       = as.character(),
  nifedipine = as.integer(),
  labetalol  = as.integer()
)

# Table for recording gestational ages at first R in pregnancy
gestages <- data.frame(
  mean_ga_nifedipine = as.numeric(),
  sd_ga_nifedipine   = as.numeric(),
  mean_ga_labetalol  = as.numeric(),
  sd_ga_labetalol    = as.numeric()
)

# Build baseline table parameters
base_table <- frame$rx_in_preg %>%
  # Ethnicity
  left_join(frame$observation %>% 
              select(person_id, observation_concept_id) %>% 
              filter(observation_concept_id %in% 
                       # White, Black, Asian, Mixed, Other
                       c(3959326, 3959330, 3959336, 3959338, 3959547))) %>% 
  mutate(ethnicity = case_when(
    observation_concept_id == 3959326 ~ "White",
    observation_concept_id == 3959330 ~ "Black",
    observation_concept_id == 3959336 ~ "Asian",
    observation_concept_id == 3959338 ~ "Mixed",
    observation_concept_id == 3959547 ~ "Other",
    TRUE ~ "Missing")) %>% 
  select(-observation_concept_id) %>% 
  group_by(ethnicity) %>% 
  {
    eth <- data.frame(
      summarise(.,
                nifedipine = sum(if_else(startdrug_preg == "nifedipine", 1L, 0L), na.rm = TRUE),
                labetalol  = sum(if_else(startdrug_preg == "labetalol", 1L, 0L), na.rm = TRUE),
                .groups = "drop"
      ) %>% 
        collect() %>% 
        mutate(variables = "ethnicity",
               cats = factor(ethnicity,
                             levels = c("White", "Black", "Asian", "Mixed",
                                        "Other", "Missing"))) %>% 
        select(variables, cats, nifedipine, labetalol) %>% 
        arrange(cats)
    )
    deets <<- rbind(deets, eth)
    .
  } %>% 
  # Singletons/Multiples - AT birth
  left_join(frame$fact_relationship %>% 
              filter(domain_concept_id_2 == 1147333 & # condition_occurrence
                       domain_concept_id_1 == 1147314 & # person
                       #                              Infant,  Child
                       relationship_concept_id %in% c(4305451, 4285883)) %>%
              distinct(condition_occurrence_id = fact_id_2, fact_id_1) %>% 
              group_by(condition_occurrence_id) %>% 
              tally() %>% 
              mutate(singleton = case_when(n == 1 ~ 1,
                                           TRUE ~ 0)) %>% 
              select(condition_occurrence_id, singleton),
            by = "condition_occurrence_id") %>% 
  group_by(singleton) %>% 
  {
    sgl <- data.frame(
      summarise(.,
                nifedipine = sum(if_else(startdrug_preg == "nifedipine", 1L, 0L), na.rm = TRUE),
                labetalol  = sum(if_else(startdrug_preg == "labetalol", 1L, 0L), na.rm = TRUE),
                .groups = "drop"
      ) %>% 
        collect() %>% 
        mutate(variables = "birth number",
               cats = factor(singleton, 
                             levels = c(1, 0),
                             labels = c("Singleton Birth", "Multiple birth"))) %>% 
        select(variables, cats, nifedipine, labetalol) %>% 
        arrange(cats)
    )
    deets <<- rbind(deets, sgl)
    .
  } %>% 
  # Singletons/Multiples: Max number of fetuses in pregnancy
  left_join(frame$measurement %>% 
              filter(measurement_concept_id == 4077859) %>% # Number of fetuses
              distinct(person_id, measurement_date, value_as_number),
            by =join_by(
              person_id == person_id,
              between(y$measurement_date, x$condition_start_date, 
                      x$condition_end_date))) %>% 
  group_by(condition_occurrence_id) %>% 
  # filter out multiple birth counts, using the maximum count as early counts
  # can miss multiples and thus increase when twins are seen later.
  filter(value_as_number  == max(value_as_number, na.rm = TRUE) |
           # in case there are births without a count...
           all(is.na(value_as_number))) %>% 
  filter(measurement_date == min(measurement_date, na.rm = TRUE) |
           # in case there are births without a count...
           all(is.na(value_as_number))) %>% 
  mutate(num_fetus = case_when(value_as_number == 1 ~ "Singleton Pregnancy",
                               value_as_number  > 1 ~ "Multiple Pregnancy",
                               TRUE ~ NA_character_)) %>% 
  select(-c(value_as_number, measurement_date)) %>% 
  group_by(num_fetus) %>% 
  {
    nfet <- data.frame(
      summarise(.,
                nifedipine = sum(if_else(startdrug_preg == "nifedipine", 1L, 0L), na.rm = TRUE),
                labetalol  = sum(if_else(startdrug_preg == "labetalol", 1L, 0L), na.rm = TRUE),
                .groups = "drop"
      ) %>% 
        collect() %>% 
        mutate(variables = "number of fetuses",
               cats = factor(num_fetus)) %>% 
        select(variables, cats, nifedipine, labetalol) %>% 
        arrange(desc(cats))
    )
    deets <<- rbind(deets, nfet)
    .
  } %>% 
  ## Diabetes
  left_join(., 
            frame$condition_occurrence %>% 
              filter(condition_concept_id %in% conditions$diabpreg |
                       condition_concept_id %in% conditions$diabetes) %>% 
              select(person_id, 
                     diab_id    = condition_concept_id, 
                     diab_start = condition_start_date,
                     diab_end   = condition_end_date),
            by = "person_id") %>% 
  group_by(person_id, condition_occurrence_id) %>% 
  mutate(gest_diab = max(
    if_else(
      diab_id %in% conditions$diabpreg &
        diab_start >= condition_start_date &
        diab_start <= condition_end_date,
      1L,
      0L
    ),
    na.rm = TRUE),
    preexist_diab = max(
      if_else(
        diab_id %in% conditions$diabetes &
          diab_start <= condition_end_date,
        1L,
        0L
      ),
      na.rm = TRUE),
    either_diab = max(
      if_else(
        gest_diab == 1L | preexist_diab == 1L,
        1L,
        0L
      ),
      na.rm = TRUE)) %>%
  select(-c(diab_id, diab_start, diab_end)) %>% 
  distinct() %>% 
  group_by(either_diab) %>% 
  {
    diab <- data.frame(
      summarise(.,
                nifedipine = sum(if_else(startdrug_preg == "nifedipine", 1L, 0L), na.rm = TRUE),
                labetalol  = sum(if_else(startdrug_preg == "labetalol", 1L, 0L), na.rm = TRUE),
                .groups = "drop"
      ) %>% 
        collect() %>% 
        mutate(variables = "diabetes present",
               cats = factor(either_diab, 
                             levels = c(0, 1),
                             labels = c("no diabetes", "diabetes present"))) %>% 
        select(variables, cats, nifedipine, labetalol)) %>% 
      filter(cats == "diabetes present")
    
    deets <<- rbind(deets, diab)
    .
  } %>% 
  left_join(frame$observation %>%
              filter(observation_concept_id %in% conditions$imd) %>% 
              distinct(person_id, observation_date, value_as_number)) %>%
  group_by(person_id, condition_occurrence_id) %>% 
  mutate(date_dist = abs(observation_date - condition_start_date),
         min_dist  = min(date_dist, na.rm = TRUE),
         pregimd   = max(
           if_else(date_dist == min_dist, value_as_number, NA_real_), na.rm = TRUE),
         imd = case_when(pregimd %in% c(1:2) ~ "Q1-2",
                         pregimd %in% c(3:5) ~ "Q3-5",
                         TRUE ~ "Missing")) %>% 
  select(-c(observation_date, value_as_number, pregimd, date_dist, min_dist)) %>% 
  distinct() %>% 
  group_by(imd) %>% 
  {
    deprv <- data.frame(
      summarise(.,
                nifedipine = sum(if_else(startdrug_preg == "nifedipine", 1L, 0L), na.rm = TRUE),
                labetalol  = sum(if_else(startdrug_preg == "labetalol", 1L, 0L), na.rm = TRUE),
                .groups = "drop"
      ) %>% 
        collect() %>% 
        mutate(variables = "deprivation quintile",
               cats = factor(imd,
                             levels = c("Q1-2", "Q3-5", "Missing"))) %>% 
        select(variables, cats, nifedipine, labetalol) %>% 
        arrange(cats)
    )
    
    deets <<- rbind(deets, deprv)
    .
  } %>% 
  ungroup() %>% 
  mutate(gestage_frst_rx = (date_frst_rx_preg - condition_start_date)/7) %>%
  {
    g_age <- data.frame(
      summarise(.,
                mean_ga_nifedipine = mean(if_else(startdrug_preg == "nifedipine", gestage_frst_rx,
                                                  NA_real_), na.rm = TRUE),
                sd_ga_nifedipine   = sd(if_else(startdrug_preg   == "nifedipine", gestage_frst_rx,
                                                NA_real_), na.rm = TRUE),
                mean_ga_labetalol  = mean(if_else(startdrug_preg == "labetalol", gestage_frst_rx,
                                                  NA_real_), na.rm = TRUE),
                sd_ga_labetalol    = sd(if_else(startdrug_preg   == "labetalol", gestage_frst_rx,
                                                NA_real_), na.rm = TRUE)
      ) 
    )
    
    gestages <<- rbind(gestages, g_age)
    .
  } %>% 
  compute(name = "base_stats", temporary = TRUE, overwrite = TRUE)
  
frame[["base_stats"]] <- tbl(con, "base_stats")

writexl::write_xlsx(
  x = list(
    "BaseCounts" = deets,
    "Gestation"  = gestages
  ),
  path = file.path(getwd(), "Unmatched Details.xlsx")
)

rm(deets, gestages, base_table)
################################################################################
## Additional details required for matching
## remove the no longer required vectors from conditions list first
conditions <- conditions[
  !names(conditions) %in% c("pregconcept", "diabpreg", "diabetes", "imd")]

conds <- data.frame(
  condition_concept_id = unique(unlist(conditions, use.names = FALSE)))

copy_to(dest = con,
        df = conds,
        name = "conds",
        overwrite = TRUE,
        temporary = TRUE)

frame[["conds"]] <- tbl(con, "conds")

rm(conds)

# Filter condition_occurrences for diagnoses of interest
cond_frame <- frame$condition_occurrence %>%
  inner_join(frame$conds) %>% 
  mutate(condgrp = case_when(
    # respiratory list likely needs reducing as currently contains all
    condition_concept_id %in% conditions$respiratory ~ "respiratory",
    # omop hypertensive disorder includes preeclampsia so remove here
    condition_concept_id %in% conditions$hypertension &
      !condition_concept_id %in% conditions$preeclampsia ~ "hypertension",
    condition_concept_id %in% conditions$preeclampsia ~ "preeclampsia",
    # eclampsia not a HTNve disorder - seizure disorder
    condition_concept_id %in% conditions$eclampsia ~ "eclampsia",
    # cardiovascular disorders include hypertensive disorders so remove here
    condition_concept_id %in% conditions$cvd &
      !condition_concept_id %in% conditions$hypertension ~ "cvd")) %>% 
  select(person_id, condgrp, startdt = condition_start_date,
         enddt = condition_end_date) %>% 
  distinct() %>% 
  compute(., name = "cond_frame", temporary = TRUE, overwrite = TRUE)

frame[["cond_frame"]] <- tbl(con, "cond_frame")

rm(cond_frame)
dbRemoveTable(con, "conds")
################################################################################
# add to base table
framemothers <- frame$base_stats %>% 
  mutate(gest_age_group = case_when(gestage_frst_rx < 11 ~ "0-10 weeks",
                                    gestage_frst_rx >= 11 &
                                      gestage_frst_rx < 20 ~ "11-19 weeks",
                                    gestage_frst_rx >= 20 &
                                      gestage_frst_rx < 28 ~ "20-27 weeks",
                                    gestage_frst_rx >= 28 &
                                      gestage_frst_rx < 35 ~ "28-34 weeks",
                                    TRUE ~ ">= 35 weeks"),
         within1yr = condition_start_date - sql("INTERVAL 1 YEAR")) %>% 
  left_join(frame$cond_frame,
            by = join_by("person_id",
                         between(y$startdt, x$within1yr, x$condition_end_date))) %>% 
  group_by(person_id, condition_occurrence_id) %>% 
  mutate(gest_weeks = (startdt - condition_start_date)/7,
         is_htn_pre20 = ifelse(
           condgrp == "hypertension" & 
             startdt >= within1yr & gest_weeks < 20, 1L, 0L),
         is_htn_post20 = ifelse(
           condgrp == "hypertension" & gest_weeks > 20, 1L, 0L),
         preeclampsia = ifelse(
           condgrp %in% c("preeclampsia", "eclampsia") &
             startdt >= condition_start_date &
             startdt <= condition_end_date, 1L, 0L),
         respiratory  = ifelse(condgrp == "respiratory", 1L, 0L),
         cvd          = ifelse(condgrp == "cvd", 1L, 0L)) %>%
  mutate(htn_type = case_when(
    max(is_htn_pre20, na.rm = TRUE) == 1 ~ "chronic",
    max(is_htn_post20, na.rm = TRUE) == 1 ~ "gestational",
    TRUE ~ NA_character_),
    preeclampsia = max(preeclampsia, na.rm = TRUE),
    respiratory  = max(respiratory, na.rm = TRUE),
    cvd          = max(cvd, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-c(within1yr, condgrp, startdt, enddt, gest_weeks, is_htn_pre20,
            is_htn_post20)) %>% 
  distinct() %>% 
  mutate(gnt_pnda_by_dt = if_else(
    between(date_frst_rx_preg, as.Date("2021-06-08"), as.Date("2025-01-17")), 1L, 0L),
    gnt_pnda_by_gest = if_else(gestage_frst_rx >= 11 &
                                 gestage_frst_rx < 35, 1L, 0L)) %>% 
  compute(., name = "frame_mothers", temporary = FALSE, overwrite = TRUE)
  
frame[["frame_mothers"]] <- tbl(con, "frame_mothers")

dbRemoveTable(con, "base_stats")
dbRemoveTable(con, "cond_frame")
dbRemoveTable(con, "rx_in_preg")
################################################################################
duck_tables <- dbListTables(con)

frame <- lapply(duck_tables, function(table_name) {
  tbl(con, table_name)
})
names(frame) <- duck_tables

rm(duck_tables, framemothers, conditions, target_db_path)
################################################################################
# dbDisconnect(con, shutdown = TRUE)
# rm(frame, con)
# gc()
  
  
  
  
  
  
  
  
  
  
  






