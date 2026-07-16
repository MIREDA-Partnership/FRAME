# 1. Establish DuckDB Connection
db_path <- file.path(proj_dir, "mireda_drugs.duckdb")

con <- dbConnect(duckdb(), dbdir = db_path, read_only = FALSE)
################################################################################
# 2. Make list of lazy tables to avoid typing tbl(con, ...) each time
omop <- dbListTables(con) |> 
  (\(x) {names(x) <- tolower(x); x})() |> 
  lapply(\(x) tbl(con, x))
################################################################################
# 3. Identify valid and standard codes for single-ingredient, oral nifedipine
#    and labetalol

# --- 1. Execute the main query and write directly to DuckDB ---
omop$concept %>%
  # 1. Target ingredients
  filter(
    tolower(concept_name) %in% c("nifedipine", "labetalol"),
    concept_class_id == "Ingredient",
    vocabulary_id == "RxNorm",
    standard_concept == "S"
  ) %>%
  select(ingredient_id = concept_id, ingredient_name = concept_name) %>%
  
  # 2. Get all descendants (includes the ingredients themselves)
  inner_join(
    omop$concept_ancestor,
    by = c("ingredient_id" = "ancestor_concept_id")
  ) %>%
  
  # 3. Join back to get descendant concept details
  inner_join(
    omop$concept,
    by = c("descendant_concept_id" = "concept_id")
  ) %>%
  
  # 4. Restrict to Standard concepts & apply strict vocabulary rules:
  # -> ONLY Ingredients are allowed from RxNorm.
  # -> ALL OTHER descendant products MUST come from RxNorm Extension.
  filter(
    standard_concept == "S",
    (concept_class_id == "Ingredient" & vocabulary_id == "RxNorm") |
      (concept_class_id != "Ingredient" & vocabulary_id == "RxNorm Extension")
  ) %>%
  
  # 5. Remove combination products (strictly single ingredient)
  inner_join(
    omop$concept_ancestor %>%
      inner_join(
        omop$concept %>% filter(concept_class_id == "Ingredient"),
        by = c("ancestor_concept_id" = "concept_id")
      ) %>%
      group_by(descendant_concept_id) %>%
      summarise(num_ingredients = n(), .groups = "drop") %>%
      filter(num_ingredients == 1L) %>%
      select(descendant_concept_id),
    by = "descendant_concept_id"
  ) %>%
  
  # 6. Hybrid Dose Form / Route Verification (WITH EXCLUSION)
  left_join(
    omop$concept_relationship %>%
      filter(relationship_id %in% c(
        "RxNorm has dose form", "Has dose form", "Has dose form group", 
        "Has dose form unit", "Has route", "Has route of admin", 
        "May have route", "Has disp dose form", "Has basic dose form"
      )) %>%
      inner_join(omop$concept, by = c("concept_id_2" = "concept_id")) %>%
      filter(
        (tolower(concept_name) == "oral" | 
           grepl("oral|chewable|tablet|capsule|syrup|suspension|lozenge|powder|solution", concept_name, ignore.case = TRUE)) &
          !grepl("inject|intra", concept_name, ignore.case = TRUE)
      ) %>%
      group_by(concept_id_1) %>%
      summarise(
        is_oral_form = 1L,
        dose_form = max(concept_name, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("descendant_concept_id" = "concept_id_1")
  ) %>%
  
  # 7. Final Filter: Must be the base Ingredient OR successfully flagged as oral
  filter(concept_class_id == "Ingredient" | is_oral_form == 1L) %>%
  
  # 8. Rename to exact requested column headers
  select(
    ingredient_name,
    drug_concept_id = descendant_concept_id,
    drug_concept_name = concept_name,
    concept_class_id,
    vocabulary_id,
    dose_form
  ) %>%
  
  # 9. Write directly to DuckDB as a permanent physical table
  compute(
    name = "frame_drugs", 
    temporary = FALSE, 
    overwrite = TRUE
  )

# Append the new table reference to your local omop list ---
omop$frame_drugs <- omop$frame_drugs

# Save the table directly to CSV via DuckDB's native execution ---
dbExecute(con, "COPY frame_drugs TO 'frame_drugs.csv' (HEADER, DELIMITER ',');")
gc()
################################################################################