################################################################################
##                       FRAME cohort selection                               ##
################################################################################
req_pkgs <- c("DBI", "duckdb", "myomoptools", "tidyverse", "dbplyr", "dtplyr",
             "data.table", "bit64", "writexl")

invisible(suppressPackageStartupMessages(
  lapply(req_pkgs, library, character.only = FALSE, logical.return = TRUE)))
rm(req_pkgs)
##  If not already run, run setup
# source(file.path(getwd(), "1_FRAME_setup.R"))
################################################################################
## If not run already, run drugidentification script
# source(file.path(getwd(), "2_FRAME_drug_identification.R"))
################################################################################
# Find mothers with nifedipine / labetalol prescribed in pregnancy
frame_drugs_events <- cdm$drug_exposure %>% 
  inner_join(cdm$frame_drugs %>% 
               mutate(drug_concept_id = as.integer(drug_concept_id)),
             by = "drug_concept_id") 

# Find pregnancy condition concept
pregconcept <- myomoptools::find_concepts(
  cdm,
  keyword = "^Pregnancy$", 
  domain = "Condition",
  vocab_id = "SNOMED",
  standard_only = TRUE) %>% 
  pull(concept_id)

# Create empty exclusions table collect exclusion counts
exclusions <- tibble(
  exclusion    = character(),
  mothers_left = integer(),
  preg_left    = integer(),
  stringsAsFactors = FALSE
)

# Compile table for study while tracking exclusions.
rx_in_preg <- cdm$condition_occurrence %>% 
  # get only condition_concept_ids for pregnancy
  filter(condition_concept_id == pregconcept) %>%
  distinct(condition_occurrence_id, person_id, condition_start_date,
           condition_end_date) %>%
  {
    Total <- data.frame(
      summarise(
        .,
        exclusions   = "1. Initial Cohort",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id)
      )
    )
    exclusions <<- rbind(exclusions, Total)
    .
  } %>%
  # filter mothers who have prescriptions from FRAME drugs only
  inner_join(frame_drugs_events,
             by = c("person_id" = "person_id")) %>%
  {
    framedrugs <- data.frame(
      summarise(
        .,
        exclusions   = "2. Rx for labetalol/nifedipine",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id)
      )
    )
    exclusions <<- rbind(exclusions, framedrugs)
    .
  } %>%
  # filter to keep only prescriptions 90 days preconception or through pregnancy
  mutate(precon = as.Date(condition_start_date - days(90))) %>% 
  filter(between(drug_exposure_start_date, precon, condition_end_date)) %>%
  {
    pre2in <- data.frame(
      summarise(
        .,
        exclusions   = "3. Rx 90 days pre-pregnancy to end",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id)
      )
    )
    exclusions <<- rbind(exclusions, pre2in)
    .
  } %>% 
  # Remove mothers without FRAME drugs between pregnancy start and end dates
  group_by(person_id, condition_occurrence_id) %>%  
  filter(drug_exposure_start_date >= precon &
           drug_exposure_start_date <= condition_end_date &
           any(drug_exposure_start_date >= condition_start_date &
                 drug_exposure_start_date <= condition_end_date,
               na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Removed pre-existing if no record during pregnancy
  {
    durpreg <- data.frame(
      summarise(
        .,
        exclusions   = "4. Rx during pregnancy",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id),
        .groups = "drop"
      )
    )
    exclusions <<- rbind(exclusions, durpreg)
    .
  } %>% 
  group_by(person_id, condition_occurrence_id) %>% 
  left_join(cdm$person %>% 
              mutate(birth_datetime = as.Date(birth_datetime)) %>% 
              select(person_id, birth_datetime)) %>% 
  group_by(person_id, condition_occurrence_id) %>% 
  mutate(date_frst_rx_precon = min( # Date of 1st Rx within 90days b4 preg start
    if_else(between(drug_exposure_start_date, precon, condition_end_date),
            drug_exposure_start_date, NA_Date_), na.rm = TRUE),
    date_frst_rx_preg = min( # Date 1st Rx within pregnancy dates
      if_else(between(drug_exposure_start_date, condition_start_date,
                      condition_end_date),
              drug_exposure_start_date, NA_Date_), na.rm = TRUE),
    age_frst_rx_precon = # Maternal age at first preconception Rx
      (min(if_else(between(drug_exposure_start_date, precon, 
                           condition_start_date),
                   drug_exposure_start_date, NA_Date_),
           na.rm = TRUE) - as.Date(birth_datetime))/365.25,
    age_frst_rx_preg = # Maternal age at first Rx during pregnancy
      (min(if_else(between(drug_exposure_start_date,condition_start_date,
                           condition_end_date),
                   drug_exposure_start_date, NA_Date_),
           na.rm = TRUE) - as.Date(birth_datetime))/365.25,
    startdrug_preg   = max(if_else( # Which drug was started first in pregnancy
      drug_exposure_start_date == date_frst_rx_preg, 
      ingredient_name,
      NA_character_), na.rm = TRUE),
    startdrug_precon = max(if_else(# Which drug started first in preconception
      drug_exposure_start_date == date_frst_rx_precon,
      ingredient_name,
      NA_character_), na.rm = TRUE)) %>% 
  # Remove mothers who were not 18+ at first Rx during pregnancy
  filter(age_frst_rx_preg >= 18) %>% 
  ungroup() %>% 
  {
    df <- .
    
    age18plus <- df %>% 
      filter(age_frst_rx_preg >= 18) %>% 
      summarise(
        .,
        exclusions   = "5. 18+ years old",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id),
        .groups = "drop"
      ) %>% 
      collect()
    
    exclusions <<- rbind(exclusions, age18plus)
    df
  } %>% 
  # Get remaining counts of labetalol 1st pregnancy Rxs
  {
    labs <- data.frame(
      summarise(
        .,
        exclusions   = "6. Labetalol only",
        mothers_left = n_distinct(person_id[startdrug_preg == "labetalol"], na.rm = TRUE),
        preg_left    = n_distinct(condition_occurrence_id[startdrug_preg == "labetalol"], na.rm = TRUE),
        .groups = "drop"
      )
    )
    exclusions <<- rbind(exclusions, labs)
    .
  } %>% 
  # Get remaining counts of nifedipine 1st pregnancy Rxs
  {
    nifs <- data.frame(
      summarise(
        .,
        exclusions   = "7. Nifedipine only",
        mothers_left = n_distinct(person_id[startdrug_preg == "nifedipine"], na.rm = TRUE),
        preg_left    = n_distinct(condition_occurrence_id[startdrug_preg == "nifedipine"], na.rm = TRUE),
        .groups = "drop"
      ) 
    )
    exclusions <<- rbind(exclusions, nifs)
    .
  } %>% 
  distinct(person_id, condition_occurrence_id, condition_start_date,
           condition_end_date, preconception_start = precon,
           mat_birthdate = birth_datetime, date_frst_rx_precon, 
           startdrug_precon, age_frst_rx_precon, date_frst_rx_preg, 
           startdrug_preg, age_frst_rx_preg) %>% 
  compute(name = "rx_in_preg", temporary = TRUE, overwrite = TRUE)

# Add detail to the cdm lazy_tbl list
cdm[["rx_in_preg"]] <- tbl(con, "rx_in_preg")

# Save the exclusions table to xlsx
writexl::write_xlsx(
  x = exclusions,
  path = file.path(getwd(), "Exclusions.xlsx")
)

rm(exclusions, pregconcept)
################################################################################
## Create FRAME duckdb
################################################################################
# Account for fathers data and remove from FRAME tables, if present.
fathers <- cdm$fact_relationship %>% 
  {
    if (nrow(collect(head(filter(., relationship_concept_id == 4283070), 1))) > 0) {
      filter(.,
             domain_concept_id_1 == 1147314 &
               relationship_concept_id == 4283070) %>% 
        distinct(person_id = fact_id_1)
      } else {
        NULL
      }
  }

# get record of mother-baby relationships
mumsbabies <- cdm$rx_in_preg %>%
  distinct(person_id, condition_occurrence_id) %>% 
  left_join(cdm$fact_relationship %>% 
              filter(domain_concept_id_2 == 1147333 & # condition_occurrence
                       domain_concept_id_1 == 1147314 & # person
                                                     # Child   Infant
                       relationship_concept_id %in% c(4285883, 4305451)) %>% 
              select(baby_id = fact_id_1, condition_occurrence_id = fact_id_2),
            by = "condition_occurrence_id") %>% 
  distinct()

# Get singular list of all person_ids for extracting data from other tables.
cohortids <- mumsbabies %>% 
  select(person_id) %>% 
  union_all(mumsbabies %>% 
              select(person_id = baby_id)) %>% 
  distinct()

# Create filtered fact_relationship table which only applies to cohort person_ids
filtrd_fr <- cdm$fact_relationship %>% 
  # remove fathers if present
  {
    if (!is.null(fathers)) {
      father_ids <- fathers %>% 
        mutate(domain_person = 1147333L)
      
      anti_join(., 
                father_ids,
                by = c("fact_id_1" = "person_id", 
                       "domain_concept_id_1" = "domain_person")) %>% 
        anti_join(father_ids,
                  by = c("fact_id_2" = "person_id", 
                         "domain_concept_id_2"  = "domain_person"))
    } else {
      .
    }
  }

frame_fact_rel <- filtrd_fr %>% 
  # Keep all realtionships which do not include a person_id
  filter(domain_concept_id_1 != 1147314L, domain_concept_id_2 != 1147314L) %>% 
  # add fr person_ids in fact_id_1
  union_all(
    filtrd_fr %>% 
      filter(domain_concept_id_1 == 1147314L, domain_concept_id_2 != 1147314L) %>% 
      semi_join(cohortids %>% select(person_id),
                by = c("fact_id_1" = "person_id"))
  ) %>% 
  # add fr person_ids in fact_id_2
  union_all(
    filtrd_fr %>% 
      filter(domain_concept_id_1 != 1147314L, domain_concept_id_2 == 1147314L) %>% 
      semi_join(cohortids %>% select(person_id),
                by = c("fact_id_2" = "person_id"))
  ) %>% 
  # add where both are person_ids and are contained in the cohort
  union_all(
    filtrd_fr %>% 
      filter(domain_concept_id_1 == 1147314L, domain_concept_id_2 == 1147314L) %>% 
      semi_join(cohortids %>% select(person_id),
                by = c("fact_id_1" = "person_id")) %>% 
      dplyr::union(
        filtrd_fr %>%
          filter(domain_concept_id_1 == 1147314L, domain_concept_id_2 == 1147314L) %>% 
          semi_join(cohortids %>% select(person_id),
                    by = c("fact_id_2" = "person_id"))
        )
    )
################################################################################                
## Attach target database for FRAME
target_db_path <- file.path(getwd(), "frm_cdm.duckdb")
src_con <- dbplyr::remote_con(cdm[[names(cdm)[1]]])

attached_dbs <- dbGetQuery(src_con, "PRAGMA database_list;")$name
if ("frame_db" %in% attached_dbs) {
  dbExecute(src_con, "DETACH frame_db")
}
rm(attached_dbs)

dbExecute(src_con, sprintf("ATTACH '%s' AS frame_db;", target_db_path))
################################################################################              
# Export CDM tables to target duckdb
write_to_frm <- function(lazy_tbl, table_name) {
  sql_str <- dbplyr::sql_render(lazy_tbl)
  dbExecute(src_con, sprintf("CREATE OR REPLACE TABLE frame_db.%s AS %s;",
                             table_name, sql_str))
}

# 1. Athena vocabulary 
vocab_tables <- c("concept", "vocabulary", "domain", "concept_class", 
                  "concept_relationship", "concept_synonym", "concept_ancestor",
                  "drug_strength", "relationship")

for (tbl in intersect(vocab_tables, names(cdm))) {
  write_to_frm(cdm[[tbl]], tbl)
}

# 2. Direct person-level cdm tables
person_tables <- c("condition_occurrence", "device_exposure", "drug_exposure",
                    "episode", "measurement", "observation",       
                    "person", "procedure_occurrence", "specimen",
                    "observation_period", "visit_occurrence", "death", 
                    "drug_era", "dose_era", "episode")

for (tbl in intersect(person_tables, names(cdm))) {
  filtered_lazy <- cdm[[tbl]] %>% 
    semi_join(cohortids, by = "person_id")
  write_to_frm(filtered_lazy, tbl)
}

# 3. Prefiltered fact_relationship
if (exists("frame_fact_rel")) {
  write_to_frm(frame_fact_rel, "fact_relationship")
}

# 4. Relational/linked tables (only actions if both table pairs exist in cdm)
if ("episode_event" %in% names(cdm) && "episode" %in% names(cdm)) {
  filtered_episodes <- cdm$episode %>% semi_join(cohortids, by = "person_id")
  filtered_ep_events <- cdm$episode_event %>% 
    semi_join(filtered_episodes %>% select(episode_id), by = "episode_id")
  write_to_frm(filtered_ep_events, "episode_event")
}

# 5. Previously created frame datasets
if (exists("frame_drugs")) {
  write_to_frm(frame_drugs, "frame_drugs")
}
  # reference key map for raw ids, pseudonymised, omoped and pregnancy ids
if ("preg_reference" %in% names(cdm)) {
  filtered_pregs <- cdm$preg_reference %>% 
    {
      if (!is.null(fathers)) {
        cdm$preg_reference %>% 
          select(-c(FAKE_DAD_ALF, DAD_HASH, dad_cdm_id)) 
      } else {
        .
      }
    } %>% 
    semi_join(mumsbabies %>% 
                select(mat_cdm_id = person_id, baby_cdm_id = baby_id),
              by = c("mat_cdm_id", "baby_cdm_id"))
  
  write_to_frm(filtered_pregs, "preg_reference")
}

if (exists("frame_drugs_events")) {
  write_to_frm(frame_drugs_events, "frame_drugs_events")
}

if (exists("rx_in_preg")) {
  write_to_frm(rx_in_preg, "rx_in_preg")
}

dbExecute(src_con, "DETACH frame_db;")

frm <- dbConnect(duckdb::duckdb(), dbdir = target_db_path,
                 config = 
                   list("temp_directory" = 
                          normalizePath(tempdir(), winslash = "/")))

# Get a list of duckdb lazytables
duck_tables <- dbListTables(frm)

frame <- lapply(duck_tables, function(table_name) {
  tbl(frm, table_name)
})
names(frame) <- duck_tables

dbRemoveTable(con, "frame_drugs")

dbDisconnect(con, shutdown = TRUE)

rm(duck_tables, frame_drugs_events, fathers, mumsbabies, cohortids, filtrd_fr,
   frame_fact_rel, src_con, write_to_frm, vocab_tables, person_tables,
   filtered_ep_events, filtered_episodes, filtered_lazy, filtered_pregs, 
   tbl, con, cdm, rx_in_preg)
gc()

dbDisconnect(frm, shutdown = TRUE)
rm(frame, frm)
gc()







