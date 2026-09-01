#' ---
#' title: "Identifying FRAME Cohort from MIREDA"
#' author: "Mike Seaborne"
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
#-----------------------------------------------------------------------------
#| include: false
#| eval: true
#| cache: false

knitr::purl(
  input = knitr::current_input(),
  output = paste0(tools::file_path_sans_ext(knitr::current_input()), ".R"),
  documentation = 2
)

#' 
#' #FRAME Cohort
#' 
#' The following scripts will extract the details of mothers from MIREDA OMOP CDMs to form the FRAME cohort. These will be mothers who have received a prescription for oral labetalol or nifedipine as a single-active ingredient during pregnancy.
#' 
#' There are differences between primary and secondary care with respect to prescribing records in administrative data. Most primary care (GP) prescriptions for hypertension in pregnancy appear to be retrospective, while hospital prescriptions are recorded on the date they were actually prescribed.
#' 
#' The script is intended to be universal so it will contain code sections relevant to either scenario and, as such, the script user will have to specify whether drug information comes from primary or secondary care. If, later users have BOTH, it is likely that using the secondary care drug data is more precise as there appears to be a lag in GP prescription dates - likely dated as per clinic/discharge letter from secondary care.
#' 
#' #Set-up files
#' 
#' Start by createing an R project for FRAME. This directory should be where you store the frame set-up files, package files, etc.
#' 
#' If you have not edited/run the files *myomoptools.R*, *1_FRAME_setup.R* and *2_FRAME_drug_identification.R*:
#' 
#' - Change the *pkg_path* in myomoptools.R to your local package directory and then run it (only need to do once).
#' 
#' - In *1_FRAME_setup.R* change the *db_path* to your MIREDA DuckDB file path and change file paths to your own myomoptools package if needed.
#' 
#' - Run both *1_FRAME_setup.R* and *2_FRAME_drug_identification.R*.
#' 
#-----------------------------------------------------------------------------
source(file.path(getwd(), "1_FRAME_setup.R"))
source(file.path(getwd(), "2_FRAME_drug_identification.R"))

#' 
#' #Define data type
#' 
#' Mothers eligible for the cohort are primarily defined by the prescription of either nifedipine or labetalol. Primary and secondary care data require different approaches for the reasons stated in the introduction. These are defined by the *rxtype* below.
#' 
#' - *rxtype* = 1L if prescribing data comes from GP record
#' - *rxtype* = 2L if prescribing data comes from secondary care (or both - use secondary care as this should be where the prescribing originates).
#' 
#-----------------------------------------------------------------------------
rxtype <- 1L

#' 
#' #Find mothers with nifedipine/labetalol prescriptions
#' 
#' The list of concept codes, *frame_drugs*, for the study drugs were created by the *2_FRAME_drug_identification.R* file. This is used to identify mothers with events matching these concept codes.
#' 
#-----------------------------------------------------------------------------
frame_drugs_events <- cdm$drug_exposure %>%
  inner_join(cdm$frame_drugs %>%
               mutate(drug_concept_id = as.integer(drug_concept_id)),
             by = "drug_concept_id") %>%
  compute(., name = "frame_drugs_events", temporary = TRUE, overwrite = TRUE)

cdm[["frame_drugs_events"]] <- tbl(con, "frame_drugs_events")

#' 
#' #Identify conditions, observations, measurements
#' 
#' Get the details from OMOP domains in this section and return relevant codes. There is nothing useful in the procedures section to use here.
#' 
#-----------------------------------------------------------------------------
#Conditions concepts
pregconcept <- myomoptools::find_concepts(
  cdm,
  keyword = "^Pregnancy$",
  domain = "Condition",
  vocab_id = "SNOMED",
  standard_only = TRUE) %>%
  distinct(concept_id)

Diabetes in pregnancy
4058243 = Diabetes mellitus during pregnancy, childbirth and the puerperium
diabpreg <- myomoptools::get_descendants(cdm, 4058243L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 4058243L) %>%
              select(concept_id))

Other diabetes
201820 = Diabetes mellitus
diabetes <- myomoptools::get_descendants(cdm, 201820L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 201820L) %>%
              select(concept_id)) %>%
  anti_join(diabpreg)

Unfiltered - all respiratory disorders
respiratory <- myomoptools::get_descendants(cdm, 317009) %>%
  union_all(myomoptools::get_descendants(cdm, 42873168)) %>%
  union_all(myomoptools::get_descendants(cdm, 4063381)) %>%
  union_all(myomoptools::get_descendants(cdm, 974952)) %>%
  union_all(myomoptools::get_descendants(cdm, 1340232)) %>%
  union_all(myomoptools::get_descendants(cdm, 4224259)) %>%
  union_all(myomoptools::get_descendants(cdm, 42539027)) %>%
  union_all(myomoptools::get_descendants(cdm, 313459)) %>%
  union_all(myomoptools::get_descendants(cdm, 4044240)) %>%
  union_all(myomoptools::get_descendants(cdm, 43531585)) %>%
  union_all(myomoptools::get_descendants(cdm, 37110295)) %>%
  union_all(myomoptools::get_descendants(cdm, 40483220)) %>%
  union_all(myomoptools::get_descendants(cdm, 4247108)) %>%
  union_all(myomoptools::get_descendants(cdm, 317971)) %>%
  union_all(myomoptools::get_descendants(cdm, 4005302)) %>%
  union_all(myomoptools::get_descendants(cdm, 196724)) %>%
  union_all(myomoptools::get_descendants(cdm, 37395588)) %>%
  union_all(myomoptools::get_descendants(cdm, 4049972)) %>%
  union_all(myomoptools::get_descendants(cdm, 4028286)) %>%
  union_all(myomoptools::get_descendants(cdm, 4060429)) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(
                concept_id %in% c(317009,   42873168, 4063381, 974952,  1340232,
                                  4224259,  42539027, 313459,  4044240, 43531585,
                                  37110295, 40483220, 4247108, 317971,  4005302,
                                  196724,   37395588, 4049972, 4028286, 4060429)) %>%
              select(concept_id))

All hypertension
316866 = Hypertensive disorder
hypertension <- myomoptools::get_descendants(cdm, 316866L) %>%
  union_all(myomoptools::get_descendants(cdm, 4086587L)) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id %in% c(316866L, 4086587L)) %>%
              select(concept_id))

pre-eclampsia
439393 = Pre-eclampsia
preeclampsia <- myomoptools::get_descendants(cdm, 439393L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 439393L) %>%
              select(concept_id))

eclampsia
443700
eclampsia <- myomoptools::get_descendants(cdm, 443700L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 443700L) %>%
              select(concept_id))

Other cardiac conditions except hypertension
134057 = Disorder of cardiovascular system
cvd <- myomoptools::get_descendants(cdm, 134057L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 134057L) %>%
              select(concept_id)) %>%
  Remove hypertension codes from here
  anti_join(hypertension)

raynauds <- cdm$concept %>%
  filter(concept_id %in% c(314962L, 1340453L)) %>%
  select(concept_id) Raynaud's & Exacerbation of Raynaud's

prem_labor <- myomoptools::get_descendants(cdm, 4273560L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 4273560L) %>%
              select(concept_id))

false_labor <- cdm$concept %>%
  filter(concept_id %in% c(45757113L, 139888L)) %>%
  select(concept_id)

cervical_incomp <- myomoptools::get_descendants(cdm, 436740L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 436740L) %>%
              select(concept_id))

pprom <- myomoptools::get_descendants(cdm, 4208215L) %>%
  select(concept_id = descendant_concept_id) %>%
  union_all(cdm$concept %>%
              filter(concept_id == 4208215L) %>%
              select(concept_id))

tocolytic <- prem_labor %>%
  union_all(false_labor) %>%
  union_all(cervical_incomp) %>%
  union_all(pprom)
################################################################################
#Cleanup conditions
conditions <- list(pregconcept, diabpreg, diabetes, respiratory, hypertension,
                   preeclampsia, eclampsia, cvd, raynauds, tocolytic)
names(conditions) <- c("pregconcept", "diabpreg", "diabetes",
                       "respiratory", "hypertension", "preeclampsia",
                       "eclampsia", "cvd", "raynauds", "tocolytic")

rm(pregconcept, diabpreg, diabetes, respiratory, hypertension, preeclampsia,
   eclampsia, cvd, raynauds, tocolytic, prem_labor, false_labor, pprom,
   cervical_incomp)
gc()
################################################################################
Observations

Index of multiple deprivation observation codes by country
imd           <-  cdm$concept %>%  Wales     Scotland  England
  filter(concept_id %in% c(35812898L, 35812898L, 35812898L)) %>%
  select(concept_id)

ambulatory_bp <- cdm$concept %>% filter(
  concept_id %in% c(1450301L, 44789316L, 44789315L, 1076803L)) %>%
  select(concept_id)

monr_24h      <- cdm$concept %>% filter(concept_id == 4015943L) %>% select(concept_id)
htn_monr      <- cdm$concept %>% filter(concept_id %in% c(4152829L, 4171421L)) %>% select(concept_id)
hx_of_event   <- cdm$concept %>%
  filter(concept_id == 1340204L) %>%
  select(concept_id)
gd_htn_ctrl   <- cdm$concept %>% filter(concept_id == 4017170L) %>% select(concept_id)
mod_htn_ctrl  <- cdm$concept %>% filter(concept_id == 4269035L) %>% select(concept_id)
poor_htn_ctrl <- cdm$concept %>% filter(concept_id == 4016922L) %>% select(concept_id)
tx_htn_strt   <- cdm$concept %>% filter(concept_id == 4015732L) %>% select(concept_id)
tx_htn_stop   <- cdm$concept %>% filter(concept_id == 4015734L) %>% select(concept_id)
on_tx_htn     <- cdm$concept %>% filter(concept_id == 4201850L) %>% select(concept_id)
ref_htn_clin  <- cdm$concept %>% filter(concept_id == 4085017L) %>% select(concept_id)
seen_htn_clin <- cdm$concept %>% filter(concept_id == 4083426L) %>% select(concept_id)
htn_tx_chgd   <- cdm$concept %>% filter(concept_id == 4015733L) %>% select(concept_id)
htn_fu_dflt   <- cdm$concept %>% filter(concept_id == 4015939L) %>% select(concept_id)
################################################################################
#Cleanup observations
observations <- list(monr_24h, htn_monr, hx_of_event, gd_htn_ctrl,
                     mod_htn_ctrl, poor_htn_ctrl, tx_htn_strt, tx_htn_stop,
                     on_tx_htn, ref_htn_clin, seen_htn_clin, htn_tx_chgd,
                     htn_fu_dflt)
names(observations) <- c("monr_24h", "htn_monr", "hx_of_event",
                         "gd_htn_ctrl", "mod_htn_ctrl", "poor_htn_ctrl",
                         "tx_htn_strt", "tx_htn_stop", "on_tx_htn",
                         "ref_htn_clin", "seen_htn_clin", "htn_tx_chgd",
                         "htn_fu_dflt")

rm(monr_24h, htn_monr, hx_of_event, gd_htn_ctrl, mod_htn_ctrl,
   poor_htn_ctrl, tx_htn_strt, tx_htn_stop, on_tx_htn, ref_htn_clin,
   seen_htn_clin, htn_tx_chgd, htn_fu_dflt)
gc()
############################################################################

#' 
#' #Create exclusions table
#' 
#' Create a an empty table to populate exclusions as filters are applied in the below code.
#' 
#-----------------------------------------------------------------------------
exclusions <- tibble(
  exclusion    = character(),
  mothers_left = integer(),
  preg_left    = integer(),
  stringsAsFactors = FALSE
)

#' 
#' #Begin cohort filtering
#' 
#-----------------------------------------------------------------------------
rx_in_preg <- cdm$condition_occurrence %>%
  get only condition_concept_ids for pregnancy
  semi_join(conditions$pregconcept,
            by = c("condition_concept_id" = "concept_id")) %>%
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
  filter mothers who have prescriptions from FRAME drugs only
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
  filter to keep only prescriptions 90 days preconception to 6wk post
  mutate(precon  = as.Date(condition_start_date - days(90)),
         postnat = as.Date(condition_end_date   + days(42))) %>%
  filter(between(drug_exposure_start_date, precon, postnat)) %>%
  {
    prepost <- data.frame(
      summarise(
        .,
        exclusions   = "3. Rx 90 days pre-pregnancy to 42 days post",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id)
      )
    )
    exclusions <<- rbind(exclusions, prepost)
    .
  }

rm(frame_drugs_events)
gc()

#' 
#' #Common factors required for next 2 approaches
#' 
#-----------------------------------------------------------------------------
GP codes for events from conditions
Isolate patient cohorts (mothers) and extract history concept
cohort_patients <- rx_in_preg %>% distinct(person_id) %>%
  compute(name = "tmp_cohort_patients", temporary = TRUE, overwrite = TRUE)
hx_concepts     <- observations$hx_of_event %>% select(concept_id) %>%
  compute(name = "tmp_hx_concepts", temporary = TRUE, overwrite = TRUE)

Pre-filter CDM tables to cohort patients first
cond_filtered <- cdm$condition_occurrence %>%
  semi_join(cohort_patients, by = "person_id")
obs_filtered  <- cdm$observation %>%
  semi_join(cohort_patients, by = "person_id")
meas_filtered <- cdm$measurement %>%
  semi_join(cohort_patients, by = "person_id")

get active concept dictionaries
cond_concepts <- conditions[names(conditions) != "pregconcept"] %>%
  keep(~ inherits(.x, "tbl")) %>%
  imap(~ .x %>%
         select(concept_id) %>%
         mutate(cncpt_nm = .y, cncpt_dom = "cond")) %>%
  reduce(union_all) %>%
  distinct()

obs_concepts <- observations %>%
  keep(~ inherits(.x, "tbl")) %>%
  imap(~ .x %>%
         select(concept_id) %>%
         mutate(cncpt_nm = .y, cncpt_dom = "obs")) %>%
  reduce(union_all) %>%
  distinct()

meas_concepts <- ambulatory_bp %>%
  select(concept_id) %>%
  mutate(cncpt_nm = "ambulatory_bp", cncpt_dom = "meas") %>%
  distinct()

Domain 1: conditions
cond_events <- cond_filtered %>%
  select(person_id, cncpt_id = condition_concept_id,
         cncpt_dt = condition_start_date, condition_source_value) %>%
  inner_join(cond_concepts, by = c("cncpt_id" = "concept_id")) %>%
  select(person_id, cncpt_id, cncpt_dt, cncpt_nm, cncpt_dom,
         condition_source_value) %>%
  compute(name = "tmp_cond_events", temporary = TRUE, overwrite = TRUE)

Domain 2: Observations
obs_events <- obs_filtered %>%
  anti_join(hx_concepts, by = c("observation_concept_id" = "concept_id")) %>%
  select(person_id, cncpt_id = observation_concept_id,
         cncpt_dt = observation_date) %>%
  inner_join(obs_concepts, by = c("cncpt_id" = "concept_id")) %>%
  select(person_id, cncpt_id, cncpt_dt, cncpt_nm, cncpt_dom) %>%
  compute(name = "tmp_obs_events", temporary = TRUE, overwrite = TRUE)

Domain 3: Measurements
meas_events <- meas_filtered %>%
  select(person_id, cncpt_id = measurement_concept_id,
         cncpt_dt = measurement_date) %>%
  inner_join(meas_concepts, by = c("cncpt_id" = "concept_id")) %>%
  select(person_id, cncpt_id, cncpt_dt, cncpt_nm, cncpt_dom) %>%
  compute(name = "tmp_meas_events", temporary = TRUE, overwrite = TRUE)

Domain 4: Target read v2 Hx observations
obs_hx_events <- obs_filtered %>%
  filter(observation_source_value %in%
           c("14A2.00", "15A3.00", "15A4.00", "15AB.00")) %>%
  semi_join(hx_concepts,
            by = c("observation_concept_id" = "concept_id")) %>%
  select(person_id, cncpt_id = observation_concept_id,
         cncpt_dt = observation_date,
         src_val  = observation_source_value) %>%
  inner_join(obs_concepts, by = c("cncpt_id" = "concept_id")) %>%
  mutate(cncpt_nm = case_when(
    src_val == "14A2.00" ~ "hx_htn_hx",
    src_val == "15A3.00" ~ "hx_eclmps_hx",
    src_val %in% c("15A4.00", "15AB.00") ~ "hx_preec_hx")) %>%
  select(person_id, cncpt_id, cncpt_dt, cncpt_nm, cncpt_dom) %>%
  compute(name = "tmp_hx_events", temporary = TRUE, overwrite = TRUE)

Union temp tables
target_events <- cond_events %>%
  union_all(obs_events) %>%
  union_all(meas_events) %>%
  union_all(obs_hx_events) %>%
  compute(name = "tmp_target_events", temporary = TRUE, overwrite = TRUE)

dbRemoveTable(con, "tmp_cond_events")
dbRemoveTable(con, "tmp_obs_events")
dbRemoveTable(con, "tmp_meas_events")
dbRemoveTable(con, "tmp_hx_events")
dbRemoveTable(con, "tmp_hx_concepts")
dbRemoveTable(con, "tmp_cohort_patients")

rm(hx_concepts, meas_events, meas_filtered, obs_concepts, obs_events,
   obs_filtered, obs_hx_events, observations)
gc()

Merge
rx_in_preg %>%
  mutate(pre1yr  = sql("condition_start_date - INTERVAL '1 year'"),
         gest20w = condition_start_date + days(240)) %>%
  left_join(
    target_events,
    by = join_by(person_id,
                 between(y$cncpt_dt, x$pre1yr, x$postnat))
  ) %>%
  select(condition_occurrence_id, person_id, condition_start_date,
         condition_end_date, drug_exposure_start_date, ingredient_name,
         drug_concept_name, pre1yr, precon, gest20w, postnat, cncpt_id,
         cncpt_dt, cncpt_nm, cncpt_dom, condition_source_value) %>%
  distinct() %>%
  compute(., name = "temp1", temporary = TRUE, overwrite = TRUE)

cdm[["temp1"]] <- tbl(con, "temp1")

htn_events_list <- c('monr_24h', 'htn_monr', 'gd_htn_ctrl', 'mod_htn_ctrl',
                     'poor_htn_ctrl', 'tx_htn_strt', 'tx_htn_stop',
                     'on_tx_htn', 'ref_htn_clin', 'seen_htn_clin',
                     'htn_tx_chgd', 'htn_fu_dflt')
tx_list   <-  c('tx_htn_strt', 'on_tx_htn', 'htn_tx_chgd')

dbRemoveTable(con, "tmp_target_events")

rm(ambulatory_bp, cohort_patients, cond_concepts, cond_events,
   cond_filtered, meas_concepts, target_events)
gc()

#' 
#' #Establish dates where there is uncertainty
#' 
#' Where prescriptions come from GP records, they appear to be mostly dated post birth. Scenario 1 below attempts to isolate when the drug was most likely to have started based on diagnoses, observations and measurements during preconception, antenatal and postnatal. Where there are diagnoses relevant to the study from both primary and secondary care, these are also extrapolated based on other indicators in the data around them.
#' 
#' Scenario 2 includes hospital prescriptions so patients are easily identified by having had a prescription on a date within pregnancy. Conditions observation and measurements from secondary care only are more reliable here.
#' 
#-----------------------------------------------------------------------------
#Scenario 1 - if non-drug events are from both GP and hospital but drugsfrom gp data only

if (rxtype == 1L) {
  Per drug anchors and imputation grouped by ingredient.
  processed_cohort <- cdm$temp1 %>%
    Implement a character length for condition_source_values to distinguish
    between conditions in primary care (>= 6 will include Read but also SNOMED later) and secondary care (<=5 for ICD10/OPCS4). Will be used to decipher possible retrospective GP dx and actual hosp dx.
    mutate(
      src_len = nchar(condition_source_value),
      is_history = if_else(cncpt_nm %in% c("hx_htn_hx", "hx_preec_hx"), 1L,
                           0L),
      is_sec_acute = if_else(!is.na(src_len) &
                               src_len <= 5L & is_history == 0L, 1L, 0L)
    ) %>%
    group_by(person_id, condition_occurrence_id, ingredient_name) %>%
    summarise(
      Prescription Windows (Primary Care)
      frst_precon_rx = min(if_else(
        drug_exposure_start_date >= precon &
          drug_exposure_start_date < condition_start_date,
        drug_exposure_start_date, as.Date(NA)), na.rm = TRUE),
      frst_preg_rx   = min(if_else(
        drug_exposure_start_date >= condition_start_date &
          drug_exposure_start_date < condition_end_date,
        drug_exposure_start_date, as.Date(NA)), na.rm = TRUE),
      frst_post_rx   = min(if_else(
        drug_exposure_start_date >= condition_end_date &
          drug_exposure_start_date <= postnat,
        drug_exposure_start_date, as.Date(NA)), na.rm = TRUE),

      Secondary care acute anchors (TRUTH is ON the day)
      htn_sec_preg_dt   = min(if_else(
        cncpt_nm == "hypertension" & is_sec_acute == 1L &
          cncpt_dt >= condition_start_date &
          cncpt_dt <= condition_end_date, cncpt_dt, as.Date(NA)), na.rm =
          TRUE),
      preec_sec_preg_dt = min(if_else(
        cncpt_nm == "preeclampsia" & is_sec_acute == 1L &
          cncpt_dt >= condition_start_date &
          cncpt_dt <= condition_end_date, cncpt_dt, as.Date(NA)), na.rm =
          TRUE),

      Primary care & history anchors
      htn_dx_dt   = min(if_else(ingredient_name == "hypertension",
                                cncpt_dt, as.Date(NA)), na.rm = TRUE),
      htn_dx_pre  = min(if_else(
        cncpt_nm == "hypertension" & cncpt_dt < gest20w,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      htn_dx_gest = min(if_else(
        cncpt_nm == "hypertension" & cncpt_dt >= gest20w & cncpt_dt <=
          condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      htn_dx_post = min(if_else(
        cncpt_nm == "hypertension" & cncpt_dt >  condition_end_date &
          cncpt_dt <= postnat, cncpt_dt, as.Date(NA)), na.rm = TRUE),

      htn_events_dt = min(if_else(
        cncpt_nm %in% htn_events_list & cncpt_dt <= condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      htn_hx_dt     = min(if_else(
        cncpt_nm == "hx_htn_hx", cncpt_dt, as.Date(NA)), na.rm = TRUE),

      preec_dx_gest = min(if_else(
        cncpt_nm == "preeclampsia" & cncpt_dt >= gest20w &
          cncpt_dt <= condition_end_date, cncpt_dt, as.Date(NA)), na.rm =
          TRUE),
      preec_dx_post = min(if_else(
        cncpt_nm == "preeclampsia" &
          cncpt_dt > condition_end_date & cncpt_dt <= postnat, cncpt_dt,
        as.Date(NA)), na.rm = TRUE),
      preec_hx_dt   = min(if_else(
        cncpt_nm == "hx_preec_hx" & cncpt_dt >= gest20w,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),

      precon_tx_dt  = min(if_else(
        cncpt_nm %in% tx_list & cncpt_dt < condition_start_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      ant_tx_dt     = min(if_else(
        cncpt_nm %in% tx_list & cncpt_dt >= condition_start_date &
          cncpt_dt <= condition_end_date, cncpt_dt,
        as.Date(NA)), na.rm = TRUE),

      raynaud_dt = min(if_else(
        cncpt_nm == "raynauds" & cncpt_dt >= condition_start_date &
          cncpt_dt <=  condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      tocolytic_dt = min(if_else(
        cncpt_nm == "tocolytic" & cncpt_dt >= gest20w & cncpt_dt <=
          condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),

      gest20w_date  = min(gest20w, na.rm = TRUE),
      cond_start_dt = min(condition_start_date, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    compute(name = "tmp_summary_anchors", temporary = TRUE, overwrite = TRUE
    ) %>%

    mutate(
      Clinical indicators
      has_chronic_htn = if_else(!is.na(htn_dx_pre) |
                                  (!is.na(htn_hx_dt) &
                                     htn_hx_dt < gest20w_date), 1L, 0L),
      has_gest_htn    = if_else(!is.na(htn_sec_preg_dt) |
                                  !is.na(htn_dx_gest) |
                                  (!is.na(htn_events_dt)), 1L, 0L),
      has_post_htn    = if_else(!is.na(htn_dx_post), 1L, 0L),
      has_preec_ant   = if_else(!is.na(preec_sec_preg_dt) |
                                  !is.na(preec_dx_gest) |
                                  (!is.na(preec_hx_dt)), 1L, 0L),

      raynaud_symp    = if_else(!is.na(raynaud_dt), 1L, 0L),
      nif_toc         = if_else(!is.na(tocolytic_dt), 1L, 0L),

      Minimum primary care/history date calculations
      htn_all_min_dt   = pmin(htn_dx_dt, htn_events_dt, htn_hx_dt, na.rm =
                                TRUE),
      preec_all_min_dt = pmin(preec_dx_gest, preec_hx_dt, preec_dx_post,
                              na.rm = TRUE),

      Secondary care TRUTH
      frst_htn_dt     = if_else(!is.na(htn_sec_preg_dt), htn_sec_preg_dt,
                                htn_all_min_dt),
      frst_preec_dt   = if_else(!is.na(preec_sec_preg_dt),
                                preec_sec_preg_dt, preec_all_min_dt)
    ) %>%

    ID/Impute earliest likely prescription start date
    mutate(
      frst_preg_rx_dt = case_when(
        Direct Rx during pregnancy
        !is.na(frst_preg_rx) ~  pmin(frst_preg_rx, ant_tx_dt, na.rm = TRUE),
        Preconception continuation itput pregnancy/postnatal or with HTN
        !is.na(frst_precon_rx) &
          (!is.na(frst_post_rx) | !is.na(ant_tx_dt) |
             has_chronic_htn == 1L | has_gest_htn == 1L |
             has_post_htn == 1L | has_preec_ant == 1L) ~ cond_start_dt,
        Postnatal Rx recoup rules
        !is.na(frst_post_rx) & !is.na(ant_tx_dt) ~ ant_tx_dt,
        !is.na(frst_post_rx) & !is.na(preec_sec_preg_dt) ~ preec_sec_preg_dt,
        !is.na(frst_post_rx) & !is.na(htn_sec_preg_dt) ~ htn_sec_preg_dt,
        !is.na(frst_post_rx) & !is.na(preec_hx_dt) ~ pmin(preec_hx_dt, gest20w_date, na.rm = TRUE),
        !is.na(frst_post_rx) & (has_gest_htn == 1L | has_chronic_htn == 1L) ~ pmin(frst_htn_dt, gest20w_date, na.rm = TRUE),
        !is.na(frst_post_rx) & !is.na(preec_dx_post) & has_gest_htn == 0L ~ preec_dx_post,
        !is.na(frst_post_rx) & !is.na(htn_dx_post) & has_gest_htn == 0L ~ htn_dx_post,
        TRUE ~ as.Date(NA)
      )
    ) %>%

    Determine htn_type AT THE TIME of drug_start_date
    mutate(
      htn_type = case_when(
        is.na(frst_preg_rx_dt) ~ NA_character_,
        Chronic: drug start b4 gest20w OR known chronic htn b4 drug start
        frst_preg_rx_dt < gest20w_date | has_chronic_htn == 1L  ~ "chronic",
        Preeclampsia: preeclampsia dx/hx existed on/b4 drug start
        !is.na(frst_preec_dt) & frst_preec_dt <= frst_preg_rx_dt ~ "preeclampsia",
        Gestational: HTN exists at/after gest20w OR via postnat HTN discharge
        !is.na(frst_htn_dt) & frst_htn_dt >= gest20w_date ~ "gestational",
        has_gest_htn == 1L | has_post_htn == 1L ~ "gestational",
        Fallback for postnatal preeclampsia recoup
        !is.na(preec_hx_dt) | !is.na(preec_dx_post) ~ "preeclampsia",
        TRUE ~ "gestational"
      )
    ) %>%
    filter(!is.na(frst_preg_rx_dt)) %>%
    compute(name = "tmp_summary_flags", temporary = TRUE, overwrite = TRUE)

  frst_drug_isolated <-  processed_cohort %>%
    group_by(person_id, condition_occurrence_id) %>%
    mutate(
      min_overall_date = min(frst_preg_rx_dt, na.rm = TRUE),
      Count number of drugs sharing abs min start date
      same_day_count = sum(if_else(frst_preg_rx_dt == min_overall_date, 1L, 0L), na.rm = TRUE),

      has_lab_precon = max(if_else(ingredient_name == "labetalol" & !is.na(frst_precon_rx), 1L, 0L), na.rm = TRUE),
      has_nif_precon = max(if_else(ingredient_name == "nifedipine" & !is.na(frst_precon_rx), 1L, 0L), na.rm = TRUE),
      has_gen_precon = max(if_else(!is.na(precon_tx_dt), 1L, 0L), na.rm = TRUE),

      precon_drug = case_when(
        has_lab_precon == 1L & has_nif_precon == 1L ~ "labetalol_and_nifedipine",
        has_lab_precon == 1L ~ "labetalol",
        has_nif_precon == 1L ~ "nifedipine",
        has_gen_precon == 1L ~ "Other",
        TRUE ~ NA_character_
      )
    ) %>%
    Exclude pregnancies where both drugs appear to have started at the same time
    filter(same_day_count == 1L) %>%
    mutate(
      Secondary drug start date (earliest start date of new drug after min_overall_date)
      scnd_drug_strt_dt =
        min(if_else(frst_preg_rx_dt > min_overall_date,
                    frst_preg_rx_dt, as.Date(NA)), na.rm = TRUE)
    ) %>%
    Get secondary drug name
    mutate(
      scnd_drug = max(if_else(frst_preg_rx_dt == scnd_drug_strt_dt,
                              ingredient_name, NA_character_), na.rm = TRUE)
    ) %>%
    filter(frst_preg_rx_dt == min_overall_date) %>%
    rename(frst_drug = ingredient_name,
           frst_drug_strt_dt = frst_precon_rx) %>%
    select(person_id, condition_occurrence_id, precon_drug, frst_preg_rx_dt,
           frst_drug, frst_drug_strt_dt, scnd_drug, scnd_drug_strt_dt,
           htn_type, raynaud_symp, nif_toc) %>%
    compute(name = "tmp_first_drug", temporary = TRUE, overwrite = TRUE)

  Join back and apply final inclusion/exclusion rules
  cdm$temp1 %>%
    select(-c(drug_exposure_start_date, drug_concept_name)) %>%
    inner_join(
      frst_drug_isolated,
      by = c("person_id", "condition_occurrence_id")
    ) %>%
    {
      rxs <- data.frame(
        summarise(
          .,
          exclusions   = "4. No Rx in pregnancy",
          mothers_left = n_distinct(person_id),
          preg_left    = n_distinct(condition_occurrence_id)
        )
      )
      exclusions <<- rbind(exclusions, rxs)
      .
    } %>%
    Exclusions for non-hypertensive nifedipine indications
    filter(!(frst_drug == "nifedipine" & raynaud_symp == 1L & is.na(htn_type))) %>%
    {
      ray <- data.frame(
        summarise(
          .,
          exclusions   = "5. Raynaud's indications removed (nifedipine)",
          mothers_left = n_distinct(person_id),
          preg_left    = n_distinct(condition_occurrence_id)
        )
      )
      exclusions <<- rbind(exclusions, ray)
      .
    } %>%
    filter(!(frst_drug == "nifedipine" & nif_toc == 1L & is.na(htn_type))) %>%

    {
      tocs <- data.frame(
        summarise(
          .,
          exclusions   = "6. Tocolytic indications removed (nifedipine)",
          mothers_left = n_distinct(person_id),
          preg_left    = n_distinct(condition_occurrence_id)
        )
      )
      exclusions <<- rbind(exclusions, tocs)
      .
    } %>%
    select(-c(raynaud_symp, nif_toc, ingredient_name,
              condition_source_value)) %>%
    distinct() %>%
    compute(name = "temp2", temporary = TRUE, overwrite = TRUE)

  dbRemoveTable(con, "tmp_summary_anchors")
  dbRemoveTable(con, "tmp_summary_flags")
  dbRemoveTable(con, "tmp_first_drug")
  dbRemoveTable(con, "temp1")

  cdm[["temp2"]] <- tbl(con, "temp2")
  cdm[["temp1"]] <- NULL
}

#Secnario 2: all events are from seconcary care only
if (rxtype != 1L) {
  Per drug anchors and imputation grouped by ingredient.
  processed_cohort <- cdm$temp1 %>%
    group_by(person_id, condition_occurrence_id, ingredient_name) %>%
    summarise(
      Prescription windows
      frst_precon_rx = min(if_else(
        drug_exposure_start_date >= precon &
          drug_exposure_start_date < condition_start_date,
        drug_exposure_start_date, as.Date(NA)), na.rm = TRUE),
      frst_preg_rx   = min(if_else(
        drug_exposure_start_date >= condition_start_date &
          drug_exposure_start_date < condition_end_date,
        drug_exposure_start_date, as.Date(NA)), na.rm = TRUE),
      frst_post_rx   = min(if_else(
        drug_exposure_start_date >= condition_end_date &
          drug_exposure_start_date <= postnat,
        drug_exposure_start_date, as.Date(NA)), na.rm = TRUE),

      Secondary care acute anchors (TRUTH is ON the day)
      htn_dx_pre  = min(if_else(
        cncpt_nm == "hypertension" & cncpt_dt < gest20w,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      htn_dx_gest = min(if_else(
        cncpt_nm == "hypertension" & cncpt_dt >= gest20w & cncpt_dt <= condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      htn_events_dt = min(if_else(
        cncpt_nm %in% htn_events_list & cncpt_dt <= condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),

      precon_tx_dt  = min(if_else(
        cncpt_nm %in% tx_list & cncpt_dt < condition_start_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),

      preec_dx_dt = min(if_else(
        cncpt_nm == "preeclampsia" &
          cncpt_dt <= condition_end_date, cncpt_dt, as.Date(NA)), na.rm = TRUE),
      preec_tx_dt = min(if_else(
        cncpt_nm %in% tx_list &
          cncpt_dt < condition_start_date, cncpt_dt, as.Date(NA)), na.rm = TRUE),

      Primary care & history anchors
      htn_dx_dt   = min(if_else(ingredient_name == "hypertension",
                                cncpt_dt, as.Date(NA)), na.rm = TRUE),

      raynaud_dt = min(if_else(
        cncpt_nm == "raynauds" & cncpt_dt >= condition_start_date &
          cncpt_dt <=  condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),
      tocolytic_dt = min(if_else(
        cncpt_nm == "tocolytic" & cncpt_dt >= gest20w & cncpt_dt <= condition_end_date,
        cncpt_dt, as.Date(NA)), na.rm = TRUE),

      gest20w_date  = min(gest20w, na.rm = TRUE),
      cond_start_dt = min(condition_start_date, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    compute(name = "tmp_sec_anchors", temporary = TRUE, overwrite = TRUE) %>%

    mutate(
      Bounded start dare for pregnancy exposure
      frst_preg_rx_dt = case_when(
        !is.na(frst_preg_rx)   ~ frst_preg_rx,
        !is.na(frst_precon_rx) &
          (!is.na(frst_post_rx) | !is.na(htn_dx_pre) | !is.na(htn_dx_gest) |
             !is.na(htn_events_dt) | !is.na(preec_dx_dt)) ~ cond_start_dt,
        !is.na(frst_post_rx) & !is.na(htn_dx_gest) ~ preec_dx_dt,
        !is.na(frst_post_rx) & !is.na(htn_dx_gest) ~ gest20w_date,
        TRUE ~ as.Date(NA))) %>%

    Determin HTN subtype
    mutate(
      htn_type = case_when(
        is.na(frst_preg_rx_dt) ~ NA_character_,
        frst_preg_rx_dt < gest20w_date | !is.na(htn_dx_pre) ~ "chronic",
        !is.na(preec_dx_dt) & preec_dx_dt <= frst_preg_rx_dt ~ "preeclampsia",
        !is.na(htn_dx_gest) | !is.na(htn_events_dt) ~ "gestational",
        TRUE ~ "gestational"
      ),
      raynaud_symp = if_else(!is.na(raynaud_dt), 1L, 0L),
      nif_toc      = if_else(!is.na(tocolytic_dt), 1L, 0L)
    ) %>%
    filter(!is.na(frst_preg_rx_dt)) %>%
    compute(name = "tmp_sec_flags", temporary = TRUE, overwrite = TRUE)

  1st drug selection and seq isolation
  frst_drug_isolated <- processed_cohort %>%
    group_by(person_id, condition_occurrence_id) %>%
    mutate(
      min_overall_date = min(frst_preg_rx_dt, na.rm = TRUE),
      Count number of drugs sharing min start date
      same_day_count = sum(if_else(frst_preg_rx_dt == min_overall_date, 1L, 0L), na.rm = TRUE),

      has_lab_precon = max(if_else(ingredient_name == "labetalol" &  !is.n
                                   (frst_precon_rx), 1L, 0L), na.rm = TRUE),
      has_nif_precon = max(if_else(ingredient_name == "nifedipine" & !is.na(frst_precon_rx), 1L, 0L), na.rm = TRUE),
      has_gen_precon = max(if_else(!is.na(precon_tx_dt), 1L, 0L), na.rm = TRUE),

      precon_drug = case_when(
        has_lab_precon == 1L & has_nif_precon == 1L ~ "labetalol_and_nifedipine",
        has_lab_precon == 1L ~ "labetalol",
        has_nif_precon == 1L ~ "nifedipine",
        has_gen_precon == 1L ~ "Other",
        TRUE ~ NA_character_)
    ) %>%
    Exclude pregnancies where both drugs have started at the same time
    filter(same_day_count == 1L) %>%
    mutate(
      Sec drug start date (earliest new drug start date after min_overall_date)
      scnd_drug_strt_dt = min(if_else(frst_preg_rx_dt > min_overall_date,
                                      frst_preg_rx_dt, as.Date(NA)), na.rm = TRUE),
      scnd_drug         = max(if_else(frst_preg_rx_dt == scnd_drug_strt_dt,
                                      ingredient_name, NA_character_), na.rm = TRUE)
    ) %>%
    filter(frst_preg_rx_dt == min_overall_date) %>%
    mutate(frst_drug_strt_dt = if_else(!is.na(frst_precon_rx),
                                       frst_precon_rx, frst_preg_rx_dt)) %>%
    rename(frst_drug = ingredient_name) %>%
    select(person_id, condition_occurrence_id, precon_drug,
           frst_drug_strt_dt,
           frst_drug, frst_preg_rx_dt, scnd_drug, scnd_drug_strt_dt,
           htn_type, raynaud_symp, nif_toc) %>%
    compute(name = "tmp_sec_first_drug", temporary = TRUE, overwrite = TRUE)

  Join back and exclude non-htn indications
  cdm$temp1 %>%
    select(-c(drug_exposure_start_date, drug_concept_name)) %>%
    inner_join(
      frst_drug_isolated,
      by = c("person_id", "condition_occurrence_id")
    ) %>%
    {
      rxs <- data.frame(
        summarise(
          .,
          exclusions   = "4. No Rx in pregnancy",
          mothers_left = n_distinct(person_id),
          preg_left    = n_distinct(condition_occurrence_id)
        )
      )
      exclusions <<- rbind(exclusions, rxs)
      .
    } %>%
    Exclusions for non-hypertensive nifedipine indications
    filter(!(frst_drug == "nifedipine" & raynaud_symp == 1L & is.na(htn_type))) %>%
    {
      ray <- data.frame(
        summarise(
          .,
          exclusions   = "5. Raynaud's indications removed (nifedipine)",
          mothers_left = n_distinct(person_id),
          preg_left    = n_distinct(condition_occurrence_id)
        )
      )
      exclusions <<- rbind(exclusions, ray)
      .
    } %>%
    filter(!(frst_drug == "nifedipine" & nif_toc == 1L & is.na(htn_type))) %>%
    {
      tocs <- data.frame(
        summarise(
          .,
          exclusions   = "6. Tocolytic indications removed (nifedipine)",
          mothers_left = n_distinct(person_id),
          preg_left    = n_distinct(condition_occurrence_id)
        )
      )
      exclusions <<- rbind(exclusions, tocs)
      .
    } %>%
    select(-c(raynaud_symp, nif_toc, ingredient_name,
              condition_source_value)) %>%
    distinct() %>%
    compute(name = "temp2", temporary = TRUE, overwrite = TRUE)

  dbRemoveTable(con, "tmp_sec_anchors")
  dbRemoveTable(con, "tmp_sec_flags")
  dbRemoveTable(con, "tmp_sec_first_drug")
  dbRemoveTable(con, "temp1")

  cdm[["temp2"]] <- tbl(con, "temp2")
  cdm[["temp1"]] <- NULL
}

rm(frst_drug_isolated, processed_cohort, htn_events_list, rxtype, tx_list)
gc()

#' 
#' #Other parameters
#' 
#' Additional parameters required for matching and propensity scoring.
#' 
#-----------------------------------------------------------------------------
cdm$temp2 %>%
  group_by(person_id, condition_occurrence_id) %>%
  left_join(cdm$person %>%
              mutate(birth_datetime = as.Date(birth_datetime)) %>%
              select(person_id, birth_datetime)) %>%
  group_by(person_id, condition_occurrence_id) %>%
  mutate(
    age_frst_rx_precon = Maternal age at first preconception Rx
      sql("DATEDIFF('day', birth_datetime, frst_drug_strt_dt)")/365.25,
    age_frst_rx_preg = Maternal age at first Rx during pregnancy
      sql("DATEDIFF('day', birth_datetime, frst_preg_rx_dt)")/365.25) %>%
  Remove mothers who were not 18+ at first Rx during pregnancy
  filter(age_frst_rx_preg >= 18) %>%
  ungroup() %>%
  {
    df <- .

    age18plus <- df %>%
      filter(age_frst_rx_preg >= 18) %>%
      summarise(
        .,
        exclusions   = "7. 18+ years old",
        mothers_left = n_distinct(person_id),
        preg_left    = n_distinct(condition_occurrence_id),
        .groups = "drop"
      ) %>%
      collect()

    exclusions <<- rbind(exclusions, age18plus)
    df
  } %>%
  Get remaining counts of labetalol 1st pregnancy Rxs
  {
    labs <- data.frame(
      summarise(
        .,
        exclusions   = "8. Labetalol only",
        mothers_left = n_distinct(person_id[frst_drug == "labetalol"],
                                  na.rm = TRUE),
        preg_left    = n_distinct(
          condition_occurrence_id[frst_drug == "labetalol"], na.rm = TRUE),
        .groups = "drop"
      )
    )
    exclusions <<- rbind(exclusions, labs)
    .
  } %>%
  Get remaining counts of nifedipine 1st pregnancy Rxs
  {
    nifs <- data.frame(
      summarise(
        .,
        exclusions   = "9. Nifedipine only",
        mothers_left = n_distinct(person_id[frst_drug == "nifedipine"],
                                  na.rm = TRUE),
        preg_left    = n_distinct(
          condition_occurrence_id[frst_drug == "nifedipine"],
          na.rm = TRUE),
        .groups = "drop"
      )
    )
    exclusions <<- rbind(exclusions, nifs)
    .
  } %>%
  distinct(person_id, mat_birthdate = birth_datetime, condition_occurrence_id,
           condition_start_date, condition_end_date, b4_conception_1y_dt = pre1yr,
           preconception_strt_dt = precon, gest20w_dt = gest20w,
           postnatal_end_dt = postnat, cncpt_id, cncpt_nm, cncpt_dom, cncpt_dt,
           precon_drug, precon_drug_strt_dt = frst_drug_strt_dt, age_frst_rx_precon,
           frst_preg_drug = frst_drug, frst_preg_drug_strt_dt = frst_preg_rx_dt,
           age_frst_rx_preg, scnd_drug, scnd_drug_strt_dt, htn_type) %>%
  compute(name = "rx_in_preg", temporary = TRUE, overwrite = TRUE)

remove the numbered temp tables and add rx_in_preg to cdm.
  dbRemoveTable(con, "temp2")
  cdm[["temp2"]] <- NULL

  cdm[["rx_in_preg"]] <- tbl(con, "rx_in_preg")

Write exclusions to table and remove object
  writexl::write_xlsx(
    x = exclusions,
    path = file.path(getwd(), "Exclusions.xlsx")
    )

  rm(exclusions, rx_in_preg)
  gc()

#' 
#' #Tables for unmatched data
#' 
#' Add two tables to collect the details for the unmatched data table in the FRAME document.
#' 
#-----------------------------------------------------------------------------
Table for writing variable counts
deets <- data.frame(
  variable   = as.character(),
  cats       = as.character(),
  nifedipine = as.integer(),
  labetalol  = as.integer()
)

Table for recording gestational ages at first R in pregnancy
gestages <- data.frame(
  mean_ga_nifedipine = as.numeric(),
  sd_ga_nifedipine   = as.numeric(),
  mean_ga_labetalol  = as.numeric(),
  sd_ga_labetalol    = as.numeric()
)

#' 
#' #Finalise a cohort base table
#' 
#' Modifying the existing table by adding additional detail to use as a baseline table for the FRAME cohort.
#' 
#-----------------------------------------------------------------------------
Build baseline table parameters
base_table <- cdm$rx_in_preg %>%
  mutate(across(
    dplyr::all_of(
      names(select(collect(head(., 1)),
                   where(~ inherits(.x, "POSIXct") ||
                           inherits(.x, "dttm"))))), ~ as.Date(.x))) %>%
  Ethnicity
  left_join(cdm$observation %>%
              select(person_id, observation_concept_id) %>%
              filter(observation_concept_id %in%
                       White, Black, Asian, Mixed, Other
                       c(3959326L, 3959330L, 3959336L, 3959338L, 3959547L))) %>%
  mutate(ethnicity = case_when(
    observation_concept_id == 3959326L ~ "White",
    observation_concept_id == 3959330L ~ "Black",
    observation_concept_id == 3959336L ~ "Asian",
    observation_concept_id == 3959338L ~ "Mixed",
    observation_concept_id == 3959547L ~ "Other",
    TRUE ~ "Missing")) %>%
  select(-observation_concept_id) %>%
  group_by(ethnicity) %>%
  {
    eth <- distinct(., condition_occurrence_id, frst_preg_drug, ethnicity) %>%
      summarise(
        nifedipine = sum(if_else(frst_preg_drug == "nifedipine", 1L, 0L), na.rm = TRUE),
        labetalol  = sum(if_else(frst_preg_drug == "labetalol", 1L, 0L), na.rm = TRUE),
        .groups = "drop"
        ) %>%
      collect() %>%
      mutate(variables = "ethnicity",
             cats = factor(ethnicity,
                           levels = c("White", "Black", "Asian", "Mixed",
                                      "Other", "Missing"))) %>%
      select(variables, cats, nifedipine, labetalol) %>%
      arrange(cats)

    deets <<- rbind(deets, eth)
    .
  } %>%
  Singletons/Multiples - AT birth
  left_join(cdm$fact_relationship %>%
              filter(domain_concept_id_2 == 1147333L & condition_occurrence
                       domain_concept_id_1 == 1147314L & person
                                                    Infant,  Child
                       relationship_concept_id %in% c(4305451L, 4285883L)) %>%
              distinct(condition_occurrence_id = fact_id_2, fact_id_1) %>%
              group_by(condition_occurrence_id) %>%
              tally() %>%
              mutate(singleton = case_when(n == 1 ~ 1,
                                           TRUE ~ 0)) %>%
              select(condition_occurrence_id, singleton),
            by = "condition_occurrence_id") %>%
  group_by(singleton) %>%
  {
    sgl <- distinct(., condition_occurrence_id, frst_preg_drug, singleton) %>%
      summarise(
        nifedipine = sum(if_else(frst_preg_drug == "nifedipine", 1L, 0L), na.rm = TRUE),
        labetalol  = sum(if_else(frst_preg_drug == "labetalol",  1L, 0L), na.rm = TRUE),
        .groups = "drop"
        ) %>%
      collect() %>%
      mutate(variables = "birth number",
             cats = factor(singleton,
                           levels = c(1, 0),
                           labels = c("Singleton Birth", "Multiple birth"))) %>%
      select(variables, cats, nifedipine, labetalol) %>%
      arrange(cats)

    deets <<- rbind(deets, sgl)
    .
  } %>%
  Singletons/Multiples: Max number of fetuses in pregnancy
  left_join(cdm$measurement %>%
              filter(measurement_concept_id == 4077859L) %>% Number of fetuses
              distinct(person_id, measurement_date, value_as_number),
            by =join_by(
              person_id == person_id,
              between(y$measurement_date, x$condition_start_date,
                      x$condition_end_date))) %>%
  group_by(condition_occurrence_id) %>%
  filter out multiple birth counts, using the maximum count as early scans
  can miss multiples when when twins, etc are seen later.
  filter(value_as_number  == max(value_as_number, na.rm = TRUE) |
           in case there are births without a count...
           all(is.na(value_as_number))) %>%
  filter(measurement_date == min(measurement_date, na.rm = TRUE) |
           in case there are births without a count...
           all(is.na(value_as_number))) %>%
  mutate(num_fetus = case_when(value_as_number == 1 ~ "Singleton Pregnancy",
                               value_as_number  > 1 ~ "Multiple Pregnancy",
                               TRUE ~ NA_character_)) %>%
  select(-c(value_as_number, measurement_date)) %>%
  group_by(num_fetus) %>%
  {
    nfet <- distinct(., condition_occurrence_id, frst_preg_drug, num_fetus) %>%
      summarise(
        nifedipine = sum(if_else(frst_preg_drug == "nifedipine", 1L, 0L), na.rm = TRUE),
        labetalol  = sum(if_else(frst_preg_drug == "labetalol", 1L, 0L), na.rm = TRUE),
        .groups = "drop"
        ) %>%
      collect() %>%
      mutate(variables = "number of fetuses",
             cats = factor(num_fetus)) %>%
      select(variables, cats, nifedipine, labetalol) %>%
      arrange(desc(cats))

    deets <<- rbind(deets, nfet)
    .
  } %>%
  #Diabetes
  left_join(.,
            cdm$condition_occurrence %>%
              semi_join(
                dplyr::union(
                  conditions[["diabpreg"]] %>% select(concept_id),
                  conditions[["diabetes"]] %>% select(concept_id)),
                by = c("condition_concept_id" = "concept_id")) %>%
              select(person_id,
                     diab_id    = condition_concept_id,
                     diab_start = condition_start_date,
                     diab_end   = condition_end_date),
            by = "person_id") %>%
  group_by(person_id, condition_occurrence_id) %>%
  mutate(gest_diab = max(
    if_else(
      diab_id %in% !!(conditions$diabpreg %>% pull(concept_id)) &
        diab_start >= condition_start_date &
        diab_start <= condition_end_date,
      1L,
      0L
    ),
    na.rm = TRUE),
    gest_diab = if_else(is.na(gest_diab), 0L, gest_diab),
    preexist_diab = max(
      if_else(
        diab_id %in% !!(conditions$diabpreg %>% pull(concept_id)) &
          diab_start <= condition_end_date,
        1L,
        0L
      ),
      na.rm = TRUE),      
    preexist_diab = if_else(is.na(preexist_diab), 0L, preexist_diab),
    either_diab = max(
      if_else(
        gest_diab == 1L | preexist_diab == 1L,
        1L,
        0L
      ),
      na.rm = TRUE),
    either_diab = if_else(is.na(either_diab), 0L, either_diab)) %>%
  select(-c(diab_id, diab_start, diab_end)) %>%
  distinct() %>%
  group_by(either_diab) %>%
  {
    diab <- distinct(., condition_occurrence_id, frst_preg_drug, either_diab) %>%
      summarise(
        nifedipine = sum(if_else(frst_preg_drug == "nifedipine", 1L, 0L), na.rm = TRUE),
        labetalol  = sum(if_else(frst_preg_drug == "labetalol",  1L, 0L), na.rm = TRUE),
        .groups = "drop"
        ) %>%
      collect() %>%
      mutate(variables = "diabetes present",
             cats = factor(either_diab,
                           levels = c(0, 1),
                           labels = c("no diabetes", "diabetes present"))) %>%
      select(variables, cats, nifedipine, labetalol) %>%
      filter(cats == "diabetes present")

      deets <<- rbind(deets, diab)
    .
  } %>%
  left_join(cdm$observation %>%
              semi_join(imd,
                        by = c("observation_concept_id" = "concept_id")) %>%
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
    deprv <- distinct(., condition_occurrence_id, frst_preg_drug, imd) %>%
      summarise(
        nifedipine = sum(if_else(frst_preg_drug == "nifedipine", 1L, 0L), na.rm = TRUE),
        labetalol  = sum(if_else(frst_preg_drug == "labetalol", 1L, 0L),  na.rm = TRUE),
        .groups = "drop"
        ) %>%
      collect() %>%
      mutate(variables = "deprivation quintile",
             cats = factor(imd,
                           levels = c("Q1-2", "Q3-5", "Missing"))) %>%
      select(variables, cats, nifedipine, labetalol) %>%
      arrange(cats)

    deets <<- rbind(deets, deprv)
    .
  } %>%
  ungroup() %>%
  mutate(gestage_frst_rx = as.numeric(frst_preg_drug_strt_dt - condition_start_date)/7) %>%
  {
    g_age <- distinct(., condition_occurrence_id, frst_preg_drug, gestage_frst_rx) %>%
      summarise(
        mean_ga_nifedipine = mean(if_else(frst_preg_drug == "nifedipine", gestage_frst_rx,
                                          NA_real_), na.rm = TRUE),
        sd_ga_nifedipine   = sd(if_else(frst_preg_drug  == "nifedipine", gestage_frst_rx,
                                        NA_real_), na.rm = TRUE),
        mean_ga_labetalol  = mean(if_else(frst_preg_drug== "labetalol", gestage_frst_rx,
                                          NA_real_), na.rm = TRUE),
        sd_ga_labetalol    = sd(if_else(frst_preg_drug  == "labetalol", gestage_frst_rx,
                                        NA_real_), na.rm = TRUE)
      ) %>%
        collect()

    gestages <<- rbind(gestages, g_age)
    .
  } %>%
  group_by(person_id, condition_occurrence_id) %>%
  mutate(
    gest_age_group = case_when(gestage_frst_rx < 11 ~ "0-10 weeks",
                                    gestage_frst_rx >= 11 &
                                      gestage_frst_rx < 20 ~ "11-19 weeks",
                                    gestage_frst_rx >= 20 &
                                      gestage_frst_rx < 28 ~ "20-27 weeks",
                                    gestage_frst_rx >= 28 &
                                      gestage_frst_rx < 35 ~ "28-34 weeks",
                                    TRUE ~ ">= 35 weeks"),
    respiratory = max(if_else(
      cncpt_nm == "respiratory" &
        between(cncpt_dt, b4_conception_1y_dt, condition_start_date), 1L, 0L),
      na.rm = TRUE),
    respiratory = if_else(is.na(respiratory), 0L, respiratory),
    cvd         = max(if_else(
      cncpt_nm == "respiratory" &
        between(cncpt_dt, b4_conception_1y_dt, condition_start_date), 1L, 0L),
      na.rm = TRUE),
    cvd = if_else(is.na(cvd), 0L, respiratory),
    inc_gpandas_periods = if_else(
      between(frst_preg_drug_strt_dt,
      as.Date("2021-06-08"), as.Date("2025-01-17")), 1L, 0L
    )) %>%
  select(-c(cncpt_nm, cncpt_id, cncpt_dom, cncpt_dt)) %>%
  distinct() %>%
  compute(name = "base_stats", temporary = TRUE, overwrite = TRUE)

cdm[["base_stats"]] <- tbl(con, "base_stats")

writexl::write_xlsx(
  x = list(
    "BaseCounts" = deets,
    "Gestation"  = gestages
  ),
  path = file.path(getwd(), "Unmatched Details.xlsx")
)

dbRemoveTable(con, "rx_in_preg")
rm(deets, gestages, base_table, conditions, imd)
gc()

#' 
#' #Create FRAME cohort DuckDB
#' 
#' The script below will removed fathers if they exist in the MIREDA data as they are not required for this study. It then creates a FRAME cohort DuckDB by duplicating all required tables that have been created in the MIREDA database and filtering them so that only cohort mothers' and babies' data is retained. Finally it will clean up the remaining objects in the R environment, remove FRAME-specific tables from the MIREDA cohort and close the MIREDA connection - leaving only the FRAME DuckDB connection and list of lazy tables within.
#' 
#-----------------------------------------------------------------------------
Account for fathers data and remove from FRAME tables, if present.
fathers <- cdm$fact_relationship %>%
  {
    if (nrow(collect(head(filter(., relationship_concept_id == 4283070L), 1))) > 0) {
      filter(.,
             domain_concept_id_1 == 1147314L &
               relationship_concept_id == 4283070L) %>%
        distinct(person_id = fact_id_1)
      } else {
        NULL
      }
  }

get record of mother-baby relationships
mumsbabies <- cdm$base_stats %>%
  distinct(person_id, condition_occurrence_id) %>%
  left_join(cdm$fact_relationship %>%
              filter(domain_concept_id_2 == 1147333L & condition_occurrence
                       domain_concept_id_1 == 1147314L & person
                                                     Child   Infant
                       relationship_concept_id %in% c(4285883L, 4305451L)) %>%
              select(baby_id = fact_id_1, condition_occurrence_id = fact_id_2),
            by = "condition_occurrence_id") %>%
  distinct()

Get singular list of all person_ids for extracting data from other tables.
cohortids <- mumsbabies %>%
  select(person_id) %>%
  union_all(mumsbabies %>%
              select(person_id = baby_id)) %>%
  distinct()

Create filtered fact_relationship table which only applies to cohort person_ids
filtrd_fr <- cdm$fact_relationship %>%
  remove fathers if present
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
  Keep all realtionships which do not include a person_id
  filter(domain_concept_id_1 != 1147314L, domain_concept_id_2 != 1147314L) %>%
  add fr person_ids in fact_id_1
  union_all(
    filtrd_fr %>%
      filter(domain_concept_id_1 == 1147314L, domain_concept_id_2 != 1147314L) %>%
      semi_join(cohortids %>% select(person_id),
                by = c("fact_id_1" = "person_id"))
  ) %>%
  add fr person_ids in fact_id_2
  union_all(
    filtrd_fr %>%
      filter(domain_concept_id_1 != 1147314L, domain_concept_id_2 == 1147314L) %>%
      semi_join(cohortids %>% select(person_id),
                by = c("fact_id_2" = "person_id"))
  ) %>%
  add where both are person_ids and are contained in the cohort
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

###############################################################################       #Attach target database for FRAME
target_db_path <- file.path(getwd(), "frm_cdm.duckdb")
src_con <- dbplyr::remote_con(cdm[[names(cdm)[1]]])

attached_dbs <- dbGetQuery(src_con, "PRAGMA database_list;")$name
if ("frame_db" %in% attached_dbs) {
  dbExecute(src_con, "DETACH frame_db")
}
rm(attached_dbs)

dbExecute(src_con, sprintf("ATTACH '%s' AS frame_db;", target_db_path))

###############################################################################       Export CDM tables to target duckdb
write_to_frm <- function(lazy_tbl, table_name) {
  sql_str <- dbplyr::sql_render(lazy_tbl)
  dbExecute(src_con, sprintf("CREATE OR REPLACE TABLE frame_db.%s AS %s;",
                             table_name, sql_str))
}

1. Athena vocabulary
vocab_tables <- c("concept", "vocabulary", "domain", "concept_class",
                  "concept_relationship", "concept_synonym", "concept_ancestor",
                  "drug_strength", "relationship")

for (tbl in intersect(vocab_tables, names(cdm))) {
  write_to_frm(cdm[[tbl]], tbl)
}

2. Direct person-level cdm tables
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

3. Pre-filtered fact_relationship
if (exists("frame_fact_rel")) {
  write_to_frm(frame_fact_rel, "fact_relationship")
}

4. Relational/linked tables (only actions if both table pairs exist in cdm)
if ("episode_event" %in% names(cdm) && "episode" %in% names(cdm)) {
  filtered_episodes <- cdm$episode %>% semi_join(cohortids, by = "person_id")
  filtered_ep_events <- cdm$episode_event %>%
    semi_join(filtered_episodes %>% select(episode_id), by = "episode_id")
  write_to_frm(filtered_ep_events, "episode_event")
}

#Location stuff not used here as there is an issue with how they have been
#written but if you entity_ids are person_ids then below should run


    location will need changing if you have other locations too, these are
    just for historical address data for individuals
if ("location_history" %in% names(cdm)) {
  filtered_loc_hx <- cdm$location_history %>%
    filter(domain_id == "PERSON") %>%
    semi_join(cohortids, by = c("entity_id" = "person_id")) %>%
  write_to_frm(filtered_loc_hx, "location_history")
}
#
if ("location_history" %in% names(cdm) && "location" %in% names(cdm)) {
  filtered_loc_hx <- cdm$location_history %>%
    filter(domain_id == "PERSON") %>%
    semi_join(cohortids, by = c("entity_id" = "person_id"))
  filtered_loc <- cdm$location %>%
    semi_join(filtered_loc_hx %>% select(location_id), by = "location_id")
  write_to_frm(filtered_loc_hx, "location_history")
}

5. Previously created frame datasets
if ("frame_drugs" %in% names(cdm)) {
  write_to_frm(cdm$frame_drugs, "frame_drugs")
}
  reference key map for raw ids, pseudonymised, omoped and pregnancy ids
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

if ("frame_drugs_events" %in% names(cdm)) {
  write_to_frm(cdm$frame_drugs_events, "frame_drugs_events")
}

if ("base_stats" %in% names(cdm)) {
  write_to_frm(cdm$base_stats, "base_stats")
}

dbExecute(src_con, "DETACH frame_db;")

frm <- dbConnect(duckdb::duckdb(), dbdir = target_db_path,
                 config =
                   list("temp_directory" =
                          normalizePath(tempdir(), winslash = "/")))

Get a list of duckdb lazytables
duck_tables <- dbListTables(frm)

frame <- lapply(duck_tables, function(table_name) {
  tbl(frm, table_name)
})
names(frame) <- duck_tables

dbRemoveTable(con, "base_stats")
dbRemoveTable(con, "frame_drugs")
dbRemoveTable(con, "frame_drugs_events")

dbDisconnect(con, shutdown = TRUE)

rm(cdm, cohortids, con, duck_tables, fathers, filtered_ep_events,
   filtered_episodes, filtered_lazy, filtered_pregs, filtrd_fr, frame_fact_rel,
   mumsbabies, person_tables, src_con, tbl, vocab_tables, write_to_frm)
gc()
#
dbDisconnect(frm, shutdown = TRUE)
rm(frame, frm)
gc()

#' 
