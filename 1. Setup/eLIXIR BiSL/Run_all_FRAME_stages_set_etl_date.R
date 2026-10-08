# set last date ETL pipeline ran
last_omop_etl_date <- "2026-09-22"

# set reporting_site_text
reporting_site_text <- "elixir_bisl"

#run the scripts 
source(file.path(getwd(), "1. Setup/1_FRAME_setup.R"),echo=T)
source(file.path(getwd(), "1. Setup/2_FRAME_drug_identification.R"),echo=T)
source(file.path(getwd(), "1. Setup/3_BiSL_code_FRAME_cohort.r"),echo=T)
source(file.path(getwd(), "1. Setup/4_MatchingPropensity.R"),echo=T)
source(file.path(getwd(), "1. Setup/table_stats.R"),echo=T)



