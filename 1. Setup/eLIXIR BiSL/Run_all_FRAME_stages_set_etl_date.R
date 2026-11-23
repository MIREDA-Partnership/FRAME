# set last date ETL pipeline ran
last_omop_etl_date <- "2026-11-17"

# set reporting_site_text
reporting_site_text <- "elixir_bisl"

# show working folder
getwd()
         
#set r path sub folder
bisl_r_sub_folder <-"1. Setup/eLIXIR BiSL/"

#run the scripts 
source(file.path(getwd(), paste0( bisl_r_sub_folder,"1_FRAME_setup.R")),echo=T)
source(file.path(getwd(), paste0( bisl_r_sub_folder,"2_FRAME_drug_identification.R")),echo=T)
source(file.path(getwd(), paste0( bisl_r_sub_folder,"3_BiSL_code_FRAME_cohort.r")),echo=T)
source(file.path(getwd(), paste0( bisl_r_sub_folder,"4_MatchingPropensity.R")),echo=T)
source(file.path(getwd(), paste0(bisl_r_sub_folder,"table_stats.R")),echo=T)

