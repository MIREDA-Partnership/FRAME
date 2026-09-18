################################################################################
## Check scripts on GitHub for installing duckdbtools if not already created  ##
################################################################################
## Check if myomoptools has been created
# if (
#   !dir.exists(file.path(getwd(), "Packages/myomoptools"))
# ) {
#   source(file.path(getwd(), "Packages/3_create_myomoptools_pkg.R"))
# }


#req_packages <- c("DBI", "duckdb", "myomoptools")
req_packages <- c("DBI", "duckdb","dplyr")

invisible(suppressPackageStartupMessages(
  lapply(req_packages, library, character.only = TRUE, logical.return = FALSE)))
rm(req_packages)


# read in the functions using scource
source(file.path(getwd(), "1. Setup/eLIXIR BiSL/0_source_myomoptools.R"))
################################################################################

# last date of the python omop etl
last_omop_etl_date <- "2026-09-10"

# set the base OMOP folder and file
base_db_folder <- paste0("B:/BRC_Elixir/Durbaba- MIREDA/duckdb/omop cdm/",last_omop_etl_date)
base_db_file <- paste0("elixir_bisl_omop_cdm_",last_omop_etl_date,".duckdb")

base_db_path <- file.path(base_db_folder , base_db_file)

#eyeball
base_db_path

# set the OMOP folder and file for the FRAME project

frame_db_folder <- "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/FRAME OMOP/cdm"
frame_db_file <- paste0("frame_",base_db_file)

db_path <- file.path(frame_db_folder , frame_db_file)



#eyeball
db_path

# tables get added to the duckdb so create a copy of the base OMOP data if does not exist
if (!file.exists(db_path )){
file.copy(base_db_path, db_path, overwrite = TRUE )
}else{
  print('Frame OMOP file exists')
}


# create conection
con <- dbConnect(duckdb(), dbdir = db_path)

################################################################################
# Get a list of duckdb lazytables
duck_tables <- dbListTables(con)

#eyeball list
duck_tables

# reads each table inot the duckdb databse into a table
cdm <- lapply(duck_tables, function(table_name) {
  tbl(con, table_name)
})

# set the table names
names(cdm) <- duck_tables

#rm(duck_tables, db_path)
################################################################################







