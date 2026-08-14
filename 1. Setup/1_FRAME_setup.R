################################################################################
## Check scripts on GitHub for installing duckdbtools if not already created  ##
################################################################################
## Check if myomoptools has been created
if (
  !dir.exists(file.path(getwd(), "Packages/myomoptools"))
) {
  source(file.path(getwd(), "Packages/3_create_myomoptools_pkg.R"))
}

req_packages <- c("DBI", "duckdb", "", "myomoptools")
invisible(suppressPackageStartupMessages(
  lapply(req_packages, library, character.only = TRUE, logical.return = FALSE)))
rm(req_packages)
################################################################################
db_path <- file.path(dirname(getwd()), "omop_cdm.duckdb")

con <- dbConnect(duckdb(), dbdir = db_path, 
                 config = 
                   list("temp_directory" = normalizePath(tempdir(), winslash = "/")))
################################################################################
# Get a list of duckdb lazytables
duck_tables <- dbListTables(con)

cdm <- lapply(duck_tables, function(table_name) {
  tbl(con, table_name)
})
names(cdm) <- duck_tables

rm(duck_tables, db_path)
################################################################################











