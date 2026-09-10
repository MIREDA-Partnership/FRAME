library(DBI)
library(duckdb)
library(tools)
library(readr)
library(data.table)

#==============================================================================
# Paths
#==============================================================================

# get current date
current_date <- Sys.Date()
current_date


# set the output folder
db_folder <- paste0("B:/BRC_Elixir/Durbaba- MIREDA/duckdb/omop cdm/",current_date,"/")
db_folder


# Check if the out folder exists, if not create
if (!dir.exists(db_folder)) {
  # Try to create the folder
  success <- dir.create(db_folder, recursive = TRUE, showWarnings = FALSE)
  
  if (success) {
    message("Folder created: ", db_folder)
  } else {
    warning("Failed to create folder: ", db_folder)
  }
} else {
  message("Folder already exists: ", db_folder)
}

# set the file name of the duckdb file
db_file_name <- paste0("elixir_bisl_omop_cdm_",current_date,".duckdb")
db_file_name

# create a full fiel name with path for the duckdb file
db_file <- paste0(db_folder,db_file_name)
db_file

# this is the folder with all OMOPed tsv file at the end for by python pipeline (I am realy a python user)
tsv_dir <- "B:/BRC_Elixir/Durbaba- MIREDA/output to carrot/All 2024 data/stage 10/"

# the folder with the Athena vocab 
# note all the cpt4 stuff is done at the folder level
vocab_dir <- "B:/BRC_Elixir/Durbaba- MIREDA/OMOP vocabulary/bundle OMOP vocabulary_download_v5_2026-05-21/"

# the folder with the OMOP CDM DDL
ddl_dir <- "B:/BRC_Elixir/Durbaba- MIREDA/OMOP_CDM_DDL/5.4/duckdb/"


# the name of the OMOP DDL file
ddl_sql_file <- "OMOPCDM_duckdb_5.4_ddl.sql"



# read in the text from the DDL file
ddl_sql_string <- read_file( paste0(ddl_dir,ddl_sql_file))

# ain't using differnt schemas so just removing all instances of @cdmDatabaseSchema.
use_ddl_sql_string <- gsub("@cdmDatabaseSchema.","",ddl_sql_string)

#eyeall
use_ddl_sql_string

#==============================================================================
# Create database directory if needed
#==============================================================================

#think this is a bit redundent by it doesn't hert to check
dir.create(dirname(db_file), recursive = TRUE, showWarnings = FALSE)

#==============================================================================
# Connect to DuckDB and creat tables from DDL
#==============================================================================

# connect to the duckdb file. If there is not a file with that name duckdb will create one
con <- dbConnect(
  duckdb(),
  dbdir = db_file
)


# execute DDL
dbExecute(con,use_ddl_sql_string)

# write some SQL to fix the data issues with the column numerator_value
fix_drug_strength_numerator_value_sql = " ALTER TABLE drug_strength
ALTER COLUMN numerator_value
SET DATA TYPE DECIMAL(38,10);
"

# execute the SQL to fix numerator_value
dbExecute(con,fix_drug_strength_numerator_value_sql)


# get list of tables fo
tables <- dbListTables(con)


# get a list of tsv file from the omop folder

tsv_files <- list.files(
  tsv_dir,
  pattern = "\\.tsv$",
  full.names = TRUE,
  ignore.case = FALSE
)




# get a list of csv files from the vocab folder

vocab_files <- list.files(
  vocab_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = FALSE
)



# combine the lists
all_files <- c(tsv_files,vocab_files)


# creat a loop to eyeball
for (f in all_files) {
  # get the table name form the file name
  table_name <- tolower(file_path_sans_ext(basename(f)))
  print(table_name)
}


# Function to convert columns to target type
# x is the orginal value
# target_type is the type from the schema
# returns x in the tyoe to match the schema
convert_column <- function(x, target_type) {
  
  # make target_type uppercase to checks
  target_type <- toupper(target_type)
  
  if (grepl("INT", target_type)) {
    return(as.integer(x))
  }
  
  if (grepl("DOUBLE|FLOAT|REAL|DECIMAL|NUMERIC", target_type)) {
    return(as.numeric(x))
  }
  
  if (target_type %in% c("VARCHAR", "TEXT", "STRING")) {
    return(as.character(x))
  }
  
  if (target_type %in% c("BOOLEAN", "BOOL")) {
    return(as.logical(x))
  }
  
  if (target_type == "DATE") {
    return(as.Date(x))
  }
  
  if (grepl("TIMESTAMP", target_type)) {
    return(as.POSIXct(x))
  }
  
  x
}



# loop through the files in the list

for (f in all_files) {
  
  # extract the tbale name in lowercase
  table_name <- tolower(file_path_sans_ext(basename(f)))
  
  print(table_name)

  # check if the table_name in the file list is in the tables in the schema
  if (table_name %in% tables) {


# Read data 
# note the vocab say it's csv but it is TSV
if (grepl("\\.tsv$",f))    { # reading tsv files
    df <- fread(
  f,
  sep = "\t",
  quote="",
  na.strings = c("", "NULL")
)    
  } 
    else { 
    df <- fread( # reading csv files by the are realy tsv file
      f,
      sep = "\t",
      na.strings = c("", "NULL")
    )
}
    
    
# need at template of the schema 
# sql to do this
    
template_sql_text <- "
SELECT
    column_name,
    data_type
FROM information_schema.columns
WHERE table_name = 'needed'
ORDER BY ordinal_position
"
# replace 'needed' with  the name of the table being worked on
use_sql_text <- gsub("needed",table_name,template_sql_text)

# Get target table schema
schema <- dbGetQuery(con, use_sql_text)


# Add missing columns and convert existing columns
#loop through the numner of columns in the schema
for (i in seq_len(nrow(schema))) {
  
  # get column name
  col_name <- schema$column_name[i]
  
  # get column type
  col_type <- schema$data_type[i]
  
  # add if in schema by missing from data
  if (!col_name %in% names(df)) {
    df[, (col_name) := NA]
    message("Added missing column: ", col_name)
  }
  
  
  # set the type of value of the data in the column using the schema cloumn type
  # uses the function to convert columns to target type
  set(
    df,
    j = col_name,
    value = convert_column(df[[col_name]], col_type)
  )
}

# Remove extra columns not in table schema
# find any extra columns in data
extra_cols <- setdiff(names(df), schema$column_name)

# if any extra columns loop and set to NULL to drop
if (length(extra_cols) > 0) {
  message(
    "Dropping extra columns: ",
    paste(extra_cols, collapse = ", ")
  )
  df[, (extra_cols) := NULL]
}

# Reorder columns to match schema table
setcolorder(df, schema$column_name)

# Append the checked and data type matched data from the dataframe into the duckdb databse
dbAppendTable(con, table_name, df)
  }
 
   
} # end loop of files

# Close connection
dbDisconnect(con, shutdown = TRUE)
