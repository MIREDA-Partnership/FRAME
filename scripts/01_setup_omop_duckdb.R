# -----------------------------------------------------------------------------
# 0. Configuration - ONLY NEEDS SETTING UP ONCE (unless you change your data)
# -----------------------------------------------------------------------------
# Need to download the Athena vocab first and then unzip it.
# Highlight all vocabs to download

## Apply for a UMLS License: https://uts.nlm.nih.gov/uts
## Get the API key from your profile and then run the following against your 
## unzipped vocab file
## java -Dumls-apikey=YOUR_API_KEY -jar cpt4.jar 5

athena_vocab_dir <- r"(C:\Users\mseab\kDrive\Desktop\athena_vocab_0526)"
vocab_db_path    <- r"(C:\Users\mseab\omop_vocab.duckdb)" # where to write/open the DuckDB

# Set to TRUE the first time, or after a fresh Athena download
build_vocab_db   <- TRUE

# CPT4 activation
activate_cpt4    <- TRUE
umls_api_key     <- "567e598b-177d-4228-956a-5ad76bf4ef5a"   # e.g. "a1b2c3d4-1234-5678-abcd-ef0123456789"

# -----------------------------------------------------------------------------
# 1. Install packages if not already present, then load
# -----------------------------------------------------------------------------
pkgs <- c(
  "duckdb",            # local DuckDB engine
  "DBI",               # database interface
  "dplyr",             # data manipulation
  "dbplyr",            # dplyr SQL backend
  "tidyr",             # reshaping (useful for concept set work)
  "stringr",           # string helpers
  "purrr",             # functional helpers
  "CDMConnector",      # OMOP CDM connection layer
  "omopgenerics",      # S3 classes underpinning CDMConnector
  "CodelistGenerator"  # vocab search, ancestor/descendant traversal
)

new_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]

if (length(new_pkgs) > 0L) {
  message("Installing: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs)
} else {
  message("All packages already installed.")
}

invisible(lapply(pkgs, library, character.only = TRUE))
message("All libraries loaded.")

# -----------------------------------------------------------------------------
# 2. CPT4 activation (run once before building DB, if applicable)
# -----------------------------------------------------------------------------
# The Athena zip includes cpt4.jar. Running it patches concept descriptions
# directly into CONCEPT.csv. Do this BEFORE loading into DuckDB.
# Requires Java -- check with: system("java -version")
# UMLS API key: https://uts.nlm.nih.gov -> sign in -> My Profile

## Or just run command prompt in your unzipped vocab folder the following:
## java -Dumls-apikey=YOUR_API_KEY -jar cpt4.jar 5

if (activate_cpt4) {
  
  if (nchar(umls_api_key) == 0L) {
    stop("Set umls_api_key above. Find it at: https://uts.nlm.nih.gov -> My Profile")
  }
  
  jar_path <- file.path(athena_vocab_dir, "cpt4.jar")
  
  if (!file.exists(jar_path)) {
    stop(
      "cpt4.jar not found in: ", athena_vocab_dir,
      "\nEnsure you downloaded a vocab bundle with CPT4 selected."
    )
  }
  
  message("Running CPT4 activation -- may take several minutes...")
  
  exit_code <- system(
    paste0("java -jar ", jar_path, " ", umls_api_key),
    wait = TRUE
  )
  
  if (exit_code == 0L) {
    message("CPT4 activation complete. CONCEPT.csv has been updated.")
  } else {
    warning(
      "CPT4 jar exited with code ", exit_code,
      ". Check your UMLS API key and Java installation."
    )
  }
}

# -----------------------------------------------------------------------------
# 3. Build the vocab DuckDB from Athena TSVs (run once, ~5-15 mins)
# -----------------------------------------------------------------------------
# MedDRA: no activation needed -- if your Athena account has MedDRA access,
# MedDRA concepts are included directly in CONCEPT.csv and CONCEPT_RELATIONSHIP.csv.

vocab_tables <- c(
  "CONCEPT",
  "CONCEPT_RELATIONSHIP",
  "CONCEPT_ANCESTOR",
  "CONCEPT_SYNONYM",
  "CONCEPT_CLASS",
  "VOCABULARY",
  "DOMAIN",
  "RELATIONSHIP",
  "DRUG_STRENGTH"   # useful for drug vocab work; silently skipped if absent
)

if (build_vocab_db) {
  
  message("Building vocab DuckDB -- large tables (CONCEPT_RELATIONSHIP, ",
          "CONCEPT_ANCESTOR) will take a few minutes...")
  
  con_build <- dbConnect(duckdb(), vocab_db_path)
  
  walk(vocab_tables, function(tbl) {
    
    tsv_path <- file.path(athena_vocab_dir, paste0(tbl, ".csv"))
    
    if (!file.exists(tsv_path)) {
      message("  Skipping (not found): ", tbl)
      return(invisible(NULL))
    }
    
    message("  Loading: ", tbl)
    
    # DuckDB native reader is significantly faster than readr for large files
    dbExecute(
      con_build,
      paste0(
        "CREATE OR REPLACE TABLE ", tbl, " AS
         SELECT * FROM read_csv_auto(
           '", tsv_path, "',
           delim    = '\t',
           quote    = '',
           header   = TRUE,
           parallel = TRUE
         )"
      )
    )
  })
  
  # Fix date columns -- Athena stores dates as YYYYMMDD integers; cast to DATE
  date_cols <- list(
    CONCEPT              = c("valid_start_date", "valid_end_date"),
    CONCEPT_RELATIONSHIP = c("valid_start_date", "valid_end_date"),
    DRUG_STRENGTH        = c("valid_start_date", "valid_end_date")
  )
  
  message("Casting date columns from integer to DATE...")
  
  iwalk(date_cols, function(cols, tbl) {
    if (!tbl %in% dbListTables(con_build)) return(invisible(NULL))
    walk(cols, function(col) {
      dbExecute(
        con_build,
        paste0(
          "ALTER TABLE ", tbl,
          " ALTER COLUMN ", col,
          " TYPE DATE USING strptime(CAST(", col, " AS VARCHAR), '%Y%m%d')"
        )
      )
    })
  })
  
  # Fix flag columns -- RELATIONSHIP table ships is_hierarchical and
  # defines_ancestry as integers but CDMConnector expects character
  char_cols <- list(
    RELATIONSHIP = c("is_hierarchical", "defines_ancestry")
  )
  
  message("Casting flag columns from integer to VARCHAR...")
  
  iwalk(char_cols, function(cols, tbl) {
    if (!tbl %in% dbListTables(con_build)) return(invisible(NULL))
    walk(cols, function(col) {
      dbExecute(
        con_build,
        paste0(
          "ALTER TABLE ", tbl,
          " ALTER COLUMN ", col,
          " TYPE VARCHAR USING CAST(", col, " AS VARCHAR)"
        )
      )
    })
  })
  
  # Create empty stub tables for person and observation_period so that
  # cdmFromCon() can build a full CDM reference against vocab-only data
  message("Creating empty clinical stub tables...")
  
  dbExecute(con_build, "
    CREATE TABLE IF NOT EXISTS PERSON (
      person_id                   BIGINT,
      gender_concept_id           INTEGER,
      year_of_birth               INTEGER,
      month_of_birth              INTEGER,
      day_of_birth                INTEGER,
      birth_datetime              TIMESTAMP,
      race_concept_id             INTEGER,
      ethnicity_concept_id        INTEGER,
      location_id                 BIGINT,
      provider_id                 BIGINT,
      care_site_id                BIGINT,
      person_source_value         VARCHAR,
      gender_source_value         VARCHAR,
      gender_source_concept_id    INTEGER,
      race_source_value           VARCHAR,
      race_source_concept_id      INTEGER,
      ethnicity_source_value      VARCHAR,
      ethnicity_source_concept_id INTEGER
    )
  ")
  
  dbExecute(con_build, "
    CREATE TABLE IF NOT EXISTS OBSERVATION_PERIOD (
      observation_period_id         BIGINT,
      person_id                     BIGINT,
      observation_period_start_date DATE,
      observation_period_end_date   DATE,
      period_type_concept_id        INTEGER
    )
  ")
  
  # Indexes -- materially speeds up joins on the larger tables
  message("Creating indexes...")
  
  list(
    "CREATE INDEX IF NOT EXISTS idx_concept_id
       ON CONCEPT(concept_id)",
    "CREATE INDEX IF NOT EXISTS idx_concept_vocab
       ON CONCEPT(vocabulary_id)",
    "CREATE INDEX IF NOT EXISTS idx_cr_id1
       ON CONCEPT_RELATIONSHIP(concept_id_1)",
    "CREATE INDEX IF NOT EXISTS idx_cr_id2
       ON CONCEPT_RELATIONSHIP(concept_id_2)",
    "CREATE INDEX IF NOT EXISTS idx_cr_rel
       ON CONCEPT_RELATIONSHIP(relationship_id)",
    "CREATE INDEX IF NOT EXISTS idx_ca_ancestor
       ON CONCEPT_ANCESTOR(ancestor_concept_id)",
    "CREATE INDEX IF NOT EXISTS idx_ca_descendant
       ON CONCEPT_ANCESTOR(descendant_concept_id)"
  ) |>
    walk(~ dbExecute(con_build, .x))
  
  dbDisconnect(con_build, shutdown = TRUE)
  message("Vocab DuckDB ready: ", vocab_db_path)
}
  
# -----------------------------------------------------------------------------
# 4. Connect and create CDM reference (vocabulary-only, no patient data needed)
# -----------------------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = vocab_db_path))

cdm <- cdmFromCon(
  con          = con,
  cdmSchema    = "main",
  writeSchema  = "main",
  cdmName      = "omop_vocab"
)

message("CDM connected. Tables: ", paste(names(cdm), collapse = ", "))

rm(list = intersect(
  c("athena_vocab_dir", "build_vocab_db", "activate_cpt4", "umls_api_key",
    "pkgs", "new_pkgs", "vocab_tables", "date_cols", "char_cols", "con_build",
    "jar_path", "exit_code"),
  ls()))
################################################################################
