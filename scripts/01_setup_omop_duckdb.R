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

# Convenience references so dplyr chains read cleanly
concept      <- cdm$concept
concept_rel  <- cdm$concept_relationship
concept_anc  <- cdm$concept_ancestor
concept_syn  <- cdm$concept_synonym

rm(list = intersect(
  c("athena_vocab_dir", "build_vocab_db", "activate_cpt4", "umls_api_key",
    "pkgs", "new_pkgs", "vocab_tables", "date_cols", "char_cols", "con_build",
    "jar_path", "exit_code"),
  ls()))
# =============================================================================
# QUERY PATTERNS (tidyverse/dplyr throughout)
# All functions return collected tibbles.
# =============================================================================


# -----------------------------------------------------------------------------
# A. Keyword search -- find standard concepts matching a term
# -----------------------------------------------------------------------------

find_concepts <- function(keyword,
                          domain        = NULL,
                          vocab_id      = NULL,
                          standard_only = TRUE) {
  
  q <- concept |>
    filter(str_detect(tolower(concept_name), tolower(keyword)))
  
  if (standard_only)  q <- q |> filter(standard_concept == "S")
  if (!is.null(domain))    q <- q |> filter(domain_id     == domain)
  if (!is.null(vocab_id))  q <- q |> filter(vocabulary_id == vocab_id)
  
  q |>
    select(concept_id, concept_name, domain_id, vocabulary_id,
           concept_class_id, concept_code, standard_concept,
           valid_end_date) |>
    collect()
}

# Examples
diabetes_concepts <- find_concepts("diabetes", domain = "Condition")
metformin_rxnorm  <- find_concepts("metformin", vocab_id = "RxNorm")

# -----------------------------------------------------------------------------
# B. Descendant concepts -- all children/grandchildren via CONCEPT_ANCESTOR
# -----------------------------------------------------------------------------
get_descendants <- function(root_concept_id,
                            min_sep = 1L,
                            max_sep = NULL) {
  
  q <- concept_anc |>
    filter(
      ancestor_concept_id      == root_concept_id,
      min_levels_of_separation >= min_sep
    )
  
  if (!is.null(max_sep)) {
    q <- q |> filter(min_levels_of_separation <= max_sep)
  }
  
  q |>
    inner_join(
      concept |> select(concept_id, concept_name, domain_id,
                        vocabulary_id, concept_code),
      by = c("descendant_concept_id" = "concept_id")
    ) |>
    select(
      descendant_concept_id,
      concept_name,
      domain_id,
      vocabulary_id,
      concept_code,
      min_levels_of_separation,
      max_levels_of_separation
    ) |>
    arrange(min_levels_of_separation, concept_name) |>
    collect()
}

# Example: all descendants of Type 2 diabetes mellitus
t2dm_descendants <- get_descendants(201826)


# -----------------------------------------------------------------------------
# C. Ancestor concepts -- walk UP the hierarchy
# -----------------------------------------------------------------------------
get_ancestors <- function(leaf_concept_id, max_sep = NULL) {
  
  q <- concept_anc |>
    filter(
      descendant_concept_id    == leaf_concept_id,
      min_levels_of_separation >= 1L
    )
  
  if (!is.null(max_sep)) {
    q <- q |> filter(min_levels_of_separation <= max_sep)
  }
  
  q |>
    inner_join(
      concept |> select(concept_id, concept_name, domain_id,
                        vocabulary_id, concept_code),
      by = c("ancestor_concept_id" = "concept_id")
    ) |>
    select(
      ancestor_concept_id,
      concept_name,
      domain_id,
      vocabulary_id,
      concept_code,
      min_levels_of_separation
    ) |>
    arrange(min_levels_of_separation, concept_name) |>
    collect()
}

# Example
t2dm_ancestors <- get_ancestors(201826, max_sep = 3L)

# -----------------------------------------------------------------------------
# D. Source code -> standard OMOP mapping
#    Supports Read v2 ("Read"), CTV3 ("CTV3"), ICD-10 ("ICD10"),
#    ICD-10-CM ("ICD10CM"), MedDRA ("MedDRA"), CPT4 ("CPT4"), etc.
# -----------------------------------------------------------------------------
map_source_to_standard <- function(source_vocab_id,
                                   keyword     = NULL,
                                   source_code = NULL) {
  
  src <- concept |>
    filter(vocabulary_id == source_vocab_id)
  
  if (!is.null(keyword)) {
    src <- src |>
      filter(str_detect(tolower(concept_name), tolower(keyword)))
  }
  
  if (!is.null(source_code)) {
    src <- src |> filter(concept_code == source_code)
  }
  
  src |>
    inner_join(
      concept_rel |> filter(relationship_id == "Maps to"),
      by = c("concept_id" = "concept_id_1")
    ) |>
    inner_join(
      concept |>
        select(
          standard_concept_id   = concept_id,
          standard_concept_name = concept_name,
          standard_domain       = domain_id,
          standard_vocab        = vocabulary_id,
          standard_code         = concept_code
        ),
      by = c("concept_id_2" = "standard_concept_id")
    ) |>
    select(
      source_concept_id   = concept_id,
      source_concept_name = concept_name,
      source_code         = concept_code,
      standard_concept_id = concept_id_2,
      standard_concept_name,
      standard_domain,
      standard_vocab,
      standard_code
    ) |>
    collect()
}

# Examples
read2_diabetes   <- map_source_to_standard("Read",   keyword = "diabetes")
icd10_diabetes   <- map_source_to_standard("ICD10",  keyword = "diabetes")
single_read_code <- map_source_to_standard("Read",   source_code = "C10E.00")

# -----------------------------------------------------------------------------
# E. Reverse lookup -- standard concept -> all source codes that map to it
# -----------------------------------------------------------------------------
get_source_codes <- function(standard_concept_id,
                             source_vocab_id = NULL) {
  
  q <- concept_rel |>
    filter(
      concept_id_2    == standard_concept_id,
      relationship_id == "Maps to"
    ) |>
    inner_join(
      concept |> select(concept_id, concept_name,
                        vocabulary_id, concept_code),
      by = c("concept_id_1" = "concept_id")
    )
  
  if (!is.null(source_vocab_id)) {
    q <- q |> filter(vocabulary_id == source_vocab_id)
  }
  
  q |>
    select(
      source_concept_id   = concept_id_1,
      source_concept_name = concept_name,
      source_vocab        = vocabulary_id,
      source_code         = concept_code
    ) |>
    arrange(source_vocab, source_code) |>
    collect()
}

# Examples
t2dm_all_sources  <- get_source_codes(201826)
t2dm_read_only    <- get_source_codes(201826, source_vocab_id = "Read")

# -----------------------------------------------------------------------------
# F. Concept synonyms -- all alternative names for a concept
# -----------------------------------------------------------------------------
get_synonyms <- function(concept_ids) {
  concept_syn |>
    filter(concept_id %in% concept_ids) |>
    inner_join(
      concept |> select(concept_id, concept_name, vocabulary_id),
      by = "concept_id"
    ) |>
    select(concept_id, concept_name, concept_synonym_name,
           language_concept_id) |>
    arrange(concept_id, concept_synonym_name) |>
    collect()
}

t2dm_synonyms <- get_synonyms(201826)

# -----------------------------------------------------------------------------
# G. Vocabulary inventory -- versions and concept counts
# -----------------------------------------------------------------------------
vocab_inventory <- cdm$vocabulary |>
  select(vocabulary_id, vocabulary_name, vocabulary_version,
         vocabulary_reference) |>
  arrange(vocabulary_id) |>
  collect()

concept_counts <- concept |>
  count(vocabulary_id, standard_concept, name = "n_concepts") |>
  arrange(desc(n_concepts)) |>
  collect()

# -----------------------------------------------------------------------------
# H. CodelistGenerator -- keyword search with auto-descendant expansion
#    (phenotyping-ready output)
# -----------------------------------------------------------------------------
diabetes_codelist <- getCandidateCodes(
  cdm      = cdm,
  keywords = "diabetes",
  domains  = "Condition"
)

# Get details (concept names, domains, vocab etc.) for the codelist
deets <- diabetes_codelist |>
  asCodelistWithDetails(cdm = cdm)

View(deets[[1]])

# -----------------------------------------------------------------------------
# Tidy up
# -----------------------------------------------------------------------------
# 1. Get the names of all functions in the Global Environment
func_names <- lsf.str()

# 2. Create a named list containing those function objects
my_functions <- mget(func_names)

rm(list = func_names)

# Test one of your functions from the list
# If your function was named 'my_plot', call it like this:
my_functions$my_plot()
# dbDisconnect(con, shutdown = TRUE)  # uncomment at end of session
