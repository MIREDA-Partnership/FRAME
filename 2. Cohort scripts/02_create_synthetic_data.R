# =====================================================================
# OMOP CDM Synthetic Data Generation with Father Exclusion
# =====================================================================
## Run file to setup existing duckDB for MIREDA OMOP CDM
source("") # Insert file path here
## Load additional packages
lapply(c("rlang", "tibble", "synthpop", "tidyr"), library, character.only = TRUE)

options(dplyr.show_progress = FALSE)

# 1. PARAMETERS & PATHS
OUTPUT_DB_PATH <- "S:/0916 - Wales Electronic Cohort for Children (WECC)- Phase 4/Mike/synthetic_maternity_cdm.duckdb"
FATHER_RELATIONSHIP_ID <- 4283070
PERSON_CHUNK_SIZE <- 5000
SEED_VALUE <- 42
set.seed(SEED_VALUE)

cat("[STEP 1] Scanning existing database structure using 'con'...\n")
all_tables <- DBI::dbListTables(con)

vocabulary_tables <- c(
  "concept", "concept_ancestor", "concept_class", "concept_relationship", 
  "concept_synonym", "domain", "drug_strength", "relationship", "vocabulary"
)

# 2. CONDITIONAL FATHER EXCLUSION COHORT ENGINE
fact_rel_exists <- FALSE
if ("fact_relationship" %in% all_tables) {
  fact_rel_exists <- DBI::dbGetQuery(con, 
    "SELECT COUNT(*) as cnt FROM fact_relationship WHERE relationship_concept_id = ?",
    params = list(FATHER_RELATIONSHIP_ID)
  )$cnt > 0
}

DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_father_ids")
if (fact_rel_exists) {
  cat("  [INFO] Father records detected. Compiling isolation list...\n")
  DBI::dbExecute(con, "
    CREATE TEMPORARY TABLE temp_father_ids AS 
    SELECT DISTINCT fact_id_1 AS person_id 
    FROM fact_relationship 
    WHERE relationship_concept_id = 4283070
  ")
} else {
  cat("  [INFO] No father relationships found. Initializing empty exclusion context...\n")
  DBI::dbExecute(con, "CREATE TEMPORARY TABLE temp_father_ids (person_id BIGINT)")
}

# 3. COMPILING UNIQUE ANONYMIZED ID MAP (UPFRONT & INDEXED)
cat("[STEP 2] Building unique global person_id remapping table on 'con'...\n")
original_ids <- DBI::dbGetQuery(con, "
  SELECT person_id 
  FROM person 
  WHERE person_id NOT IN (SELECT person_id FROM temp_father_ids)
")$person_id

filtered_person_count <- length(original_ids)
synthetic_ids <- sample(10000000:99999999, size = filtered_person_count, replace = FALSE)

id_map_df <- data.frame(
  original_person_id = original_ids,
  synthetic_person_id = synthetic_ids
)

DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_person_id_map")
DBI::dbWriteTable(con, "temp_person_id_map", id_map_df, overwrite = TRUE)
DBI::dbExecute(con, "CREATE INDEX idx_m_orig ON temp_person_id_map(original_person_id)")

# Clear data frame from R environment to preserve memory
rm(id_map_df)
gc()

# 4. ATTACH DESTINATION DATABASE ENVIRONMENT TO CON
if (file.exists(OUTPUT_DB_PATH)) {
  file.remove(OUTPUT_DB_PATH)
}
DBI::dbExecute(con, sprintf("ATTACH '%s' AS out_db", OUTPUT_DB_PATH))

# 5. COPY STANDARDIZED VOCABULARY TABLES AS-IS
cat("[STEP 3] Migrating standard reference vocabulary data...\n")
for (v_table in vocabulary_tables) {
  if (v_table %in% all_tables) {
    cat(sprintf("  [COPY] Copying reference table: %s...\n", v_table))
    flush.console()
    DBI::dbExecute(con, sprintf("CREATE TABLE out_db.%s AS SELECT * FROM %s", v_table, v_table))
  }
}

# 6. FIXED CHUNKED SYNTHESIS ENGINE (1-TO-1 VECTOR ALIGNMENT)
cat("[STEP 4] Initiating chunked person profile synthesis...\n")
person_struct <- DBI::dbGetQuery(con, "SELECT * FROM person LIMIT 0")
DBI::dbExecute(con, "CREATE TABLE out_db.person AS SELECT * FROM person LIMIT 0")

num_chunks <- ceiling(filtered_person_count / PERSON_CHUNK_SIZE)

for (chunk_num in 1:num_chunks) {
  start_idx <- (chunk_num - 1) * PERSON_CHUNK_SIZE + 1
  end_idx <- min(chunk_num * PERSON_CHUNK_SIZE, filtered_person_count)
  
  cat(sprintf("  [SYNTH] Chunk %d/%d (Rows %d to %d)...\n", chunk_num, num_chunks, start_idx, end_idx))
  flush.console()
  
  chunk_orig_ids <- original_ids[start_idx:end_idx]
  chunk_synth_ids <- synthetic_ids[start_idx:end_idx]
  
  # Stream target data slice using con
  id_list_str <- paste(chunk_orig_ids, collapse = ", ")
  chunk_data <- DBI::dbGetQuery(con, sprintf("SELECT * FROM person WHERE person_id IN (%s)", id_list_str))
  
  # Drop technical ID attributes before modeling
  id_cols <- names(chunk_data)[grepl("_id$", names(chunk_data))]
  synth_input <- chunk_data[, !(names(chunk_data) %in% id_cols), drop = FALSE]
  
  # Execute synthpop simulation
  synth_obj <- synthpop::syn(synth_input, m = 1, method = "cart", print.flag = FALSE)
  synthetic_df <- synth_obj$syn
  
  # Map arrays directly via positional index sequence to ensure absolute uniqueness
  synthetic_df$person_id <- chunk_synth_ids
  
  # Restore structural non-person identifiers if present
  for (col in id_cols) {
    if (col != "person_id" && col %in% names(chunk_data)) {
      synthetic_df[[col]] <- chunk_data[[col]]
    }
  }
  
  # Align layout with target schema
  synthetic_df <- synthetic_df[, names(person_struct), drop = FALSE]
  
  # Write batch directly to output target via database engine boundaries
  DBI::dbWriteTable(con, "temp_person_chunk", synthetic_df, overwrite = TRUE)
  DBI::dbExecute(con, "INSERT INTO out_db.person SELECT * FROM temp_person_chunk")
  DBI::dbExecute(con, "DROP TABLE temp_person_chunk")
  
  rm(chunk_data, synth_input, synth_obj, synthetic_df)
  if (chunk_num %% 5 == 0) gc()
}

# 7. HIGH-SPEED IN-DATABASE CLINICAL TABLE STREAMING
cat("[STEP 5] Stream-mapping clinical domains directly inside database engine...\n")
clinical_tables <- setdiff(all_tables, c(vocabulary_tables, "person", "fact_relationship"))

for (table_name in clinical_tables) {
  cols <- DBI::dbListFields(con, table_name)
  
  if ("person_id" %in% cols) {
    cat(sprintf("  [STREAM] Processing table %s...\n", table_name))
    flush.console()
    
    pk_col <- paste0(table_name, "_id")
    select_parts <- c()
    
    for (col in cols) {
      if (col == "person_id") {
        select_parts <- c(select_parts, "m.synthetic_person_id AS person_id")
      } else if (col == pk_col) {
        random_base <- sample(10000000:50000000, 1)
        select_parts <- c(select_parts, sprintf("(%d + row_number() OVER ()) AS %s", random_base, pk_col))
      } else {
        select_parts <- c(select_parts, sprintf("t.%s", col))
      }
    }
    
    sql_query <- sprintf("
      CREATE TABLE out_db.%s AS 
      SELECT %s 
      FROM %s t
      JOIN temp_person_id_map m ON t.person_id = m.original_person_id
    ", table_name, paste(select_parts, collapse = ", "), table_name)
    
    DBI::dbExecute(con, sql_query)
  } else {
    cat(sprintf("  [SKIP/COPY] Table %s does not contain person_id, migrating as-is...\n", table_name))
    flush.console()
    DBI::dbExecute(con, sprintf("CREATE TABLE out_db.%s AS SELECT * FROM %s", table_name, table_name))
  }
}

# 8. PRESERVING RELEVANT FACT RELATIONSHIPS
if ("fact_relationship" %in% all_tables) {
  cat("  [STREAM] Filtering and remapping fact_relationship data...\n")
  flush.console()
  DBI::dbExecute(con, "
    CREATE TABLE out_db.fact_relationship AS 
    SELECT 
      CASE WHEN t.domain_concept_id_1 = 56 THEN COALESCE(m1.synthetic_person_id, t.fact_id_1) ELSE t.fact_id_1 END AS fact_id_1,
      CASE WHEN t.domain_concept_id_2 = 56 THEN COALESCE(m2.synthetic_person_id, t.fact_id_2) ELSE t.fact_id_2 END AS fact_id_2,
      t.domain_concept_id_1,
      t.domain_concept_id_2,
      t.relationship_concept_id
    FROM fact_relationship t
    LEFT JOIN temp_person_id_map m1 ON t.fact_id_1 = m1.original_person_id AND t.domain_concept_id_1 = 56
    LEFT JOIN temp_person_id_map m2 ON t.fact_id_2 = m2.original_person_id AND t.domain_concept_id_2 = 56
    WHERE t.relationship_concept_id != 4283070 OR t.relationship_concept_id IS NULL
  ")
}

# 9. DETACH FILE AND REMOVE ENVIRONMENT TEMPS
cat("[STEP 6] Detaching output file from 'con' and dropping session maps...\n")
DBI::dbExecute(con, "DETACH out_db")
DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_father_ids")
DBI::dbExecute(con, "DROP TABLE IF EXISTS temp_person_id_map")

# Clear residual vector objects to honor 16GB limit constraints
rm(original_ids, synthetic_ids, all_tables, clinical_tables, vocabulary_tables, num_chunks, filtered_person_count)
gc()

# 10. INITIALIZE SYN_CON FOR USER CHECK SCRIPTS
cat("[STEP 7] Initializing active 'syn_con' connection for validation checks...\n")
syn_con <- DBI::dbConnect(duckdb::duckdb(), OUTPUT_DB_PATH)

cat("\n[SUCCESS] Pipeline complete. 'syn_con' is open and ready for verification checks.\n")
