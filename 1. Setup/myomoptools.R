############################################################################################
##                 Create a local package for OMOP functions in Duckdb                    ##
############################################################################################
## Ensure you have Rtools installed on your system before you start
# Check for and load required packages for creating this local package
pkg_path = file.path(
  "P:",
  Sys.getenv("USERNAME"),
  "Projects/duckdbtools"
  )

if (!dir.exists(pkg_path)) {
  dir.create(pkg_path, recursive = TRUE, showWarnings = FALSE)
  }
# Create subdirectory for R scripts for each function to be saved in
dir.create(file.path(pkg_path, "R"), showWarnings = FALSE)

############################################################################################
##                  Start creating scripts to write the R functions                       ##
############################################################################################
# ------------------------------------------------------------------------------------------
# A. Keyword search -- find standard concepts matching a term
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/find_concepts.R"),
  text = '
#\' Search for concepts using keywords
#\' 
#\' Look for concepts within the OMOP CDM using keyword searches within the concept_name
#\' fileds of vocabulary
#\' 
#\' @return a dataframe of standard concepts and their concept_ids, etc
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name)
#\' @param keyword A quoted keyword search term e.g. \'diabetes\'
#\' @param domain OMOP domain name e.g. \'concept\', \'condition_occurrence\'
#\' @param vocab_id Name of specific vocabulary dictionary e.g. \'RxNorm\'
#\' @param standard_only TRUE/FALSE field for standard or non-standard concepts
#\' 
#\' @examples
#\' \\dontrun{
#\'   diabetes_concepts <- find_concepts("diabetes", domain = "Condition")
#\'   metformin_rxnorm  <- find_concepts("metformin", vocab_id = "RxNorm")
#\' }
#\' 
#\' @export
find_concepts <- function(cdm = "cdm",
                          keyword,
                          domain        = NULL,
                          vocab_id      = NULL,
                          standard_only = TRUE) {

  q <- cdm$concept |>
    filter(str_detect(tolower(concept_name), tolower(keyword)))

if (standard_only)      q <- q |> filter(standard_concept == "S")
if (!is.null(domain))   q <- q |> filter(domain_id == domain)
if (!is.null(vocab_id)) q <- q |> filter(domain_id == vocab_id)

q |>
  select(concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, concept_code,
          standard_concept, valid_end_date) |>
  collect()
}
  '
)

# ------------------------------------------------------------------------------------------
# B. Descendant concepts -- all children/grandchildren via CONCEPT_ANCESTOR
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/get_descendants.R"),
  text = '
#\' Find descendants of a specific concept_id
#\' 
#\' Specify a concept code within OMOP CDM to find ALL its descendants - including children, 
#\' grandchildren and so on
#\' 
#\' @return a dataframe of descendant concepts and their details
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name)
#\' @param root_concept_id Concept_id for which you want to see descendants
#\' @param min_sep Minimum level of descendant separation
#\' @param max_sep Maximum level of descendant separation - leave NULL for all
#\' 
#\' @examples
#\' \\dontrun{
#\' # Example: All descendants of Type 2 diabetes mellitus
#\'   t2dm_descendants <- get_descendants(201826)
#\' }
#\' 
#\' @export
get_descendants <- function(cdm = "cdm",
                            root_concept_id,
                            min_sep = 1L,
                            max_sep = NULL) {
  q <- cdm$concept_ancestor |>
    filter(
      ancestor_concept_id      == root_concept_id,
      min_levels_of_separation >= min_sep
    )

  if (!is.null(max_sep)) {
    q <- q |> filter(min_levels_of_separation <= max_sep)
  }

q |>
  inner_join(
    cdm$concept |> select(concept_id, concept_name, domain_id, vocabulary_id, concept_code),
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
  '
)

# ------------------------------------------------------------------------------------------
# C. Ancestor concepts -- walk UP the hierarchy
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/get_ancestors.R"),
  text = '
#\' Find ascendants of a specific concept_id
#\' 
#\' Specifiy a concept code within OMOP CDM to find its ancestor concepts
#\' 
#\' @return a dataframe of ancestor concepts and their details
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name) 
#\' @param leaf_concept_id Concept_id for which you want to see ancestors
#\' @param max_sep Specify the number of degrees of separation for ancestors - leave as NULL 
#\'   for ALL ancestors
#\' 
#\' @examples
#\' \\dontrun {
#\' # Example:
#\'   t2dm_ancestors <- get_ancestors(201826, max_sep = 3L)
#\' }
#\' 
#\' @export
get_ancestors <- function(cem = "cdm", leaf_concept_id, max_sep = NULL) {

  q <- cdm$concept_ancestor |>
    filter(
      descendant_concept_id    == leaf_concept_id,
      min_levels_of_separation >= 1L
    )

  if (!is.null(max_sep)) {
    q <- q |> filter(min_levels_of_separation <= max_sep)
  }

q |>
  inner_join(
    cdm$concept |> select(concept_id, concept_name, domain_id, vocabulary_id, concept_code),
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
  '
)

# ------------------------------------------------------------------------------------------
# D. Source code -> standard OMOP mapping
#    Supports Read v2 ("Read"), CTV3 ("CTV3"), ICD10 ("ICD10"), ICD-10-CM ("ICD10CM"),
#    MedDRA ("MedDRA"), etc
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/map_source_to_standard.R"),
  text = '
#\' Map a source code to its standard OMOP concept_id
#\' 
#\' Takes either a keyword or a source code and maps it to the OMOP standard concept_id 
#\' equivalent
#\' 
#\' @return Concept details of mapped terms, ids, etc
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name)
#\' @param source_vocab_id The vocabulary for the code to map e.g. \'Read\'
#\' @param keyword Specify keyword to search for
#\' @param source_code Code from the source specified in the source_vocab_id
#\' 
#\' @examples
#\' \\dontrun{
#\'   read2diabetes    <- map_source_to_standard("Read",  keyword = "diabetes")
#\'   icd10diabetes    <- map_source_to_standard("ICD10", keyword = "diabetes")
#\'   single_read_code <- map_source_to_standard("Read",  source_code = "C10E.00")
#\' }
#\' 
#\' 
#\' @export
map_source_to_standard <- function(cdm = "cdm",
                                   source_vocab_id,
                                   keyword     = NULL,
                                   source_code = NULL) {
  src <- cdm$concept |>
    filter(vocabulary_id == source_vocab_id)

  if (!is.null(keyword)) {
    src <- src |>
      filter(str_detect(tolower(concept_name), tolower(keyword)))
  }

  if (!is.null(source_code)) {
    src <- src |> filter(concept_code == source_code
  }

  src |>
    inner_join(
      cdm$concept_relationship |> filter(relationship_id == "Maps to"),
      by = c("concept_id" = "concept_id_1")
    ) |>
    inner_join(
      cdm$concept |>
        select(
          standard_concept_id   = concept_id,
          standard_concept_name = concept_name,
          standard_domain       = domain_id,
          standard_vocab        = vocabulary_id,
          standard_code         = concept_code
        ),
      by = c("concept_id_2" = "standcard_concept_id")
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
  '
)

# ------------------------------------------------------------------------------------------
# E. Reverse lookup -- standard concept -> all source codes that map to it
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/get_source_codes.R"),
  text = '
#\' Retrieves source codes from standard OMOP concept_ids
#\' 
#\' Takes a standard OMOP concept_id and returns a list of source codes to which it maps. 
#\' Allows you to give a specific source code if you do no wish to retrieve all
#\' 
#\' @return Tables of source codes and terms which map to the specified code
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name)
#\' @param standard_concept_id A concept_id which is "Standard"
#\' @param source_vocab_id The code for the source data required e.g. "ICD10"
#\' 
#\' @examples
#\' \\dontrun{
#\'     t2dm_all_sources <- get_source_codes(201826)
#\'     t2dm_read_only   <- get_source_codes(201826, source_vocab_id = "Read")
#\' }
#\' 
#\' @export
get_source_codes <- function(cdm = "cdm",
                             standard_concept_id,
                             source_vocab_id = NULL) {
  q <- cdm$concept_relationship |>
    filter(
      concept_id_2    == stancard_concept_id,
      relationship_id == "Maps to"
    ) |>
    inner_join(
      cdm$concept |> select(concept_id, concept_name, vocabulary_id, concept_code),
      by = c("concept_id_1" = "concept_id")
    )

  if (!is.null(source_vocabulary_id)) {
    q <- q |> filter(vocabulary_id == source_vocabulary_id)
  }

  q |>
    select(
      source_concept_id   = concept_id_1,
      source_concept_name = concept_name,
      source_vocab        = vocabulary_id,
      source_code         = concept_code
    ) |>
    arrange(source_vocabm source_code) |>
    collect()
}
  '
)

# ------------------------------------------------------------------------------------------
# F. Concept synonyms -- all alternative names for a concept
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/get_synonyms.R"),
  text = '
#\' Alternative names for a concept
#\' 
#\' Supply any concept_id and return a table with all the synonyms that match it 
#\' along with their language_concept_id, and concept_synonym_name
#\' 
#\' @return Table of all related synonyms to the concept_id specified
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name)
#\' @param concept_ids the concept_id for which you want to retrieve synonyms
#\' 
#\' @examples
#\' \\dontrun{
#\'     t2dm_synonyms <- get_synonyms(201826)
#\' }
#\' 
#\' @export
get_synonyms <- function(cdm = "cdm", concept_ids) {
  cdm$concept_synonym |>
    filter(cdm$concept_id %in% concept_ids) |>
    inner_join(
      cdm$concept |> select(concept_id, concept_name, vocabulary_id),
    by = "concept_id"
    ) |>
    select(concept_id, concept_name, concept_synonym_name, language_concept_id) |>
    arrange(concept_id, concept_synonym_name) |>
    collect()
}
  '
)

# ------------------------------------------------------------------------------------------
# G. Drugs - find all stansdard RxNorm Extension (DM+D) drugs for an RxNorm 'Ingredient'
# ------------------------------------------------------------------------------------------
writeLines(
  con = file.path(pkg_path, "R/get_drugs_list.R"),
  text = '
#\' Retrieve RxNorm Extension (dm+d) drug concepts from an OMOP CDM
#\' 
#\' @param cdm A CDM reference object exposing cdm$concept, cdm$concept_ancestor and
#\'   cdm$concept_relationship as lazy tables. Where cdm contains a list of 
#\'   tbl(duckdb_conn, table_name)
#\' @param ingredients Character vector of RxNorm ingredient names. Case-insensitive
#\' @param dose_from_groups Character vector of RxNorm Dose Form Group names to 
#\'   restrict to (e.g. c("Oral Product", "Inhalation Product")), or "ALL" to apply 
#\'   no dose form filter. Case-insenstitive. Child dose form groups are automatically 
#\'   included via the concept hierarchy
#\' @param combination Controls which ingredient profiles are retained: 
#\'   "single_only"  - Exactly one ingredient; must be one of those specified 
#\'   "any"          - At least one specified ingredient present; co-ingredients from 
#\'                    outside the list are permitted.
#\' "specified_only" - Every ingredient in the product must come from the specified list. 
#\'                    Combinations between specified ingredients are allowed (e.g. 
#\'                    nifedipine + labetalol if both are specified); combinations with 
#\'                    any outside ingredient are excluded
#\' 
#\' @examples
#\' \\dontrun{
#\'     # Sngle oral ingredient - the original use case
#\'     oral_nifedipine <- get_drugs_list(
#\'       cdm              = cdm,
#\'       ingredients      = "nifedipine",
#\'       dose_form_groups = "Oral Product",
#\'       combination      = "single_only")
#\' 
#\'     # Both ingredients, oral, single products only
#\'     oral_mono <- get_drugs_list(
#\'       cdm              = cdm,
#\'       ingredients      = c("nifedipine", "labetalol"),
#\'       dose_form_groups = "Oral Product",
#\'       combination      = "single_only")
#\' 
#\'     # Allow nifedipine+labetalol combinations but not nifedipine+atenolol
#\'     oral_specified_combos <- get_drugs_list(
#\'       cdm              = cdm,
#\'       ingredients      = c("nifedipine", "labetalol"),
#\'       dose_form_group  = "Oral Product",
#\'       combination      = "specified only")
#\' 
#\'     # All dose forms, any co-ingredients
#\'     all_forms_any <- get_drugs_list(
#\'       cdm              = cdm,
#\'       ingredients      = c("nifedipine", "labetalol"),
#\'       dose_form_group  = "ALL",
#\'       combination      = "any")
#\' 
#\'     # Materialise when ready
#\'     results <- oral_mono |> collect()
#\' }
#\' 
#\' @return A lazy table with columns concept_id, concept_name, concept_class_id, 
#\'         vocabulary_id. Call collect() when ready to materialise
#\' 
#\' @export
get_drugs_list <- function(
  cdm = cdm,
  ingredients,
  dose_form_groups = "ALL",
  combination = c("single_only", "any", "specified_only")
) {
  combination <- match.arg(combination)

  ingredients_lc      <- tolower(ingredients)
  dose_form_groups_lc <- if (!identical(dose_form_groups, "ALL")) tolower(dose_form_groups)

  # 1. Target RxNorm ingredient concepts
  target_ingredients <- cdm$concept |>
    filter(
      tolower(concept_name) %in% .env$ingredients_lc,
      vocabulary_id    == "RxNorm",
      concept_calss_id == "Ingredient",
      standard_concept == "S",
      is.na(invalid_reason)
    ) |>
select(ingredient_concept_id = concept_id, ingredient_name = concept_name)

  # 2. All RxNorm descendants of those ingredients
  rxnorm_descendants <- cdm$concept_ancestor |>
    inner_join(
      target_ingredients,
      by = c("ancestor_concept_id" = "ingredient_concept_id")
    ) |>
    inner_join(
      cdm$concept |>
        filter(vocabulary_id = "RxNorm", is.na(invalid_reason)) |>
        select(concept_id),
      by = c("descendant_concept_id" = "concept_id")
    ) |>
    select(rxnorm_bridge_id = descendant_concept_id, ingredient_name)

  # 3. Optionally restrict bridge to specified dose form groups
  if (!identical(dose_form_groups, "ALL")) {
    dfg_descendants <- cdm$concept |>
      filter(
        tolower(concept_name) %in% .env$dose_form_groups_lc,
        concept_class_id == "Dose Form Group",
        vocabulary_id    == "RxNorm"
      ) |>
      select(ancestor_concept_id = concept_id) |>
      inner_join(
        cdm$concept_ancestor |>
          select(ancestor_concept_id, descendant_concept_id),
        by = "ancestor_concept_id"
      ) |>
      select(dfg_concept_id = descendant_concept_id)

    rxnorm_bridge <- rxnorm_descendants |>

'
  )

############################################################################################
## Write the Description file with dependencies
writeLines(
  con = file.path(pkg_path, ""),
  text = '
#\' 

'
  )
############################################################################################
roxygen2::roxygenise(pkg_path)
r_binary_path <- file.path(R.home("bin"), "R")
system2(r_binary_path, c("CMD", "INSTALL", shQuote(pkg_path)))

rm(pkg_path, r_binary_path
############################################################################################
