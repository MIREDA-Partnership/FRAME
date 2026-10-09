req_packages <- c("DBI", "duckdb","dplyr","purrr")


invisible(suppressPackageStartupMessages(
  lapply(req_packages, library, character.only = TRUE, logical.return = FALSE)))
rm(req_packages)


omop_db_file = paste0(  "frame_elixir_bisl_omop_cdm_", last_omop_etl_date,".duckdb")

frame_db_file = paste0( "FRAME_study_elixir_bisl_omop_cdm_", last_omop_etl_date,".duckdb")


db_folder = "B:/BRC_Elixir/Durbaba- MIREDA/test OMOP/FRAME OMOP/cdm/"


out_compare_file = paste0(reporting_site_text,"_","FRAME_compare_omop__", last_omop_etl_date,".csv")


omop_db_path  <- file.path(db_folder , omop_db_file)
frame_db_path <- file.path(db_folder , frame_db_file)







table_stats_function <- function(db_filename,column_name_start =""){
  con <- dbConnect(duckdb(), dbdir = db_filename)
  con_tables <- dbListTables(con)
  
table_stats <- map_dfr(con_tables, function(tbl) {
  
  n_rows <- dbGetQuery(
    con,
    sprintf("SELECT COUNT(*) AS n FROM %s", dbQuoteIdentifier(con, tbl))
  )$n
  
  n_cols <- dbGetQuery(
    con,
    sprintf("DESCRIBE %s", dbQuoteIdentifier(con, tbl))
  ) |> nrow()
  

  
  tibble(
    table_name = tbl,
    rows = n_rows,
    columns = n_cols
  )
  
})


dbDisconnect(con, shutdown = TRUE)

names( table_stats) <- c("table_name",
                        paste0(column_name_start,"rows"),
                        paste0(column_name_start,"columns")
                        )

return( table_stats)
}



omop_table_stats <- table_stats_function(omop_db_path,"omop_")

frame_table_stats <- table_stats_function(frame_db_path,"frame_")

compare_table_stats <-  full_join(omop_table_stats,frame_table_stats, by="table_name")

write.csv(compare_table_stats,paste0(db_folder,out_compare_file),row.names = F,na="")
