get_mdb_table_names <- function(mdb_file) {
  raw_tables <- system(paste("mdb-tables -d '|' ", mdb_file), intern = TRUE)
  table_names <- unlist(strsplit(raw_tables, split = "\\|"))
  table_names <- trimws(table_names)
  table_names <- table_names[table_names != ""]
  return(table_names)
}
