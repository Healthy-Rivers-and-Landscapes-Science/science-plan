import_mdb_tables <- function(mdb_file, table_names) {
  for (table_name in table_names) {
    csv_file <- tempfile(fileext = ".csv")

    quoted_table_name <- paste0('"', table_name, '"')

    result <- system(
      paste("mdb-export", mdb_file, quoted_table_name, ">", csv_file),
      intern = TRUE,
      ignore.stderr = TRUE
    )

    if (file.exists(csv_file) && file.info(csv_file)$size > 0) {
      table_data <- read.csv(csv_file, stringsAsFactors = FALSE)
      assign(
        x = gsub(" ", "_", tolower(table_name)),
        value = table_data,
        envir = .GlobalEnv
      )
    } else {
      warning(paste("failed to import table '", table_name, "'"))
    }
  }
}
