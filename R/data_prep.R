
# R/data_prep.R
# Requires R/database.R and a country profile.

# Import a CSV into SQLite.
# Existing tables are protected unless overwrite = TRUE.
import_raw_data <- function(
    profile,
    db_path = get_db_path(),
    overwrite = FALSE
) {
  if (!file.exists(profile$csv_path)) {
    stop("CSV file not found: ", profile$csv_path)
  }
  
  raw <- read.csv(
    profile$csv_path,
    stringsAsFactors = FALSE
  )
  
  if (!profile$datetime_col %in% names(raw)) {
    stop("Datetime column missing: ", profile$datetime_col)
  }
  
  parsed_time <- as.POSIXct(
    raw[[profile$datetime_col]],
    tz = "UTC"
  )
  
  if (anyNA(parsed_time)) {
    stop("Some datetime values could not be parsed.")
  }
  
  raw[[profile$datetime_col]] <- format(
    parsed_time,
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  raw <- raw[order(raw[[profile$datetime_col]]), ]
  table_name <- paste0("raw_", tolower(profile$name))
  
  with_db(function(con) {
    if (DBI::dbExistsTable(con, table_name) && !overwrite) {
      stop(
        "Table already exists: ", table_name,
        ". Set overwrite = TRUE to replace it."
      )
    }
    
    DBI::dbWriteTable(
      con,
      table_name,
      raw,
      overwrite = overwrite
    )
  }, db_path = db_path)
  
  invisible(nrow(raw))
}


# Read existing raw records from SQLite.
read_raw_data <- function(
    profile,
    db_path = get_db_path()
) {
  table_name <- paste0("raw_", tolower(profile$name))
  
  raw <- with_db(function(con) {
    if (!DBI::dbExistsTable(con, table_name)) {
      stop(
        "Table not found: ", table_name,
        ". Import the dataset first."
      )
    }
    
    DBI::dbReadTable(con, table_name)
  }, db_path = db_path)
  
  raw[[profile$datetime_col]] <- as.POSIXct(
    raw[[profile$datetime_col]],
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  raw <- raw[order(raw[[profile$datetime_col]]), ]
  rownames(raw) <- NULL
  
  raw
}


# Backwards-compatible wrapper for the existing pipeline.
# Reads existing records; imports only if the table is absent.
load_and_store <- function(
    profile,
    db_path = get_db_path(),
    overwrite = FALSE
) {
  table_name <- paste0("raw_", tolower(profile$name))
  
  exists <- with_db(
    function(con) DBI::dbExistsTable(con, table_name),
    db_path = db_path
  )
  
  if (overwrite || !exists) {
    import_raw_data(
      profile,
      db_path = db_path,
      overwrite = overwrite
    )
  }
  
  read_raw_data(profile, db_path)
}


# Save processed records separately.
store_processed <- function(
    df,
    profile,
    db_path = get_db_path(),
    overwrite = TRUE
) {
  table_name <- paste0("processed_", tolower(profile$name))
  processed <- df
  
  if (profile$datetime_col %in% names(processed)) {
    processed[[profile$datetime_col]] <- format(
      as.POSIXct(processed[[profile$datetime_col]], tz = "UTC"),
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  }
  
  with_db(function(con) {
    if (DBI::dbExistsTable(con, table_name) && !overwrite) {
      stop("Table already exists: ", table_name)
    }
    
    DBI::dbWriteTable(
      con,
      table_name,
      processed,
      overwrite = overwrite
    )
  }, db_path = db_path)
  
  invisible(df)
}
