
# R/database.R

library(DBI)
library(RSQLite)

get_db_path <- function() {
  "outputs/pipeline.sqlite"
}

connect_db <- function(db_path = get_db_path()) {
  db_dir <- dirname(db_path)
  
  if (!dir.exists(db_dir)) {
    dir.create(db_dir, recursive = TRUE)
  }
  
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = db_path
  )
  
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  
  con
}

# Execute a function with an open database connection.
with_db <- function(fn, db_path = get_db_path()) {
  con <- connect_db(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  fn(con)
}

# List tables in the database.
list_db_tables <- function(db_path = get_db_path()) {
  with_db(
    function(con) DBI::dbListTables(con),
    db_path = db_path
  )
}
