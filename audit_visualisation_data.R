
# ============================================================
# VISUALIZATION DATA AUDIT
# Project: Panama Electricity Demand Forecasting
# Purpose: Inspect data sources before building the Shiny app
# ============================================================

library(DBI)
library(RSQLite)

# Project paths
db_path <- "outputs/pipeline.sqlite"

prediction_path <-
  "outputs/results/validation_backtest_rf_predictions.csv"

comparison_path <-
  "outputs/results/rf_vs_seasonal_naive.csv"

backtest_path <-
  "outputs/results/validation_backtest_rf.rds"

# Capture console output in a report file as well
dir.create("outputs/reports", recursive = TRUE, showWarnings = FALSE)

audit_file <- "outputs/reports/visualisation_data_audit.txt"

sink(audit_file, split = TRUE)

cat("====================================================\n")
cat("PANAMA VISUALIZATION DATA AUDIT\n")
cat("====================================================\n\n")

# ------------------------------------------------------------
# 1. SQLITE DATABASE
# ------------------------------------------------------------

cat("\n1. DATABASE AUDIT\n")
cat("-----------------\n")

if (!file.exists(db_path)) {
  
  cat("DATABASE NOT FOUND:", db_path, "\n")
  
} else {
  
  con <- dbConnect(SQLite(), db_path)
  
  tryCatch({
    
    tables <- dbListTables(con)
    
    cat("Database:", db_path, "\n")
    cat("Tables:", paste(tables, collapse = ", "), "\n\n")
    
    for (tbl in tables) {
      
      cat("\nTABLE:", tbl, "\n")
      cat("-----------------\n")
      
      schema <- dbGetQuery(
        con,
        paste0("PRAGMA table_info('", tbl, "')")
      )
      
      print(schema[, c("name", "type"), drop = FALSE])
      
      row_count <- dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          dbQuoteIdentifier(con, tbl)
        )
      )$n[1]
      
      cat("Rows:", row_count, "\n")
      
      cols <- dbListFields(con, tbl)
      
      if (all(c("datetime", "nat_demand") %in% cols)) {
        
        dat <- dbReadTable(con, tbl)
        
        dat$datetime <- as.POSIXct(
          dat$datetime,
          tz = "UTC"
        )
        
        dat$nat_demand <- suppressWarnings(
          as.numeric(dat$nat_demand)
        )
        
        cat("Demand records:", nrow(dat), "\n")
        cat(
          "Missing timestamps:",
          sum(is.na(dat$datetime)),
          "\n"
        )
        cat(
          "Missing demand values:",
          sum(is.na(dat$nat_demand)),
          "\n"
        )
        
        valid_dates <- dat$datetime[!is.na(dat$datetime)]
        
        if (length(valid_dates) > 0) {
          cat(
            "Date range:",
            format(min(valid_dates), "%Y-%m-%d %H:%M"),
            "to",
            format(max(valid_dates), "%Y-%m-%d %H:%M"),
            "\n"
          )
        }
        
        cat(
          "Duplicate timestamps:",
          sum(duplicated(dat$datetime[!is.na(dat$datetime)])),
          "\n"
        )
        
        cat(
          "Demand summary:\n"
        )
        
        print(
          summary(dat$nat_demand)
        )
        
        numeric_cols <- names(dat)[
          vapply(dat, is.numeric, logical(1))
        ]
        
        cat(
          "\nNumeric columns:",
          paste(numeric_cols, collapse = ", "),
          "\n"
        )
        
        weather_cols <- grep(
          "T2M|QV2M|TQL|W2M",
          names(dat),
          value = TRUE,
          ignore.case = TRUE
        )
        
        cat(
          "Weather columns:",
          if (length(weather_cols)) {
            paste(weather_cols, collapse = ", ")
          } else {
            "None found"
          },
          "\n"
        )
        
        # Check whether timestamps have hourly gaps.
        # Interpret this only after confirming hourly data.
        ordered_dates <- sort(
          unique(dat$datetime[!is.na(dat$datetime)])
        )
        
        if (length(ordered_dates) > 1) {
          gaps <- as.numeric(
            diff(ordered_dates),
            units = "hours"
          )
          
          cat(
            "Intervals not equal to one hour:",
            sum(abs(gaps - 1) > 1e-6),
            "\n"
          )
        }
      }
    }
    
  }, finally = {
    dbDisconnect(con)
  })
}

# ------------------------------------------------------------
# 2. FORECAST PREDICTIONS
# ------------------------------------------------------------

cat("\n\n2. FORECAST PREDICTIONS\n")
cat("-----------------------\n")

if (!file.exists(prediction_path)) {
  
  cat("Prediction CSV not found:", prediction_path, "\n")
  
} else {
  
  pred <- read.csv(
    prediction_path,
    stringsAsFactors = FALSE
  )
  
  cat("Rows:", nrow(pred), "\n")
  cat("Columns:", paste(names(pred), collapse = ", "), "\n\n")
  
  print(head(pred, 5))
  
  if ("horizon" %in% names(pred)) {
    cat("\nRows per horizon:\n")
    print(table(pred$horizon, useNA = "ifany"))
  }
  
  if ("origin" %in% names(pred)) {
    cat("\nForecast origins:\n")
    print(sort(unique(pred$origin)))
  }
  
  if (all(c("actual_demand", "predicted_demand") %in% names(pred))) {
    
    pred$actual_demand <- as.numeric(pred$actual_demand)
    pred$predicted_demand <- as.numeric(pred$predicted_demand)
    
    valid <- complete.cases(
      pred[, c("actual_demand", "predicted_demand")]
    )
    
    errors <- pred$actual_demand[valid] -
      pred$predicted_demand[valid]
    
    cat("\nOverall saved-prediction diagnostics:\n")
    cat("Valid prediction pairs:", sum(valid), "\n")
    
    if (length(errors) > 0) {
      cat("MAE:", mean(abs(errors)), "\n")
      cat("RMSE:", sqrt(mean(errors^2)), "\n")
      cat("Mean error (actual - predicted):", mean(errors), "\n")
    }
  }
}

# ------------------------------------------------------------
# 3. MODEL COMPARISON
# ------------------------------------------------------------

cat("\n\n3. MODEL COMPARISON\n")
cat("-------------------\n")

if (!file.exists(comparison_path)) {
  
  cat("Comparison CSV not found:", comparison_path, "\n")
  
} else {
  
  comp <- read.csv(
    comparison_path,
    stringsAsFactors = FALSE
  )
  
  cat("Rows:", nrow(comp), "\n")
  cat("Columns:", paste(names(comp), collapse = ", "), "\n\n")
  
  print(comp)
}

# ------------------------------------------------------------
# 4. BACKTEST RDS
# ------------------------------------------------------------

cat("\n\n4. BACKTEST RDS\n")
cat("---------------\n")

if (!file.exists(backtest_path)) {
  
  cat("Backtest RDS not found:", backtest_path, "\n")
  
} else {
  
  bt <- readRDS(backtest_path)
  
  cat("Top-level object names:\n")
  print(names(bt))
  
  if ("summary_metrics" %in% names(bt)) {
    cat("\nBacktest summary metrics:\n")
    print(as.data.frame(bt$summary_metrics))
  }
}

# ------------------------------------------------------------
# 5. FINAL SUMMARY
# ------------------------------------------------------------

cat("\n\n5. FILE AVAILABILITY\n")
cat("--------------------\n")

paths <- c(
  database = db_path,
  predictions = prediction_path,
  comparison = comparison_path,
  backtest = backtest_path
)

print(
  data.frame(
    artifact = names(paths),
    path = unname(paths),
    exists = file.exists(paths),
    row.names = NULL
  )
)

cat("\nAudit finished.\n")
cat("Report saved to:", audit_file, "\n")

sink()
