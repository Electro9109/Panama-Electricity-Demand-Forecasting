
# ============================================================
# PANAMA ELECTRICITY FORECASTING
# Simulated Hourly Data Ingestion + Retraining
#
# This is an isolated proof of concept.
#
# - Reads pipeline.sqlite in read-only mode.
# - Simulates hourly batches from historical records.
# - Validates timestamps, duplicates, missing values and features.
# - Evaluates the current model before adding each batch.
# - Retrains only after a batch passes validation.
# - Saves experimental artifacts in a unique run directory.
# - Never modifies production models, the database, or Shiny apps.
#
# IMPORTANT:
# This tests ingestion mechanics, not live API connectivity.
# The historical 2020 records were examined in earlier experiments,
# so results are not an independent final model evaluation.
# ============================================================


# ------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------

PROJECT_ROOT <- "C:/Users/ASUS/Coding/R/Panama"

# Test that a deliberately damaged batch is rejected.
RUN_NEGATIVE_TEST <- TRUE

# Simulated batches: seven days each.
BATCH_DAYS <- 7L

NUM_TREES <- 300L
NUM_THREADS <- 2L
RANDOM_SEED <- 42L

TARGET <- "nat_demand"

FEATURES <- c(
  "lag_1",
  "roll_mean",
  "hour_cos",
  "lag_168",
  "hour_sin",
  "lag_24",
  "roll_sd",
  "is_weekend",
  "holiday_flag",
  "TQL_toc",
  "month_cos",
  "QV2M_toc",
  "T2M_toc",
  "TQL_san",
  "month_sin"
)

REQUIRED_COLUMNS <- unique(c(
  "datetime",
  TARGET,
  FEATURES
))


# ------------------------------------------------------------
# 2. CHECK ENVIRONMENT
# ------------------------------------------------------------

if (!dir.exists(PROJECT_ROOT)) {
  stop("Project root not found: ", PROJECT_ROOT)
}

setwd(PROJECT_ROOT)

required_packages <- c("DBI", "RSQLite", "ranger")

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

DB_PATH <- file.path(
  PROJECT_ROOT, "outputs", "pipeline.sqlite"
)

OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "outputs",
  "experiments",
  "hourly_ingestion"
)

if (!file.exists(DB_PATH)) {
  stop("Database not found: ", DB_PATH)
}

dir.create(
  OUTPUT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

run_id <- paste0(
  format(Sys.time(), "%Y%m%d_%H%M%S"),
  "_",
  sprintf(
    "%03d",
    as.integer((as.numeric(Sys.time()) %% 1) * 1000)
  )
)

OUTPUT_DIR <- file.path(
  OUTPUT_ROOT,
  paste0("run_", run_id)
)

if (dir.exists(OUTPUT_DIR)) {
  stop("Run directory already exists: ", OUTPUT_DIR)
}

if (!dir.create(OUTPUT_DIR, recursive = TRUE)) {
  stop("Could not create experiment output directory.")
}

set.seed(RANDOM_SEED)

cat("\n============================================\n")
cat("SIMULATED HOURLY INGESTION EXPERIMENT\n")
cat("============================================\n")
cat("Run ID: ", run_id, "\n", sep = "")
cat("Output: ", OUTPUT_DIR, "\n", sep = "")
cat("Database access: READ-ONLY\n")


# ------------------------------------------------------------
# 3. READ HISTORICAL SOURCE DATA
# ------------------------------------------------------------

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = DB_PATH,
  flags = RSQLite::SQLITE_RO
)

source_data <- tryCatch(
  {
    if (!DBI::dbExistsTable(con, "processed_panama")) {
      stop("processed_panama table does not exist.")
    }
    
    DBI::dbReadTable(con, "processed_panama")
  },
  finally = {
    DBI::dbDisconnect(con)
  }
)

missing_columns <- setdiff(
  REQUIRED_COLUMNS,
  names(source_data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

source_data$datetime <- as.POSIXct(
  source_data$datetime,
  tz = "UTC"
)

if (anyNA(source_data$datetime)) {
  stop("Source data contains timestamps that cannot be parsed.")
}

source_data <- source_data[
  order(source_data$datetime),
  ,
  drop = FALSE
]

if (anyDuplicated(source_data$datetime)) {
  stop("Source data contains duplicate timestamps.")
}

cat("\nSource records: ", nrow(source_data), "\n", sep = "")
cat(
  "Source date range: ",
  format(min(source_data$datetime), "%Y-%m-%d %H:%M"),
  " to ",
  format(max(source_data$datetime), "%Y-%m-%d %H:%M"),
  "\n",
  sep = ""
)


# ------------------------------------------------------------
# 4. DEFINE A BATCH VALIDATOR
# ------------------------------------------------------------

validate_batch <- function(batch, expected_start, expected_end) {
  
  issues <- character(0)
  
  if (nrow(batch) == 0) {
    return(list(
      valid = FALSE,
      issues = "Batch is empty.",
      rows = 0L,
      expected_rows = NA_integer_
    ))
  }
  
  missing_cols <- setdiff(
    REQUIRED_COLUMNS,
    names(batch)
  )
  
  if (length(missing_cols) > 0) {
    issues <- c(
      issues,
      paste(
        "Missing columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
    
    return(list(
      valid = FALSE,
      issues = issues,
      rows = nrow(batch),
      expected_rows = NA_integer_
    ))
  }
  
  timestamps <- batch$datetime
  
  if (!inherits(timestamps, "POSIXct") ||
      anyNA(timestamps)) {
    
    issues <- c(
      issues,
      "Invalid or missing timestamps."
    )
    
  } else {
    
    if (anyDuplicated(timestamps)) {
      issues <- c(
        issues,
        "Duplicate timestamps found."
      )
    }
    
    if (any(timestamps < expected_start |
            timestamps >= expected_end)) {
      issues <- c(
        issues,
        "Records fall outside the expected batch window."
      )
    }
    
    expected_times <- seq(
      from = expected_start,
      to = expected_end - 3600,
      by = "hour"
    )
    
    expected_rows <- length(expected_times)
    
    if (length(timestamps) != expected_rows) {
      issues <- c(
        issues,
        paste0(
          "Expected ",
          expected_rows,
          " hourly rows but received ",
          length(timestamps),
          "."
        )
      )
    }
    
    if (length(timestamps) > 0) {
      sorted_times <- sort(timestamps)
      
      if (any(diff(as.numeric(sorted_times)) != 3600)) {
        issues <- c(
          issues,
          "Hourly intervals contain gaps or irregular spacing."
        )
      }
      
      missing_times <- setdiff(
        as.numeric(expected_times),
        as.numeric(timestamps)
      )
      
      if (length(missing_times) > 0) {
        issues <- c(
          issues,
          paste(
            length(missing_times),
            "expected hourly timestamps are missing."
          )
        )
      }
    }
  }
  
  numeric_columns <- c(TARGET, FEATURES)
  
  non_numeric <- numeric_columns[
    !vapply(
      batch[, numeric_columns, drop = FALSE],
      is.numeric,
      logical(1)
    )
  ]
  
  if (length(non_numeric) > 0) {
    issues <- c(
      issues,
      paste(
        "Non-numeric model columns:",
        paste(non_numeric, collapse = ", ")
      )
    )
  } else {
    
    missing_counts <- vapply(
      batch[, numeric_columns, drop = FALSE],
      function(x) sum(is.na(x) | !is.finite(x)),
      numeric(1)
    )
    
    bad_columns <- names(missing_counts)[
      missing_counts > 0
    ]
    
    if (length(bad_columns) > 0) {
      issues <- c(
        issues,
        paste(
          "Missing or non-finite values in:",
          paste(bad_columns, collapse = ", ")
        )
      )
    }
    
    if (all(is.finite(batch[[TARGET]])) &&
        any(batch[[TARGET]] < 0)) {
      issues <- c(
        issues,
        "Negative demand values detected."
      )
    }
  }
  
  list(
    valid = length(issues) == 0,
    issues = unique(issues),
    rows = nrow(batch),
    expected_rows = if (exists("expected_rows")) {
      expected_rows
    } else {
      NA_integer_
    }
  )
}


# ------------------------------------------------------------
# 5. TRAINING AND METRIC FUNCTIONS
# ------------------------------------------------------------

train_rf <- function(training_data) {
  
  model_data <- training_data[
    ,
    c(TARGET, FEATURES),
    drop = FALSE
  ]
  
  ranger::ranger(
    dependent.variable.name = TARGET,
    data = model_data,
    num.trees = NUM_TREES,
    seed = RANDOM_SEED,
    num.threads = NUM_THREADS
  )
}


evaluate_rf <- function(model, batch) {
  
  actual <- batch[[TARGET]]
  
  predicted <- predict(
    model,
    data = batch[, FEATURES, drop = FALSE]
  )$predictions
  
  errors <- actual - predicted
  
  nonzero <- actual != 0
  
  mape <- if (any(nonzero)) {
    mean(
      abs(errors[nonzero] / actual[nonzero])
    ) * 100
  } else {
    NA_real_
  }
  
  denominator <- sum(
    (actual - mean(actual))^2
  )
  
  r2 <- if (length(actual) > 1 && denominator > 0) {
    1 - sum(errors^2) / denominator
  } else {
    NA_real_
  }
  
  data.frame(
    rows = length(actual),
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    MAPE = mape,
    R2 = r2
  )
}


# ------------------------------------------------------------
# 6. SET UP THE INITIAL TRAINING HISTORY
# ------------------------------------------------------------

initial_cutoff <- as.POSIXct(
  "2020-01-01 00:00:00",
  tz = "UTC"
)

history <- source_data[
  source_data$datetime < initial_cutoff,
  ,
  drop = FALSE
]

# Rows missing features cannot be used to fit this model.
history <- history[
  complete.cases(
    history[, c(TARGET, FEATURES), drop = FALSE]
  ),
  ,
  drop = FALSE
]

if (nrow(history) == 0) {
  stop("No complete pre-2020 training rows are available.")
}

history <- history[
  order(history$datetime),
  ,
  drop = FALSE
]

cat("\nInitial training history: ", nrow(history), " rows\n", sep = "")
cat("Training initial model...\n")

current_model <- train_rf(history)

cat("Initial model trained.\n")


# ------------------------------------------------------------
# 7. CREATE HISTORICAL BATCHES TO SIMULATE ARRIVAL
# ------------------------------------------------------------

simulation_start <- as.POSIXct(
  "2020-01-01 00:00:00",
  tz = "UTC"
)

simulation_end <- max(source_data$datetime) + 3600

batch_starts <- seq(
  from = simulation_start,
  to = simulation_end - 3600,
  by = BATCH_DAYS * 24 * 3600
)

batch_windows <- lapply(
  batch_starts,
  function(start_time) {
    end_time <- min(
      start_time + BATCH_DAYS * 24 * 3600,
      simulation_end
    )
    
    list(
      start = start_time,
      end = end_time
    )
  }
)

# Remove any empty final window.
batch_windows <- Filter(
  function(w) w$end > w$start,
  batch_windows
)

if (length(batch_windows) == 0) {
  stop("No batches available in the simulation period.")
}

# This audit log records what happened to each batch.
audit_log <- data.frame(
  batch_id = character(),
  start_time = character(),
  end_time = character(),
  received_rows = integer(),
  expected_rows = integer(),
  validation_status = character(),
  action = character(),
  pre_update_MAE = numeric(),
  pre_update_RMSE = numeric(),
  pre_update_MAPE = numeric(),
  model_training_rows = integer(),
  issues = character(),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 8. NEGATIVE TEST: VERIFY A DAMAGED BATCH IS REJECTED
# ------------------------------------------------------------

if (RUN_NEGATIVE_TEST) {
  
  cat("\n============================================\n")
  cat("NEGATIVE VALIDATION TEST\n")
  cat("============================================\n")
  
  test_window <- batch_windows[[1]]
  
  test_batch <- source_data[
    source_data$datetime >= test_window$start &
      source_data$datetime < test_window$end,
    ,
    drop = FALSE
  ]
  
  if (nrow(test_batch) > 1) {
    
    # Simulate a missing hourly record.
    damaged_batch <- test_batch[-1, , drop = FALSE]
    
    test_result <- validate_batch(
      damaged_batch,
      test_window$start,
      test_window$end
    )
    
    cat("Original batch rows: ", nrow(test_batch), "\n", sep = "")
    cat("Damaged batch rows: ", nrow(damaged_batch), "\n", sep = "")
    cat(
      "Validation result: ",
      if (test_result$valid) "ACCEPTED" else "REJECTED",
      "\n",
      sep = ""
    )
    
    if (test_result$valid) {
      stop(
        "SAFETY TEST FAILED: damaged batch was accepted."
      )
    }
    
    cat("PASS: Damaged batch was rejected.\n")
    
    cat("Detected issues:\n")
    for (issue in test_result$issues) {
      cat(" - ", issue, "\n", sep = "")
    }
    
  } else {
    stop("Cannot run negative test: first batch is too small.")
  }
}


# ------------------------------------------------------------
# 9. PROCESS EACH SIMULATED INCOMING BATCH
# ------------------------------------------------------------

cat("\n============================================\n")
cat("PROCESSING SIMULATED BATCHES\n")
cat("============================================\n")

for (i in seq_along(batch_windows)) {
  
  window <- batch_windows[[i]]
  
  batch_id <- sprintf("BATCH_%03d", i)
  
  cat("\n--------------------------------------------\n")
  cat(batch_id, "\n")
  cat(
    "Window: ",
    format(window$start, "%Y-%m-%d %H:%M"),
    " to ",
    format(window$end, "%Y-%m-%d %H:%M"),
    " (end exclusive)\n",
    sep = ""
  )
  
  # Simulate receiving records for this time window.
  batch <- source_data[
    source_data$datetime >= window$start &
      source_data$datetime < window$end,
    ,
    drop = FALSE
  ]
  
  cat("Received rows: ", nrow(batch), "\n", sep = "")
  
  # Validate before evaluating or adding to history.
  validation <- validate_batch(
    batch,
    window$start,
    window$end
  )
  
  if (!validation$valid) {
    
    cat("Validation: REJECTED\n")
    
    for (issue in validation$issues) {
      cat(" - ", issue, "\n", sep = "")
    }
    
    audit_log <- rbind(
      audit_log,
      data.frame(
        batch_id = batch_id,
        start_time = as.character(window$start),
        end_time = as.character(window$end),
        received_rows = nrow(batch),
        expected_rows = validation$expected_rows,
        validation_status = "REJECTED",
        action = "NO_APPEND_NO_RETRAIN",
        pre_update_MAE = NA_real_,
        pre_update_RMSE = NA_real_,
        pre_update_MAPE = NA_real_,
        model_training_rows = nrow(history),
        issues = paste(validation$issues, collapse = " | "),
        stringsAsFactors = FALSE
      )
    )
    
    # Invalid data never enters the working history.
    next
  }
  
  cat("Validation: ACCEPTED\n")
  
  # Evaluate the current model BEFORE training on this batch.
  # This is a one-step-ahead style evaluation because the batch
  # already contains its historical lag/weather feature values.
  batch_metrics <- evaluate_rf(
    current_model,
    batch
  )
  
  cat("Current model, before update:\n")
  print(
    batch_metrics,
    row.names = FALSE,
    digits = 4
  )
  
  # Append accepted records only to in-memory experimental history.
  proposed_history <- rbind(
    history,
    batch
  )
  
  proposed_history <- proposed_history[
    order(proposed_history$datetime),
    ,
    drop = FALSE
  ]
  
  # Verify the append has not created duplicate timestamps.
  if (anyDuplicated(proposed_history$datetime)) {
    stop(
      "Duplicate timestamps would result from appending ",
      batch_id,
      ". Stopping safely."
    )
  }
  
  # Train a candidate from accumulated data.
  cat("Retraining candidate model...\n")
  
  candidate_model <- train_rf(proposed_history)
  
  # Only update the in-memory active model after training succeeds.
  current_model <- candidate_model
  history <- proposed_history
  
  cat("PASS: Batch accepted and candidate retrained.\n")
  cat(
    "Updated training history: ",
    nrow(history),
    " rows\n",
    sep = ""
  )
  
  # Save this experimental model separately.
  # No production model path is used.
  saveRDS(
    current_model,
    file.path(
      OUTPUT_DIR,
      paste0("experimental_model_after_", batch_id, ".rds")
    )
  )
  
  audit_log <- rbind(
    audit_log,
    data.frame(
      batch_id = batch_id,
      start_time = as.character(window$start),
      end_time = as.character(window$end),
      received_rows = nrow(batch),
      expected_rows = validation$expected_rows,
      validation_status = "ACCEPTED",
      action = "APPENDED_AND_RETRAINED",
      pre_update_MAE = batch_metrics$MAE,
      pre_update_RMSE = batch_metrics$RMSE,
      pre_update_MAPE = batch_metrics$MAPE,
      model_training_rows = nrow(history),
      issues = "",
      stringsAsFactors = FALSE
    )
  )
}


# ------------------------------------------------------------
# 10. SAVE AUDIT RESULTS
# ------------------------------------------------------------

write.csv(
  audit_log,
  file.path(OUTPUT_DIR, "ingestion_audit.csv"),
  row.names = FALSE
)

saveRDS(
  list(
    run_id = run_id,
    run_mode = "HISTORICAL_BATCH_SIMULATION",
    initial_training_rows = nrow(
      source_data[
        source_data$datetime < initial_cutoff &
          complete.cases(
            source_data[, c(TARGET, FEATURES), drop = FALSE]
          ),
        ,
        drop = FALSE
      ]
    ),
    final_training_rows = nrow(history),
    processed_batches = nrow(audit_log),
    accepted_batches = sum(
      audit_log$validation_status == "ACCEPTED"
    ),
    rejected_batches = sum(
      audit_log$validation_status == "REJECTED"
    ),
    audit_log = audit_log,
    note = paste(
      "Historical replay only.",
      "No live API was used.",
      "No database or production model was modified."
    )
  ),
  file.path(OUTPUT_DIR, "experiment_summary.rds")
)


# ------------------------------------------------------------
# 11. FINAL SUMMARY
# ------------------------------------------------------------

cat("\n============================================\n")
cat("INGESTION EXPERIMENT SUMMARY\n")
cat("============================================\n")

if (nrow(audit_log) > 0) {
  print(
    audit_log[
      ,
      c(
        "batch_id",
        "received_rows",
        "validation_status",
        "action",
        "model_training_rows"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
}

cat("\nAccepted batches: ",
    sum(audit_log$validation_status == "ACCEPTED"),
    "\n",
    sep = "")

cat("Rejected batches: ",
    sum(audit_log$validation_status == "REJECTED"),
    "\n",
    sep = "")

cat("Final experimental training rows: ", nrow(history), "\n", sep = "")

cat("\nSaved artifacts:\n")
cat(" - ingestion_audit.csv\n")
cat(" - experiment_summary.rds\n")
cat(" - experimental_model_after_BATCH_*.rds\n")

cat("\nOutput directory: ", OUTPUT_DIR, "\n", sep = "")
cat("Database modified: NO\n")
cat("Production models modified: NO\n")
cat("Shiny applications modified: NO\n")
cat("Live API connected: NO\n")

cat("\n============================================\n")
cat("EXPERIMENT FINISHED\n")
cat("============================================\n")
