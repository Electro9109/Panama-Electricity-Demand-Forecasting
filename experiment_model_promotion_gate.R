
# ============================================================
# PANAMA ELECTRICITY DEMAND FORECASTING
# Candidate Model Validation and Promotion Gate
#
# Features:
#   1. Read-only SQLite data access
#   2. Chronological train/validation/test partitions
#   3. Incumbent versus candidate model comparison
#   4. Configurable rejection-path test
#   5. Unique output folder for every run
#   6. Explicit approval/rejection artifact safeguards
#   7. Saved metrics, thresholds, and decision records
#
# This script DOES NOT deploy or replace production models.
# ============================================================


# ------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------

PROJECT_ROOT <- "C:/Users/ASUS/Coding/R/Panama"

# TRUE  = deliberately force a rejection for safety testing.
# FALSE = use the normal promotion criteria.
FORCE_REJECTION_TEST <- FALSE

# Normal acceptance criteria
NORMAL_MIN_RMSE_IMPROVEMENT <- 0.02
MAX_MAE_WORSENING <- 0.01
MAX_MAPE_WORSENING <- 0.01

# Required RMSE improvement during the forced rejection test.
# The observed candidate improvement was approximately 10%.
TEST_MIN_RMSE_IMPROVEMENT <- 1.00

NUM_TREES <- 300
RANDOM_SEED <- 42
NUM_THREADS <- 2

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


# ------------------------------------------------------------
# 2. PACKAGE AND PATH CHECKS
# ------------------------------------------------------------

required_packages <- c(
  "DBI",
  "RSQLite",
  "dplyr",
  "ranger"
)

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
    "Install the missing packages first: ",
    paste(missing_packages, collapse = ", "),
    "\nExample: install.packages('ranger')"
  )
}

if (!dir.exists(PROJECT_ROOT)) {
  stop("Project directory not found: ", PROJECT_ROOT)
}

setwd(PROJECT_ROOT)

DB_PATH <- file.path(
  PROJECT_ROOT,
  "outputs",
  "pipeline.sqlite"
)

OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "outputs",
  "experiments",
  "model_promotion_gate"
)

if (!file.exists(DB_PATH)) {
  stop("SQLite database not found: ", DB_PATH)
}

dir.create(
  OUTPUT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

# Generate a unique directory for this run.
time_now <- Sys.time()

run_id <- paste0(
  format(time_now, "%Y%m%d_%H%M%S"),
  "_",
  sprintf(
    "%03d",
    as.integer((as.numeric(time_now) %% 1) * 1000)
  )
)

OUTPUT_DIR <- file.path(
  OUTPUT_ROOT,
  paste0("run_", run_id)
)

if (dir.exists(OUTPUT_DIR)) {
  stop(
    "Run directory already exists. ",
    "Refusing to overwrite: ", OUTPUT_DIR
  )
}

if (!dir.create(OUTPUT_DIR, recursive = TRUE)) {
  stop("Could not create output directory: ", OUTPUT_DIR)
}

MIN_RMSE_IMPROVEMENT <- if (FORCE_REJECTION_TEST) {
  TEST_MIN_RMSE_IMPROVEMENT
} else {
  NORMAL_MIN_RMSE_IMPROVEMENT
}

set.seed(RANDOM_SEED)


# ------------------------------------------------------------
# 3. LOAD DATA IN READ-ONLY MODE
# ------------------------------------------------------------

cat("\n============================================\n")
cat("MODEL PROMOTION GATE\n")
cat("============================================\n")

cat(
  "Run mode: ",
  if (FORCE_REJECTION_TEST) {
    "FORCED REJECTION SAFETY TEST"
  } else {
    "NORMAL VALIDATION"
  },
  "\n",
  sep = ""
)

cat("Output directory: ", OUTPUT_DIR, "\n", sep = "")

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = DB_PATH,
  flags = RSQLite::SQLITE_RO
)

data <- tryCatch(
  {
    if (!DBI::dbExistsTable(con, "processed_panama")) {
      stop("Table 'processed_panama' does not exist.")
    }
    
    DBI::dbReadTable(con, "processed_panama")
  },
  finally = {
    DBI::dbDisconnect(con)
  }
)

required_columns <- unique(c(
  "datetime",
  TARGET,
  FEATURES
))

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

data$datetime <- as.POSIXct(
  data$datetime,
  tz = "UTC"
)

if (anyNA(data$datetime)) {
  stop("Some timestamps could not be parsed.")
}

if (anyDuplicated(data$datetime)) {
  stop(
    "Duplicate timestamps detected. ",
    "Resolve duplicate records before training."
  )
}

data <- data |>
  dplyr::arrange(datetime)

# Use the same feature-completeness rule throughout.
rows_before_filter <- nrow(data)

data <- data[
  stats::complete.cases(
    data[, required_columns, drop = FALSE]
  ),
  ,
  drop = FALSE
]

cat("\nData preparation:\n")
cat("Rows before feature checks: ", rows_before_filter, "\n")
cat("Rows retained: ", nrow(data), "\n")
cat("Rows excluded: ", rows_before_filter - nrow(data), "\n")

if (nrow(data) == 0) {
  stop("No usable rows remain after feature checks.")
}


# ------------------------------------------------------------
# 4. CHRONOLOGICAL DATA PARTITIONS
# ------------------------------------------------------------

date_2019 <- as.POSIXct(
  "2019-01-01 00:00:00",
  tz = "UTC"
)

date_july_2019 <- as.POSIXct(
  "2019-07-01 00:00:00",
  tz = "UTC"
)

date_2020 <- as.POSIXct(
  "2020-01-01 00:00:00",
  tz = "UTC"
)

initial_data <- data |>
  dplyr::filter(datetime < date_2019)

batch_1 <- data |>
  dplyr::filter(
    datetime >= date_2019,
    datetime < date_july_2019
  )

validation_data <- data |>
  dplyr::filter(
    datetime >= date_july_2019,
    datetime < date_2020
  )

final_test_data <- data |>
  dplyr::filter(datetime >= date_2020)

partition_sizes <- c(
  initial_training = nrow(initial_data),
  additional_batch = nrow(batch_1),
  promotion_validation = nrow(validation_data),
  final_evaluation = nrow(final_test_data)
)

if (any(partition_sizes == 0)) {
  stop(
    "At least one data partition is empty. ",
    "Check the dataset's date coverage."
  )
}

cat("\n============================================\n")
cat("DATA PARTITIONS\n")
cat("============================================\n")

print(partition_sizes)

cat("\nPromotion validation: July-December 2019\n")
cat("Final evaluation: January 2020 onward\n")

# Save a record of the partition sizes.
write.csv(
  data.frame(
    partition = names(partition_sizes),
    rows = as.integer(partition_sizes)
  ),
  file.path(OUTPUT_DIR, "data_partitions.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# 5. TRAINING AND EVALUATION FUNCTIONS
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


evaluate_rf <- function(model, evaluation_data) {
  actual <- evaluation_data[[TARGET]]
  
  predicted <- predict(
    model,
    data = evaluation_data[
      ,
      FEATURES,
      drop = FALSE
    ]
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
  
  r_squared <- if (
    length(actual) > 1 &&
    denominator > 0
  ) {
    1 - sum(errors^2) / denominator
  } else {
    NA_real_
  }
  
  data.frame(
    rows = length(actual),
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    MAPE = mape,
    R2 = r_squared
  )
}


# ------------------------------------------------------------
# 6. TRAIN INCUMBENT AND CANDIDATE MODELS
# ------------------------------------------------------------

cat("\nTraining incumbent model...\n")

incumbent_model <- train_rf(initial_data)

candidate_training_data <- dplyr::bind_rows(
  initial_data,
  batch_1
) |>
  dplyr::arrange(datetime)

cat("Training candidate model...\n")

candidate_model <- train_rf(candidate_training_data)


# ------------------------------------------------------------
# 7. EVALUATE BOTH ON THE SAME VALIDATION PERIOD
# ------------------------------------------------------------

cat("\nEvaluating both models on July-December 2019...\n")

incumbent_metrics <- evaluate_rf(
  incumbent_model,
  validation_data
)

candidate_metrics <- evaluate_rf(
  candidate_model,
  validation_data
)

comparison <- dplyr::bind_rows(
  cbind(
    model = "Incumbent",
    training_rows = nrow(initial_data),
    incumbent_metrics
  ),
  cbind(
    model = "Candidate",
    training_rows = nrow(candidate_training_data),
    candidate_metrics
  )
)

cat("\n============================================\n")
cat("VALIDATION RESULTS\n")
cat("============================================\n")

print(
  comparison,
  row.names = FALSE,
  digits = 4
)

write.csv(
  comparison,
  file.path(OUTPUT_DIR, "validation_comparison.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# 8. CALCULATE PROMOTION CRITERIA
# ------------------------------------------------------------

if (
  !is.finite(incumbent_metrics$RMSE) ||
  incumbent_metrics$RMSE <= 0 ||
  !is.finite(candidate_metrics$RMSE) ||
  !is.finite(incumbent_metrics$MAE) ||
  incumbent_metrics$MAE <= 0 ||
  !is.finite(candidate_metrics$MAE) ||
  !is.finite(incumbent_metrics$MAPE) ||
  incumbent_metrics$MAPE <= 0 ||
  !is.finite(candidate_metrics$MAPE)
) {
  stop(
    "Metrics are invalid or unsuitable for the promotion gate. ",
    "No candidate will be approved."
  )
}

rmse_improvement <- (
  incumbent_metrics$RMSE - candidate_metrics$RMSE
) / incumbent_metrics$RMSE

mae_change <- (
  candidate_metrics$MAE - incumbent_metrics$MAE
) / incumbent_metrics$MAE

mape_change <- (
  candidate_metrics$MAPE - incumbent_metrics$MAPE
) / incumbent_metrics$MAPE

checks <- data.frame(
  criterion = c(
    "RMSE improvement meets minimum",
    "MAE does not worsen beyond tolerance",
    "MAPE does not worsen beyond tolerance"
  ),
  observed = c(
    rmse_improvement,
    mae_change,
    mape_change
  ),
  threshold = c(
    MIN_RMSE_IMPROVEMENT,
    MAX_MAE_WORSENING,
    MAX_MAPE_WORSENING
  ),
  passed = c(
    rmse_improvement >= MIN_RMSE_IMPROVEMENT,
    mae_change <= MAX_MAE_WORSENING,
    mape_change <= MAX_MAPE_WORSENING
  )
)

cat("\n============================================\n")
cat("PROMOTION CHECKS\n")
cat("============================================\n")

print(
  checks,
  row.names = FALSE,
  digits = 4
)

write.csv(
  checks,
  file.path(OUTPUT_DIR, "promotion_checks.csv"),
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. MAKE THE PROMOTION DECISION
# ------------------------------------------------------------

approved <- all(checks$passed)

decision <- if (approved) {
  "APPROVED"
} else {
  "REJECTED"
}

cat("\n============================================\n")
cat("PROMOTION DECISION\n")
cat("============================================\n")

cat("Decision: ", decision, "\n", sep = "")

if (FORCE_REJECTION_TEST && approved) {
  # A forced-rejection run should not pass its gate.
  # This guard prevents a test run from creating an approved model.
  stop(
    "SAFETY TEST FAILED: candidate passed the forced threshold. ",
    "No approved artifact has been saved by this run."
  )
}

if (approved) {
  cat("Candidate passed all configured criteria.\n")
} else {
  cat("Candidate failed at least one configured criterion.\n")
  cat("The incumbent remains the selected model.\n")
}


# ------------------------------------------------------------
# 10. SAVE THE DECISION RECORD
# ------------------------------------------------------------

decision_record <- list(
  run_id = run_id,
  decision = decision,
  forced_rejection_test = FORCE_REJECTION_TEST,
  timestamp_utc = format(
    Sys.time(),
    tz = "UTC",
    usetz = TRUE
  ),
  validation_period = "2019-07-01 to 2019-12-31",
  final_evaluation_period = "2020-01-01 onward",
  thresholds = list(
    min_rmse_improvement = MIN_RMSE_IMPROVEMENT,
    max_mae_worsening = MAX_MAE_WORSENING,
    max_mape_worsening = MAX_MAPE_WORSENING
  ),
  observed_changes = list(
    rmse_improvement = rmse_improvement,
    mae_relative_change = mae_change,
    mape_relative_change = mape_change
  ),
  checks = checks,
  validation_comparison = comparison,
  training_rows_incumbent = nrow(initial_data),
  training_rows_candidate = nrow(candidate_training_data),
  validation_rows = nrow(validation_data),
  features = FEATURES,
  target = TARGET
)

saveRDS(
  decision_record,
  file.path(OUTPUT_DIR, "promotion_decision.rds")
)


# ------------------------------------------------------------
# 11. APPROVAL PATH
# ------------------------------------------------------------

approved_model_path <- file.path(
  OUTPUT_DIR,
  "approved_candidate_model.rds"
)

if (approved) {
  
  # This branch is only reached when the candidate is approved.
  # Refit on all pre-2020 data before evaluation.
  all_pre_2020 <- data |>
    dplyr::filter(datetime < date_2020) |>
    dplyr::arrange(datetime)
  
  cat(
    "\nRefitting approved candidate using ",
    nrow(all_pre_2020),
    " pre-2020 rows...\n",
    sep = ""
  )
  
  approved_model <- train_rf(all_pre_2020)
  
  final_metrics <- evaluate_rf(
    approved_model,
    final_test_data
  )
  
  final_record <- list(
    model = approved_model,
    model_type = "Random Forest",
    decision = decision,
    training_rows = nrow(all_pre_2020),
    training_start = min(all_pre_2020$datetime),
    training_end = max(all_pre_2020$datetime),
    features = FEATURES,
    target = TARGET,
    validation_metrics = candidate_metrics,
    final_evaluation_metrics = final_metrics,
    note = paste(
      "Experimental artifact only.",
      "This model has not been deployed to the application."
    )
  )
  
  saveRDS(
    final_record,
    approved_model_path
  )
  
  write.csv(
    final_metrics,
    file.path(OUTPUT_DIR, "final_evaluation_metrics.csv"),
    row.names = FALSE
  )
  
  cat("\nFinal evaluation results:\n")
  
  print(
    final_metrics,
    row.names = FALSE,
    digits = 4
  )
  
  if (!file.exists(approved_model_path)) {
    stop("Approval artifact was not saved correctly.")
  }
  
} else {
  
  # Rejected runs must never save an approved model artifact.
  if (file.exists(approved_model_path)) {
    stop(
      "SAFETY FAILURE: rejected run contains an approved model artifact."
    )
  }
  
  cat("\nNo approved model artifact was created for this run.\n")
}


# ------------------------------------------------------------
# 12. VERIFY ARTIFACT SAFETY
# ------------------------------------------------------------

saved_decision <- readRDS(
  file.path(OUTPUT_DIR, "promotion_decision.rds")
)

if (!identical(saved_decision$decision, decision)) {
  stop("Safety check failed: saved decision does not match.")
}

if (identical(decision, "REJECTED")) {
  
  stopifnot(!file.exists(approved_model_path))
  
  cat("PASS: Rejected candidate was not saved as approved.\n")
  cat("PASS: This run has no approved model artifact.\n")
  
} else {
  
  stopifnot(file.exists(approved_model_path))
  
  cat("PASS: Approved model artifact exists for this run.\n")
}

cat("PASS: Decision record was saved and verified.\n")
cat("PASS: Every run uses its own output directory.\n")


# ------------------------------------------------------------
# 13. FINISH
# ------------------------------------------------------------

cat("\n============================================\n")
cat("EXPERIMENT FINISHED\n")
cat("============================================\n")

cat("Decision: ", decision, "\n", sep = "")
cat("Run ID: ", run_id, "\n", sep = "")
cat("Results: ", OUTPUT_DIR, "\n", sep = "")

cat("Database access: read-only\n")
cat("Production models: not modified\n")
cat("Application deployment: not performed\n")

if (FORCE_REJECTION_TEST) {
  cat("\nRemember to set FORCE_REJECTION_TEST <- FALSE\n")
  cat("before running a normal promotion evaluation.\n")
}

cat("============================================\n")
