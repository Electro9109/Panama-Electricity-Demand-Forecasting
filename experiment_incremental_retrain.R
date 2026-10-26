
# ============================================================
# TEMPORARY EXPERIMENT: SIMULATED MODEL UPDATES
# Panama electricity demand forecasting
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)

# ------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------

project_dir <- "C:/Users/ASUS/Coding/R/Panama"

db_path <- file.path(project_dir, "outputs/pipeline.sqlite")

experiment_dir <- file.path(
  project_dir,
  "outputs/experiments/incremental_retrain"
)

dir.create(
  experiment_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!requireNamespace("ranger", quietly = TRUE)) {
  install.packages("ranger")
}

# Use only features already available in processed_panama.
features <- c(
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
# 2. LOAD HISTORICAL DATA
# Read-only connection: this script does not modify SQLite.
# ------------------------------------------------------------

con <- dbConnect(
  RSQLite::SQLite(),
  db_path,
  flags = RSQLite::SQLITE_RO
)

dat <- tryCatch(
  dbReadTable(con, "processed_panama"),
  finally = dbDisconnect(con)
)

dat$datetime <- as.POSIXct(
  dat$datetime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

required_columns <- c("datetime", "nat_demand", features)

missing_columns <- setdiff(
  required_columns,
  names(dat)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

dat <- dat %>%
  arrange(datetime) %>%
  filter(
    !is.na(datetime),
    !is.na(nat_demand)
  )

# Drop rows with missing model inputs.
dat <- dat[
  complete.cases(dat[, features, drop = FALSE]),
]

# ------------------------------------------------------------
# 3. DEFINE INITIAL DATA, NEW BATCHES AND TEST PERIOD
# ------------------------------------------------------------

initial_data <- dat %>%
  filter(datetime < as.POSIXct(
    "2019-01-01 00:00:00", tz = "UTC"
  ))

batch_1 <- dat %>%
  filter(
    datetime >= as.POSIXct(
      "2019-01-01 00:00:00", tz = "UTC"
    ),
    datetime < as.POSIXct(
      "2019-07-01 00:00:00", tz = "UTC"
    )
  )

batch_2 <- dat %>%
  filter(
    datetime >= as.POSIXct(
      "2019-07-01 00:00:00", tz = "UTC"
    ),
    datetime < as.POSIXct(
      "2020-01-01 00:00:00", tz = "UTC"
    )
  )

test_data <- dat %>%
  filter(
    datetime >= as.POSIXct(
      "2020-01-01 00:00:00", tz = "UTC"
    )
  )

if (
  nrow(initial_data) == 0 ||
  nrow(batch_1) == 0 ||
  nrow(batch_2) == 0 ||
  nrow(test_data) == 0
) {
  stop("One or more date partitions are empty.")
}

cat("\nDATA PARTITIONS\n")
cat("Initial training:", nrow(initial_data), "rows\n")
cat("New batch 1:", nrow(batch_1), "rows\n")
cat("New batch 2:", nrow(batch_2), "rows\n")
cat("Fixed test set:", nrow(test_data), "rows\n")

# ------------------------------------------------------------
# 4. TRAIN AND EVALUATE A TEMPORARY MODEL
# ------------------------------------------------------------

train_rf <- function(training_data) {
  
  model_data <- training_data %>%
    select(nat_demand, all_of(features))
  
  ranger::ranger(
    dependent.variable.name = "nat_demand",
    data = model_data,
    num.trees = 300,
    seed = 42,
    num.threads = 2
  )
}

evaluate_rf <- function(model, test_data, model_version) {
  
  predictions <- predict(
    model,
    data = test_data[, features, drop = FALSE]
  )$predictions
  
  actual <- test_data$nat_demand
  errors <- actual - predictions
  
  data.frame(
    model_version = model_version,
    training_rows = model$num.samples,
    test_rows = length(actual),
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    MAPE = mean(
      abs(errors) / pmax(abs(actual), 1)
    ) * 100,
    R2 = if (length(actual) > 1 &&
             var(actual) > 0) {
      1 - sum(errors^2) /
        sum((actual - mean(actual))^2)
    } else {
      NA_real_
    }
  )
}

# ------------------------------------------------------------
# 5. SIMULATE NEW DATA ARRIVING IN BATCHES
# ------------------------------------------------------------

# Version 1: only the original training period
training_data <- initial_data

cat("\nTraining model version 1...\n")
model_v1 <- train_rf(training_data)

results <- list(
  evaluate_rf(model_v1, test_data, "Initial model")
)

# Version 2: batch 1 has arrived; retrain from accumulated data
training_data <- bind_rows(
  training_data,
  batch_1
) %>%
  arrange(datetime)

cat("\nBatch 1 arrived. Training model version 2...\n")
model_v2 <- train_rf(training_data)

results[[2]] <- evaluate_rf(
  model_v2,
  test_data,
  "After batch 1"
)

# Version 3: batch 2 has arrived; retrain again
training_data <- bind_rows(
  training_data,
  batch_2
) %>%
  arrange(datetime)

cat("\nBatch 2 arrived. Training model version 3...\n")
model_v3 <- train_rf(training_data)

results[[3]] <- evaluate_rf(
  model_v3,
  test_data,
  "After batch 2"
)

# ------------------------------------------------------------
# 6. COMPARE RESULTS
# ------------------------------------------------------------

results_table <- bind_rows(results)

cat("\n============================================\n")
cat("TEMPORARY MODEL UPDATE EXPERIMENT\n")
cat("============================================\n")

print(results_table, digits = 4, row.names = FALSE)

write.csv(
  results_table,
  file.path(experiment_dir, "model_update_comparison.csv"),
  row.names = FALSE
)

# Save models separately from production models.
saveRDS(
  list(
    initial_model = model_v1,
    after_batch_1 = model_v2,
    after_batch_2 = model_v3
  ),
  file.path(experiment_dir, "temporary_models.rds")
)

cat("\nExperiment complete.\n")
cat("Results saved in:\n", experiment_dir, "\n")
cat("Production models and database were not modified.\n")
