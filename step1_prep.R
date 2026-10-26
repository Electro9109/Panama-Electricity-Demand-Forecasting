
# step1_prep.R
# Prepare data and select features using training data only.

source("R/config.R")
source("R/database.R")
source("R/data_prep.R")
source("R/features.R")
source("R/models.R")

dir.create("outputs", recursive = TRUE, showWarnings = FALSE)

t0 <- Sys.time()

tick <- function(label) {
  cat(sprintf(
    "[%s] %s (%.1fs)\n",
    format(Sys.time(), "%H:%M:%S"),
    label,
    as.numeric(Sys.time() - t0, units = "secs")
  ))
}

# 1. Load existing raw data from SQLite.
profile <- panama_profile

raw <- load_and_store(
  profile,
  overwrite = FALSE
)

tick("Raw data loaded")

# 2. Validate the time series.
dt <- raw[[profile$datetime_col]]

if (anyNA(dt)) {
  stop("Datetime contains missing or invalid values.")
}

if (anyDuplicated(dt)) {
  stop("Duplicate timestamps detected.")
}

if (!all(diff(as.numeric(dt)) > 0)) {
  stop("Timestamps must be strictly increasing.")
}

if (profile$frequency == "hourly" &&
    any(diff(as.numeric(dt)) != 3600)) {
  stop("Hourly dataset contains missing or irregular intervals.")
}

if (anyNA(raw[[profile$demand_col]])) {
  stop("Demand column contains missing values.")
}

tick("Time-series validation passed")

# 3. Engineer past-only features.
feat <- engineer_features(raw, profile)

tick("Features engineered")

# 4. Isolate the training period BEFORE feature selection.
train_end <- as.POSIXct(profile$train_end, tz = "UTC")

train_feat <- feat[
  feat[[profile$datetime_col]] <= train_end,
  ,
  drop = FALSE
]

if (nrow(train_feat) == 0) {
  stop("No training observations found.")
}

tick("Training period isolated")

# 5. Select weather features using training data only.
weather_keep <- prune_redundant_weather(
  train_feat,
  profile,
  corr_threshold = 0.90
)

engineered_extra <- c(
  "hour_sin", "hour_cos",
  "month_sin", "month_cos",
  "is_weekend",
  paste0("lag_", profile$lag_steps),
  "roll_mean", "roll_sd"
)

if (!is.null(profile$holiday_col)) {
  engineered_extra <- c(engineered_extra, "holiday_flag")
}

if (!is.null(profile$school_col)) {
  engineered_extra <- c(engineered_extra, "school_flag")
}

candidate_cols <- intersect(
  c(weather_keep, engineered_extra),
  names(train_feat)
)

candidate_cols <- candidate_cols[
  vapply(train_feat[candidate_cols], is.numeric, logical(1))
]

if (length(candidate_cols) == 0) {
  stop("No numeric candidate features found.")
}

cat("\nCandidate features:\n")
print(candidate_cols)

# 6. Fit feature selector using training observations only.
final_features <- select_features(
  train_feat,
  profile,
  candidate_cols,
  top_n = min(15, length(candidate_cols))
)

if (length(final_features) == 0) {
  stop("Feature selection returned no features.")
}

cat("\nSelected features:\n")
print(final_features)

tick("Leakage-safe feature selection complete")

# 7. Store processed records and save a checkpoint.
store_processed(feat, profile, overwrite = TRUE)

saveRDS(
  list(
    feat = feat,
    final_features = final_features,
    profile = profile
  ),
  "outputs/step1.rds"
)

tick("Processed data and checkpoint saved")
