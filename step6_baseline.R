
# step6_baseline.R
# Compare recursive Random Forest against a weekly seasonal-naive baseline.

source("R/config.R")
source("R/forecasting.R")

# Load saved preprocessing and backtest results
ck <- readRDS("outputs/step1.rds")
df <- ck$feat
profile <- ck$profile

validation_backtest <- readRDS(
  "outputs/results/validation_backtest_rf.rds"
)

dt_col <- profile$datetime_col
y_col <- profile$demand_col

# Ensure timestamps are ordered and comparable
df[[dt_col]] <- as.POSIXct(df[[dt_col]], tz = "UTC")
df <- df[order(df[[dt_col]]), ]

bt <- validation_backtest$predictions
bt$datetime <- as.POSIXct(bt$datetime, tz = "UTC")
bt$origin <- as.POSIXct(bt$origin, tz = "UTC")

# Seasonal-naive forecast: demand from exactly 168 hours earlier
past_time <- bt$datetime - 168 * 3600

past_idx <- match(
  as.numeric(past_time),
  as.numeric(df[[dt_col]])
)

if (anyNA(past_idx)) {
  stop("Some weekly reference timestamps were not found.")
}

bt$naive_prediction <- df[[y_col]][past_idx]

if (anyNA(bt$naive_prediction)) {
  stop("The seasonal-naive predictions contain missing values.")
}

# Metrics for each origin and horizon
groups <- split(
  bt,
  interaction(bt$origin, bt$horizon, drop = TRUE)
)

baseline_metrics <- do.call(
  rbind,
  lapply(groups, function(x) {
    m <- forecast_metrics(
      actual = x$actual_demand,
      predicted = x$naive_prediction
    )
    
    data.frame(
      origin = x$origin[1],
      horizon = x$horizon[1],
      MAE = as.numeric(m["MAE"]),
      RMSE = as.numeric(m["RMSE"]),
      MAPE = as.numeric(m["MAPE"]),
      R2 = as.numeric(m["R2"])
    )
  })
)

rownames(baseline_metrics) <- NULL

# Average metrics across origins for each horizon
baseline_summary <- do.call(
  rbind,
  lapply(
    split(baseline_metrics, baseline_metrics$horizon),
    function(x) {
      data.frame(
        horizon = x$horizon[1],
        origins = nrow(x),
        mean_MAE = mean(x$MAE, na.rm = TRUE),
        mean_RMSE = mean(x$RMSE, na.rm = TRUE),
        mean_MAPE = mean(x$MAPE, na.rm = TRUE),
        mean_R2 = mean(x$R2, na.rm = TRUE)
      )
    }
  )
)

rownames(baseline_summary) <- NULL

# Rename columns explicitly to avoid ambiguous merge suffixes
rf_summary <- validation_backtest$summary_metrics

names(rf_summary)[names(rf_summary) == "mean_MAE"] <- "RF_MAE"
names(rf_summary)[names(rf_summary) == "mean_RMSE"] <- "RF_RMSE"
names(rf_summary)[names(rf_summary) == "mean_MAPE"] <- "RF_MAPE"
names(rf_summary)[names(rf_summary) == "mean_R2"] <- "RF_R2"

names(baseline_summary)[
  names(baseline_summary) == "mean_MAE"
] <- "Naive_MAE"
names(baseline_summary)[
  names(baseline_summary) == "mean_RMSE"
] <- "Naive_RMSE"
names(baseline_summary)[
  names(baseline_summary) == "mean_MAPE"
] <- "Naive_MAPE"
names(baseline_summary)[
  names(baseline_summary) == "mean_R2"
] <- "Naive_R2"

comparison <- merge(
  rf_summary,
  baseline_summary,
  by = "horizon",
  suffixes = c("_RF", "_Naive")
)

comparison <- comparison[order(comparison$horizon), ]

cat("\nRandom Forest vs Seasonal-Naive Baseline\n")
print(comparison, row.names = FALSE)

# Save the comparison and detailed baseline metrics
dir.create(
  "outputs/results",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  comparison,
  "outputs/results/rf_vs_seasonal_naive.csv",
  row.names = FALSE
)

write.csv(
  baseline_metrics,
  "outputs/results/seasonal_naive_origin_metrics.csv",
  row.names = FALSE
)

saveRDS(
  list(
    baseline_metrics = baseline_metrics,
    baseline_summary = baseline_summary,
    comparison = comparison
  ),
  "outputs/results/seasonal_naive_baseline.rds"
)

cat("\nSaved results to outputs/results/:\n")
cat("- rf_vs_seasonal_naive.csv\n")
cat("- seasonal_naive_origin_metrics.csv\n")
cat("- seasonal_naive_baseline.rds\n")
