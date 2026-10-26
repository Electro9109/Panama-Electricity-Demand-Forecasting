
# step5_backtest.R

source("R/config.R")
source("R/features.R")
source("R/forecasting.R")
source("R/backtesting.R")

dir.create(
  "outputs/results",
  recursive = TRUE,
  showWarnings = FALSE
)

ck <- readRDS("outputs/step1.rds")

df <- ck$feat
profile <- ck$profile

feature_cols <- intersect(
  get_forecast_features(),
  names(df)
)

# Six monthly validation origins.
origins <- as.POSIXct(
  c(
    "2019-01-01 00:00:00",
    "2019-03-01 00:00:00",
    "2019-05-01 00:00:00",
    "2019-07-01 00:00:00",
    "2019-09-01 00:00:00",
    "2019-11-01 00:00:00"
  ),
  tz = "UTC"
)

# The original validation period ends on 2019-12-31.
# All horizons below fit within that validation period.
t0 <- Sys.time()

validation_backtest <- backtest_recursive_rf(
  df = df,
  profile = profile,
  feature_cols = feature_cols,
  horizons = c(24, 72, 144),
  origins = origins,
  mtry = 7,
  min.node.size = 10,
  num.trees = 300
)

cat("\nValidation results by horizon:\n")
print(validation_backtest$summary_metrics)

cat("\nResults by individual origin:\n")
print(validation_backtest$origin_metrics)

saveRDS(
  validation_backtest,
  "outputs/results/validation_backtest_rf.rds"
)

write.csv(
  validation_backtest$summary_metrics,
  "outputs/results/validation_backtest_rf_summary.csv",
  row.names = FALSE
)

write.csv(
  validation_backtest$predictions,
  "outputs/results/validation_backtest_rf_predictions.csv",
  row.names = FALSE
)

cat(
  "\nBacktesting completed in",
  round(as.numeric(Sys.time() - t0, units = "mins"), 2),
  "minutes.\n"
)
