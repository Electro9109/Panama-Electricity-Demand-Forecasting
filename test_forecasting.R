
source("R/config.R")
source("R/database.R")
source("R/data_prep.R")
source("R/features.R")
source("R/forecasting.R")

ck <- readRDS("outputs/step1.rds")

df <- ck$feat
profile <- ck$profile

dt <- df[[profile$datetime_col]]

# Only features that can be generated without future weather.
forecast_features <- intersect(
  get_forecast_features(),
  names(df)
)

# Fit through the existing training cutoff.
fitted <- fit_recursive_rf(
  df = df,
  profile = profile,
  feature_cols = forecast_features,
  train_end = profile$train_end,
  mtry = 7,
  min.node.size = 10
)

cat("Training rows:", fitted$training_rows, "\n")
cat("Features used:\n")
print(fitted$feature_cols)

# Use history through the training cutoff.
origin <- as.POSIXct(profile$train_end, tz = "UTC")

history <- df[
  dt <= origin,
  ,
  drop = FALSE
]

# Use timestamps and calendar information only for the next 24 hours.
future_rows <- which(
  dt > origin &
    dt <= origin + 24 * 3600
)

future_calendar <- df[
  future_rows,
  intersect(
    c(
      profile$datetime_col,
      "holiday_flag",
      "school_flag"
    ),
    names(df)
  ),
  drop = FALSE
]

forecast_24h <- forecast_recursive(
  fitted,
  history,
  future_calendar
)

print(forecast_24h)

cat("\nForecast summary:\n")
print(summary(forecast_24h$predicted_demand))
