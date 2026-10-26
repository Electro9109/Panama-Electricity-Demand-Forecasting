
# R/forecasting.R
# Recursive multi-horizon electricity-demand forecasting.
# Requires R/database.R, R/config.R, R/data_prep.R,
# R/features.R, and the ranger package.

# These features can be calculated at a future timestamp without
# knowing the actual future demand or weather.
get_forecast_features <- function() {
  c(
    "lag_1", "lag_24", "lag_168",
    "roll_mean", "roll_sd",
    "hour_sin", "hour_cos",
    "month_sin", "month_cos",
    "is_weekend", "holiday_flag", "school_flag"
  )
}


# Fit the one-hour-ahead model using only observations available
# before the specified training cutoff.
fit_recursive_rf <- function(
    df,
    profile,
    feature_cols = get_forecast_features(),
    train_end,
    mtry = 7,
    min.node.size = 10,
    num.trees = 300
) {
  missing_cols <- setdiff(
    c(profile$datetime_col, profile$demand_col, feature_cols),
    names(df)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  cutoff <- as.POSIXct(train_end, tz = "UTC")
  dt <- df[[profile$datetime_col]]
  
  train <- df[
    !is.na(dt) & dt <= cutoff,
    ,
    drop = FALSE
  ]
  
  train <- train[
    complete.cases(
      train[, c(feature_cols, profile$demand_col), drop = FALSE]
    ),
    ,
    drop = FALSE
  ]
  
  if (nrow(train) == 0) {
    stop("No complete training rows available.")
  }
  
  if (length(unique(train[[profile$demand_col]])) < 2) {
    stop("Training demand must contain at least two distinct values.")
  }
  
  # Remove features that have no variation in the training data.
  variable_cols <- feature_cols[
    vapply(
      train[, feature_cols, drop = FALSE],
      function(x) length(unique(x)) > 1,
      logical(1)
    )
  ]
  
  if (length(variable_cols) == 0) {
    stop("No varying features remain in the training data.")
  }
  
  formula <- stats::reformulate(
    variable_cols,
    response = profile$demand_col
  )
  
  fit <- ranger::ranger(
    formula,
    data = train,
    num.trees = num.trees,
    mtry = min(mtry, length(variable_cols)),
    min.node.size = min.node.size,
    splitrule = "variance",
    importance = "impurity",
    seed = 42
  )
  
  list(
    model = fit,
    feature_cols = variable_cols,
    train_end = cutoff,
    training_rows = nrow(train),
    profile = profile
  )
}


# Generate a forecast from one origin.
#
# history: observations available at the forecast origin.
# future_calendar: future timestamps and known calendar features.
#
# Actual future demand and weather are deliberately not used.
forecast_recursive <- function(
    fitted,
    history,
    future_calendar
) {
  profile <- fitted$profile
  dt_col <- profile$datetime_col
  y_col <- profile$demand_col
  features <- fitted$feature_cols
  
  required_history <- c(dt_col, y_col)
  
  if (!all(required_history %in% names(history))) {
    stop("History must contain datetime and demand columns.")
  }
  
  if (!dt_col %in% names(future_calendar)) {
    stop("Future calendar must contain the datetime column.")
  }
  
  if (nrow(future_calendar) == 0) {
    stop("Future calendar contains no timestamps.")
  }
  
  history <- history[order(history[[dt_col]]), , drop = FALSE]
  future_calendar <- future_calendar[
    order(future_calendar[[dt_col]]),
    ,
    drop = FALSE
  ]
  
  history_times <- as.numeric(history[[dt_col]])
  future_times <- as.numeric(future_calendar[[dt_col]])
  
  if (anyNA(history_times) || anyNA(future_times)) {
    stop("Invalid timestamps found.")
  }
  
  if (anyDuplicated(history_times) || anyDuplicated(future_times)) {
    stop("Duplicate timestamps found.")
  }
  
  if (any(diff(history_times) <= 0) ||
      any(diff(future_times) <= 0)) {
    stop("Timestamps must be strictly increasing.")
  }
  
  if (min(future_times) <= max(history_times)) {
    stop("Forecast timestamps must be later than the forecast origin.")
  }
  
  if (length(history_times) < 168) {
    stop("At least 168 historical hourly observations are required.")
  }
  
  if (anyNA(history[[y_col]])) {
    stop("Historical demand contains missing values.")
  }
  
  # Verify continuity across the forecast boundary and within
  # the requested future period.
  combined_times <- c(
    tail(history_times, 1),
    future_times
  )
  
  if (any(abs(diff(combined_times) - 3600) > 1)) {
    stop("Forecast requires consecutive hourly timestamps.")
  }
  
  # Future calendar fields must be known in advance.
  calendar_features <- intersect(
    c("holiday_flag", "school_flag"),
    features
  )
  
  missing_calendar <- setdiff(
    calendar_features,
    names(future_calendar)
  )
  
  if (length(missing_calendar) > 0) {
    stop(
      "Future calendar is missing: ",
      paste(missing_calendar, collapse = ", ")
    )
  }
  
  # Use the historical demand vector to build recursive lags.
  y_history <- as.numeric(history[[y_col]])
  predictions <- numeric(nrow(future_calendar))
  
  for (i in seq_len(nrow(future_calendar))) {
    dt <- future_calendar[[dt_col]][i]
    
    hour <- as.integer(format(dt, "%H", tz = "UTC"))
    dow <- as.integer(format(dt, "%u", tz = "UTC"))
    month <- as.integer(format(dt, "%m", tz = "UTC"))
    
    n <- length(y_history)
    
    row <- list(
      lag_1 = y_history[n],
      lag_24 = y_history[n - 23],
      lag_168 = y_history[n - 167],
      roll_mean = mean(tail(y_history, 24)),
      roll_sd = stats::sd(tail(y_history, 24)),
      hour_sin = sin(2 * pi * hour / 24),
      hour_cos = cos(2 * pi * hour / 24),
      month_sin = sin(2 * pi * month / 12),
      month_cos = cos(2 * pi * month / 12),
      is_weekend = as.integer(dow %in% c(6, 7))
    )
    
    for (nm in calendar_features) {
      row[[nm]] <- future_calendar[[nm]][i]
    }
    
    newdata <- as.data.frame(row)
    newdata <- newdata[, features, drop = FALSE]
    
    pred <- as.numeric(
      predict(fitted$model, data = newdata)$predictions
    )[1]
    
    if (!is.finite(pred)) {
      stop("Model produced an invalid prediction at step ", i)
    }
    
    predictions[i] <- pred
    
    # Feed the prediction into the next step, not actual future demand.
    y_history <- c(y_history, pred)
  }
  
  data.frame(
    datetime = future_calendar[[dt_col]],
    horizon_hour = seq_len(nrow(future_calendar)),
    predicted_demand = predictions
  )
}


# Calculate evaluation metrics safely.
forecast_metrics <- function(actual, predicted) {
  valid <- is.finite(actual) & is.finite(predicted)
  
  actual <- actual[valid]
  predicted <- predicted[valid]
  
  if (length(actual) == 0) {
    stop("No valid observations for metric calculation.")
  }
  
  error <- actual - predicted
  
  nonzero <- actual != 0
  
  c(
    MAE = mean(abs(error)),
    RMSE = sqrt(mean(error^2)),
    MAPE = if (any(nonzero)) {
      mean(abs(error[nonzero] / actual[nonzero])) * 100
    } else {
      NA_real_
    },
    R2 = if (length(actual) > 1 && stats::var(actual) > 0) {
      1 - sum(error^2) /
        sum((actual - mean(actual))^2)
    } else {
      NA_real_
    }
  )
}
