
# R/backtesting.R

backtest_recursive_rf <- function(
    df,
    profile,
    feature_cols,
    horizons = c(24, 72, 144),
    origins,
    mtry = 7,
    min.node.size = 10,
    num.trees = 300
) {
  dt_col <- profile$datetime_col
  y_col <- profile$demand_col
  
  dt <- df[[dt_col]]
  df <- df[order(dt), , drop = FALSE]
  dt <- df[[dt_col]]
  
  if (anyDuplicated(dt) || anyNA(dt)) {
    stop("Backtesting requires unique, valid timestamps.")
  }
  
  if (any(diff(as.numeric(dt)) != 3600)) {
    stop("Backtesting requires consecutive hourly observations.")
  }
  
  if (anyNA(df[[y_col]])) {
    stop("Demand contains missing values.")
  }
  
  results <- list()
  metric_results <- list()
  result_idx <- 1L
  metric_idx <- 1L
  
  for (origin in origins) {
    origin <- as.POSIXct(origin, tz = "UTC")
    
    history_idx <- which(dt <= origin)
    
    if (length(history_idx) < 168) {
      warning("Skipping origin with insufficient history: ", origin)
      next
    }
    
    history <- df[history_idx, , drop = FALSE]
    
    # Fit using observations available at this origin only.
    fitted <- fit_recursive_rf(
      df = history,
      profile = profile,
      feature_cols = feature_cols,
      train_end = origin,
      mtry = mtry,
      min.node.size = min.node.size,
      num.trees = num.trees
    )
    
    for (h in horizons) {
      future_idx <- which(
        dt > origin &
          dt <= origin + h * 3600
      )
      
      if (length(future_idx) != h) {
        warning(
          "Skipping origin ", format(origin),
          " for horizon ", h,
          ": expected ", h,
          " future observations but found ", length(future_idx)
        )
        next
      }
      
      future <- df[future_idx, , drop = FALSE]
      
      future_calendar <- future[
        ,
        intersect(
          c(dt_col, "holiday_flag", "school_flag"),
          names(future)
        ),
        drop = FALSE
      ]
      
      forecast <- forecast_recursive(
        fitted,
        history,
        future_calendar
      )
      
      actual <- future[[y_col]]
      predicted <- forecast$predicted_demand
      
      results[[result_idx]] <- data.frame(
        origin = rep(origin, h),
        horizon = rep(h, h),
        horizon_hour = seq_len(h),
        datetime = future[[dt_col]],
        actual_demand = actual,
        predicted_demand = predicted
      )
      
      result_idx <- result_idx + 1L
      
      metric_results[[metric_idx]] <- data.frame(
        origin = origin,
        horizon = h,
        as.list(forecast_metrics(actual, predicted)),
        check.names = FALSE
      )
      
      metric_idx <- metric_idx + 1L
    }
  }
  
  if (length(results) == 0) {
    stop("No valid backtest forecasts were produced.")
  }
  
  predictions <- do.call(rbind, results)
  origin_metrics <- do.call(rbind, metric_results)
  
  # Average performance across forecast origins for each horizon.
  summary_metrics <- do.call(
    rbind,
    lapply(split(origin_metrics, origin_metrics$horizon), function(x) {
      data.frame(
        horizon = x$horizon[1],
        origins = nrow(x),
        mean_MAE = mean(x$MAE, na.rm = TRUE),
        mean_RMSE = mean(x$RMSE, na.rm = TRUE),
        mean_MAPE = mean(x$MAPE, na.rm = TRUE),
        mean_R2 = mean(x$R2, na.rm = TRUE)
      )
    })
  )
  
  rownames(summary_metrics) <- NULL
  
  list(
    predictions = predictions,
    origin_metrics = origin_metrics,
    summary_metrics = summary_metrics
  )
}
