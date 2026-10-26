library(zoo)

engineer_features <- function(df, profile) {
  dt <- df[[profile$datetime_col]]
  
  df$hour       <- as.integer(format(dt, "%H"))
  df$dow        <- as.integer(format(dt, "%u"))
  df$month      <- as.integer(format(dt, "%m"))
  df$is_weekend <- as.integer(df$dow %in% c(6, 7))
  
  df$hour_sin  <- sin(2 * pi * df$hour / 24)
  df$hour_cos  <- cos(2 * pi * df$hour / 24)
  df$month_sin <- sin(2 * pi * df$month / 12)
  df$month_cos <- cos(2 * pi * df$month / 12)
  
  if (!is.null(profile$holiday_col) && profile$holiday_col %in% names(df)) {
    df$holiday_flag <- df[[profile$holiday_col]]
  }
  if (!is.null(profile$school_col) && profile$school_col %in% names(df)) {
    df$school_flag <- df[[profile$school_col]]
  }
  
  y <- df[[profile$demand_col]]
  for (L in profile$lag_steps) {
    df[[paste0("lag_", L)]] <- c(rep(NA, L), head(y, -L))
  }
  
  win <- profile$seasonal_period
  rm_raw <- rollapply(y, width = win, FUN = mean, align = "right", fill = NA)
  rs_raw <- rollapply(y, width = win, FUN = sd,   align = "right", fill = NA)
  df$roll_mean <- c(NA, head(rm_raw, -1))
  df$roll_sd   <- c(NA, head(rs_raw, -1))
  
  df
}

prune_redundant_weather <- function(df, profile, corr_threshold = 0.90) {
  all_weather_cols <- unlist(profile$weather_stations, use.names = FALSE)
  all_weather_cols <- intersect(all_weather_cols, names(df))
  if (length(all_weather_cols) < 2) return(all_weather_cols)
  
  cm <- suppressWarnings(cor(df[all_weather_cols], use = "pairwise.complete.obs"))
  to_drop <- character(0)
  for (i in seq_len(ncol(cm))) {
    for (j in seq_len(i - 1)) {
      if (!is.na(cm[i, j]) && abs(cm[i, j]) > corr_threshold) {
        to_drop <- union(to_drop, colnames(cm)[i])
      }
    }
  }
  setdiff(all_weather_cols, to_drop)
}

select_features <- function(df, profile, candidate_cols, top_n = 15,
                            sample_n = 8000, ntree = 150) {
  library(randomForest)
  complete <- df[stats::complete.cases(df[, c(candidate_cols, profile$demand_col)]), ]
  if (nrow(complete) > sample_n) {
    set.seed(1)
    complete <- complete[sample(nrow(complete), sample_n), ]
  }
  rf <- randomForest(
    x = complete[, candidate_cols],
    y = complete[[profile$demand_col]],
    ntree = ntree,
    importance = TRUE,
    nodesize = 10
  )
  imp <- importance(rf)[, 1]
  names(sort(imp, decreasing = TRUE))[seq_len(min(top_n, length(imp)))]
}