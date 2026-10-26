make_country_profile <- function(name,
                                 csv_path,
                                 datetime_col,
                                 demand_col,
                                 frequency = c("hourly", "daily"),
                                 weather_stations = list(),
                                 holiday_col = NULL,
                                 school_col = NULL,
                                 seasonal_period,
                                 lag_steps,
                                 train_end,
                                 val_end) {
  frequency <- match.arg(frequency)
  structure(
    list(
      name = name,
      csv_path = csv_path,
      datetime_col = datetime_col,
      demand_col = demand_col,
      frequency = frequency,
      weather_stations = weather_stations,
      holiday_col = holiday_col,
      school_col = school_col,
      seasonal_period = seasonal_period,
      lag_steps = lag_steps,
      train_end = train_end,
      val_end = val_end
    ),
    class = "country_profile"
  )
}

panama_profile <- make_country_profile(
  name             = "Panama",
  csv_path         = "data/panama.csv",
  datetime_col     = "datetime",
  demand_col       = "nat_demand",
  frequency        = "hourly",
  weather_stations = list(
    toc = c("T2M_toc", "QV2M_toc", "TQL_toc", "W2M_toc"),
    san = c("T2M_san", "QV2M_san", "TQL_san", "W2M_san"),
    dav = c("T2M_dav", "QV2M_dav", "TQL_dav", "W2M_dav")
  ),
  holiday_col      = "holiday",
  school_col       = "school",
  seasonal_period  = 24,
  lag_steps        = c(1, 24, 168),
  train_end        = "2018-12-31 23:00:00",
  val_end          = "2019-12-31 23:00:00"
)