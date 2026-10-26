
# ============================================================
# PANAMA ELECTRICITY DEMAND ANALYTICS
# Interactive, Tableau-inspired visualisation dashboard
# Shiny + Plotly + SQLite
#
# READ-ONLY:
#   - Does not modify SQLite data
#   - Does not retrain models
#   - Does not overwrite saved results
# ============================================================

# ------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "shiny", "bslib", "plotly", "DT", "DBI", "RSQLite",
  "dplyr", "tidyr", "lubridate", "scales"
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
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them using:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

# ------------------------------------------------------------
# 2. PROJECT PATHS
# ------------------------------------------------------------

project_dir <- "C:/Users/ASUS/Coding/R/Panama"

db_path <- file.path(project_dir, "outputs/pipeline.sqlite")

pred_path <- file.path(
  project_dir,
  "outputs/results/validation_backtest_rf_predictions.csv"
)
comparison_path <- file.path(
  project_dir,
  "outputs/panama_model_comparison.csv"
)

if (!file.exists(db_path)) {
  stop("SQLite database not found: ", db_path)
}

# ------------------------------------------------------------
# 3. SAFE DATETIME PARSER
# Avoids lubridate::parse_date_time() and its regex issue.
# ------------------------------------------------------------

parse_datetime <- function(x, tz = "UTC") {
  
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  }
  
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  }
  
  # Handle numeric Unix timestamps if present.
  if (is.numeric(x)) {
    finite_values <- x[is.finite(x)]
    
    if (
      length(finite_values) > 0 &&
      all(finite_values > 1e8 & finite_values < 1e11)
    ) {
      return(as.POSIXct(x, origin = "1970-01-01", tz = tz))
    }
    
    return(as.POSIXct(
      rep(NA_real_, length(x)),
      origin = "1970-01-01",
      tz = tz
    ))
  }
  
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  result <- as.POSIXct(
    rep(NA_real_, length(x)),
    origin = "1970-01-01",
    tz = tz
  )
  
  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%m/%d/%Y",
    "%d/%m/%Y"
  )
  
  for (fmt in formats) {
    
    missing <- is.na(result) & !is.na(x)
    
    if (!any(missing)) {
      break
    }
    
    parsed <- suppressWarnings(
      as.POSIXct(
        x[missing],
        format = fmt,
        tz = tz
      )
    )
    
    result[missing] <- parsed
  }
  
  # ISO timestamps ending with Z or a numeric UTC offset.
  # Remove the timezone suffix only if ordinary formats failed.
  missing <- is.na(result) & !is.na(x)
  
  if (any(missing)) {
    
    candidate <- x[missing]
    
    candidate <- sub("Z$", "", candidate)
    candidate <- sub("[+-][0-9]{2}:[0-9]{2}$", "", candidate)
    
    parsed <- suppressWarnings(
      as.POSIXct(
        candidate,
        format = "%Y-%m-%dT%H:%M:%OS",
        tz = tz
      )
    )
    
    result[missing] <- parsed
  }
  
  result
}

# ------------------------------------------------------------
# 4. REUSABLE HELPERS
# ------------------------------------------------------------

format_number <- function(x, accuracy = 0.1) {
  
  if (
    length(x) == 0 ||
    is.na(x[1]) ||
    !is.finite(x[1])
  ) {
    return("N/A")
  }
  
  scales::number(
    x[1],
    accuracy = accuracy,
    big.mark = ","
  )
}

empty_plot <- function(message) {
  
  plot_ly() |>
    layout(
      annotations = list(
        text = message,
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 15, color = "#64748b")
      ),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      paper_bgcolor = "white",
      plot_bgcolor = "white"
    ) |>
    config(displaylogo = FALSE, responsive = TRUE)
}

style_plot <- function(
    p,
    title = NULL,
    x_title = NULL,
    y_title = NULL
) {
  
  p |>
    layout(
      title = list(
        text = title,
        x = 0.02,
        xanchor = "left"
      ),
      xaxis = list(
        title = x_title,
        gridcolor = "#edf0f5",
        automargin = TRUE
      ),
      yaxis = list(
        title = y_title,
        gridcolor = "#edf0f5",
        automargin = TRUE
      ),
      paper_bgcolor = "white",
      plot_bgcolor = "white",
      font = list(
        family = "Arial",
        color = "#263244"
      ),
      margin = list(
        l = 65,
        r = 25,
        t = 65,
        b = 65
      ),
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.2
      )
    ) |>
    config(
      displaylogo = FALSE,
      responsive = TRUE
    )
}

make_card <- function(label, value, note = "") {
  
  div(
    class = "metric-card",
    div(class = "metric-label", label),
    div(class = "metric-value", value),
    div(class = "metric-note", note)
  )
}

# ------------------------------------------------------------
# 5. LOAD HISTORICAL DATA FROM SQLITE
# ------------------------------------------------------------

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = db_path
)

historical <- tryCatch(
  {
    tables <- DBI::dbListTables(con)
    
    if (!"processed_panama" %in% tables) {
      stop(
        "Table processed_panama was not found. Available tables: ",
        paste(tables, collapse = ", ")
      )
    }
    
    DBI::dbReadTable(con, "processed_panama")
  },
  finally = {
    DBI::dbDisconnect(con)
  }
)

if (!all(c("datetime", "nat_demand") %in% names(historical))) {
  stop(
    "The processed_panama table must contain datetime and nat_demand.\n",
    "Available columns: ",
    paste(names(historical), collapse = ", ")
  )
}

historical$datetime <- parse_datetime(historical$datetime)

historical$nat_demand <- suppressWarnings(
  as.numeric(as.character(historical$nat_demand))
)

historical <- historical |>
  filter(
    !is.na(datetime),
    is.finite(nat_demand)
  ) |>
  arrange(datetime) |>
  mutate(
    date = as.Date(datetime),
    hour = as.integer(format(datetime, "%H")),
    year = as.integer(format(datetime, "%Y")),
    month_number = as.integer(format(datetime, "%m")),
    weekday_number = as.integer(format(datetime, "%u")),
    weekday = factor(
      weekdays(datetime),
      levels = c(
        "Monday", "Tuesday", "Wednesday", "Thursday",
        "Friday", "Saturday", "Sunday"
      )
    ),
    month = factor(
      month.abb[month_number],
      levels = month.abb
    )
  )

if (nrow(historical) == 0) {
  stop(
    "No valid historical rows remain after datetime parsing. ",
    "Inspect the datetime values in processed_panama."
  )
}

date_min <- min(historical$date, na.rm = TRUE)
date_max <- max(historical$date, na.rm = TRUE)

weather_cols <- grep(
  "^(T2M|QV2M|TQL|W2M)_",
  names(historical),
  value = TRUE
)

weather_cols <- weather_cols[
  vapply(
    historical[weather_cols],
    is.numeric,
    logical(1)
  )
]

message("Historical records loaded: ", nrow(historical))
message("Historical date range: ", date_min, " to ", date_max)

# ------------------------------------------------------------
# 6. LOAD SAVED FORECAST PREDICTIONS
# ------------------------------------------------------------

predictions <- data.frame()

if (file.exists(pred_path)) {
  
  predictions <- tryCatch(
    read.csv(
      pred_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) {
      warning("Could not read forecast CSV: ", conditionMessage(e))
      data.frame()
    }
  )
}

rename_if_needed <- function(data, target, alternatives) {
  
  if (target %in% names(data)) {
    return(data)
  }
  
  matches <- intersect(alternatives, names(data))
  
  if (length(matches) > 0) {
    names(data)[names(data) == matches[1]] <- target
  }
  
  data
}

if (nrow(predictions) > 0) {
  
  predictions <- rename_if_needed(
    predictions,
    "datetime",
    c("timestamp", "date_time", "ds", "date")
  )
  
  predictions <- rename_if_needed(
    predictions,
    "actual_demand",
    c("actual", "actuals", "nat_demand", "y_true")
  )
  
  predictions <- rename_if_needed(
    predictions,
    "predicted_demand",
    c("predicted", "prediction", "forecast", "y_pred")
  )
  
  required_prediction_cols <- c(
    "datetime", "actual_demand", "predicted_demand"
  )
  
  if (all(required_prediction_cols %in% names(predictions))) {
    
    predictions$datetime <- parse_datetime(predictions$datetime)
    
    predictions$actual_demand <- suppressWarnings(
      as.numeric(as.character(predictions$actual_demand))
    )
    
    predictions$predicted_demand <- suppressWarnings(
      as.numeric(as.character(predictions$predicted_demand))
    )
    
    predictions <- predictions |>
      filter(
        is.finite(actual_demand),
        is.finite(predicted_demand)
      ) |>
      mutate(
        error = actual_demand - predicted_demand,
        abs_error = abs(error),
        date = as.Date(datetime)
      )
    
    if (!"horizon_hour" %in% names(predictions)) {
      
      if ("horizon" %in% names(predictions)) {
        predictions$horizon_hour <- suppressWarnings(
          as.numeric(as.character(predictions$horizon))
        )
      } else {
        predictions$horizon_hour <- NA_real_
      }
      
    } else {
      
      predictions$horizon_hour <- suppressWarnings(
        as.numeric(as.character(predictions$horizon_hour))
      )
    }
    
    if (!"origin" %in% names(predictions)) {
      predictions$origin <- "All origins"
    }
    
    predictions$origin <- as.character(predictions$origin)
    
  } else {
    
    warning(
      "Forecast CSV is missing recognisable datetime, ",
      "actual_demand, or predicted_demand columns."
    )
    
    predictions <- data.frame()
  }
}

message("Forecast predictions loaded: ", nrow(predictions))

# ------------------------------------------------------------
# 7. LOAD MODEL COMPARISON RESULTS
# ------------------------------------------------------------

comparison <- data.frame()

if (file.exists(comparison_path)) {
  
  comparison <- tryCatch(
    read.csv(
      comparison_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) {
      warning(
        "Could not read model comparison CSV: ",
        conditionMessage(e)
      )
      data.frame()
    }
  )
}

# ------------------------------------------------------------
# 8. USER INTERFACE
# ------------------------------------------------------------

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2457c5",
  secondary = "#64748b",
  bg = "#f5f7fb",
  fg = "#1e293b"
)

ui <- page_sidebar(
  
  title = div(
    class = "brand",
    span(class = "brand-mark", "P"),
    span("PANAMA", class = "brand-name"),
    span(" / DEMAND ANALYTICS", class = "brand-subtitle")
  ),
  
  theme = theme,
  fillable = TRUE,
  
  sidebar = sidebar(
    width = 285,
    open = "desktop",
    
    h5("ANALYSIS CONTROLS"),
    
    p(
      "Filters update the historical views.",
      class = "sidebar-note"
    ),
    
    dateRangeInput(
      "date_range",
      "Historical date range",
      start = date_min,
      end = date_max,
      min = date_min,
      max = date_max,
      format = "yyyy-mm-dd",
      separator = " to "
    ),
    
    selectInput(
      "aggregation",
      "Trend aggregation",
      choices = c(
        "Hourly" = "hour",
        "Daily" = "day",
        "Weekly" = "week",
        "Monthly" = "month"
      ),
      selected = "day"
    ),
    
    selectInput(
      "weekday_filter",
      "Day of week",
      choices = c(
        "All days",
        levels(historical$weekday)
      ),
      selected = "All days"
    ),
    
    selectInput(
      "year_filter",
      "Year",
      choices = c(
        "All years",
        sort(unique(historical$year))
      ),
      selected = "All years"
    ),
    
    if (length(weather_cols) > 0) {
      selectInput(
        "weather_variable",
        "Weather variable",
        choices = weather_cols,
        selected = weather_cols[1]
      )
    },
    
    hr(),
    
    actionButton(
      "reset_filters",
      "Reset filters",
      class = "btn-outline-primary w-100"
    ),
    
    hr(),
    
    div(
      class = "sidebar-foot",
      strong("Historical source"),
      p("SQLite: processed_panama"),
      strong("Forecast source"),
      p("Saved validation predictions"),
      span("Read-only mode", class = "readonly-tag")
    )
  ),
  
  tags$head(
    tags$style(HTML("
      body { background: #f5f7fb; }
      .navbar { box-shadow: 0 2px 10px rgba(15,23,42,.05); }
      .brand { display:flex; align-items:center; gap:10px; }
      .brand-mark {
        display:inline-flex; align-items:center; justify-content:center;
        background:#2457c5; color:white; width:31px; height:31px;
        border-radius:9px; font-weight:800;
      }
      .brand-name { font-weight:800; letter-spacing:1.2px; }
      .brand-subtitle { color:#64748b; font-size:12px; }
      .sidebar-note { font-size:12px; color:#64748b; }
      .sidebar-foot { font-size:12px; color:#64748b; }
      .readonly-tag {
        display:inline-block; margin-top:5px; padding:5px 8px;
        border-radius:6px; background:#e7f6ed; color:#167347;
        font-weight:600;
      }
      .page-heading { margin:8px 0 20px; }
      .page-heading h2 { font-size:25px; font-weight:750; margin-bottom:5px; }
      .page-heading p { color:#64748b; margin:0; }
      .metric-grid {
        display:grid; grid-template-columns:repeat(4,minmax(0,1fr));
        gap:14px; margin-bottom:18px;
      }
      .metric-card {
        background:white; border:1px solid #e8edf4; border-radius:12px;
        padding:18px; min-height:115px;
        box-shadow:0 3px 12px rgba(15,23,42,.025);
      }
      .metric-label {
        color:#64748b; font-size:11px; font-weight:700;
        text-transform:uppercase; letter-spacing:.6px;
      }
      .metric-value { font-size:25px; font-weight:750; margin-top:8px; }
      .metric-note { font-size:11px; color:#94a3b8; margin-top:4px; }
      .insight {
        padding:13px 16px; background:#eef4ff;
        border-left:4px solid #2457c5; border-radius:7px;
        margin-bottom:16px; color:#334155;
      }
      .nav-tabs { margin-bottom:18px; }
      @media (max-width:900px) {
        .metric-grid { grid-template-columns:repeat(2,minmax(0,1fr)); }
      }
      @media (max-width:500px) {
        .metric-grid { grid-template-columns:1fr; }
      }
    "))
  ),
  
  navset_tab(
    id = "main_tabs",
    
    # --------------------------------------------------------
    # OVERVIEW
    # --------------------------------------------------------
    
    nav_panel(
      "Overview",
      
      div(
        class = "page-heading",
        h2("Demand overview"),
        p("Explore historical electricity demand over time.")
      ),
      
      uiOutput("overview_kpis"),
      
      div(
        class = "insight",
        uiOutput("overview_insight")
      ),
      
      layout_columns(
        col_widths = c(8, 4),
        
        card(
          card_header("Electricity demand over time"),
          plotlyOutput("demand_trend", height = "400px")
        ),
        
        card(
          card_header("Demand distribution"),
          plotlyOutput("demand_distribution", height = "400px")
        )
      ),
      
      card(
        card_header("Monthly average demand by year"),
        plotlyOutput("annual_profile", height = "350px")
      ),
      
      card(
        card_header("Filtered data summary"),
        DTOutput("summary_table")
      )
    ),
    
    # --------------------------------------------------------
    # TIME PATTERNS
    # --------------------------------------------------------
    
    nav_panel(
      "Time patterns",
      
      div(
        class = "page-heading",
        h2("Temporal patterns"),
        p("Identify recurring daily and weekly demand behaviour.")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Average demand by hour"),
          plotlyOutput("hourly_profile", height = "350px")
        ),
        
        card(
          card_header("Average demand by weekday"),
          plotlyOutput("weekday_profile", height = "350px")
        )
      ),
      
      card(
        card_header("Hour × weekday heatmap"),
        plotlyOutput("temporal_heatmap", height = "430px")
      )
    ),
    
    # --------------------------------------------------------
    # WEATHER
    # --------------------------------------------------------
    
    nav_panel(
      "Weather & drivers",
      
      div(
        class = "page-heading",
        h2("Weather and demand drivers"),
        p("Explore associations between demand and weather features.")
      ),
      
      if (length(weather_cols) > 0) {
        layout_columns(
          col_widths = c(7, 5),
          
          card(
            card_header("Demand vs selected weather variable"),
            plotlyOutput("weather_scatter", height = "380px")
          ),
          
          card(
            card_header("Weather correlation"),
            plotlyOutput("weather_correlation", height = "380px")
          )
        )
      } else {
        card(
          card_header("Weather data unavailable"),
          p("No numeric weather columns matching expected names were found.")
        )
      },
      
      card(
        card_header("Weather variable summary"),
        DTOutput("weather_summary")
      )
    ),
    
    # --------------------------------------------------------
    # FORECAST DIAGNOSTICS
    # --------------------------------------------------------
    
    nav_panel(
      "Forecast diagnostics",
      
      div(
        class = "page-heading",
        h2("Forecast diagnostics"),
        p("Assess saved validation results without retraining models.")
      ),
      
      uiOutput("forecast_kpis"),
      
      div(
        class = "insight",
        uiOutput("forecast_insight")
      ),
      
      layout_columns(
        col_widths = c(8, 4),
        
        card(
          card_header("Actual vs predicted demand"),
          plotlyOutput("forecast_timeline", height = "360px")
        ),
        
        card(
          card_header("Prediction error distribution"),
          plotlyOutput("error_distribution", height = "360px")
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Mean absolute error by forecast step"),
          plotlyOutput("error_by_horizon", height = "330px")
        ),
        
        card(
          card_header("Forecast metrics"),
          DTOutput("forecast_metrics")
        )
      )
    ),
    
    # --------------------------------------------------------
    # MODEL COMPARISON
    # --------------------------------------------------------
    
    nav_panel(
      "Model comparison",
      
      div(
        class = "page-heading",
        h2("Model comparison"),
        p("Compare saved model results using available metrics.")
      ),
      
      selectInput(
        "comparison_metric",
        "Metric",
        choices = c("RMSE", "MAE", "MAPE", "R2", "R²"),
        selected = "RMSE"
      ),
      
      card(
        card_header("Performance comparison"),
        plotlyOutput("comparison_plot", height = "390px")
      ),
      
      card(
        card_header("Comparison data"),
        DTOutput("comparison_table")
      )
    )
  )
)

# ------------------------------------------------------------
# 9. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # RESET FILTERS
  # ----------------------------------------------------------
  
  observeEvent(input$reset_filters, {
    
    updateDateRangeInput(
      session,
      "date_range",
      start = date_min,
      end = date_max
    )
    
    updateSelectInput(
      session,
      "aggregation",
      selected = "day"
    )
    
    updateSelectInput(
      session,
      "weekday_filter",
      selected = "All days"
    )
    
    updateSelectInput(
      session,
      "year_filter",
      selected = "All years"
    )
  })
  
  # ----------------------------------------------------------
  # SHARED HISTORICAL FILTERS
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- historical
    
    if (
      !is.null(input$date_range) &&
      length(input$date_range) == 2 &&
      all(!is.na(input$date_range))
    ) {
      data <- data |>
        filter(
          date >= as.Date(input$date_range[1]),
          date <= as.Date(input$date_range[2])
        )
    }
    
    if (
      !is.null(input$weekday_filter) &&
      input$weekday_filter != "All days"
    ) {
      data <- data |>
        filter(as.character(weekday) == input$weekday_filter)
    }
    
    if (
      !is.null(input$year_filter) &&
      input$year_filter != "All years"
    ) {
      data <- data |>
        filter(year == as.numeric(input$year_filter))
    }
    
    data
  })
  
  # ----------------------------------------------------------
  # OVERVIEW KPI CARDS
  # ----------------------------------------------------------
  
  output$overview_kpis <- renderUI({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(
        div(
          class = "insight",
          "No records match the selected filters."
        )
      )
    }
    
    peak_row <- which.max(data$nat_demand)
    
    div(
      class = "metric-grid",
      
      make_card(
        "Mean demand",
        format_number(mean(data$nat_demand, na.rm = TRUE)),
        "Selected period"
      ),
      
      make_card(
        "Peak demand",
        format_number(max(data$nat_demand, na.rm = TRUE)),
        format(data$datetime[peak_row], "%Y-%m-%d %H:%M")
      ),
      
      make_card(
        "Minimum demand",
        format_number(min(data$nat_demand, na.rm = TRUE)),
        "Selected period"
      ),
      
      make_card(
        "Observations",
        format(nrow(data), big.mark = ","),
        paste(length(unique(data$date)), "days represented")
      )
    )
  })
  
  output$overview_insight <- renderUI({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data is available for the selected filters.")
    }
    
    peak_row <- which.max(data$nat_demand)
    
    HTML(paste0(
      "<strong>Key observation:</strong> Average demand is ",
      format_number(mean(data$nat_demand, na.rm = TRUE)),
      ". Peak demand is ",
      format_number(data$nat_demand[peak_row]),
      " at ",
      format(data$datetime[peak_row], "%Y-%m-%d %H:%M"),
      "."
    ))
  })
  
  # ----------------------------------------------------------
  # ELECTRICITY DEMAND OVER TIME
  # ----------------------------------------------------------
  
  output$demand_trend <- renderPlotly({
    
    data <- filtered_data()
    
    validate(
      need(nrow(data) > 0, "No records match the selected filters."),
      need(
        any(is.finite(data$nat_demand)),
        "No valid electricity demand values are available."
      ),
      need(
        any(!is.na(data$datetime)),
        "No valid timestamps are available."
      )
    )
    
    # Explicit aggregation. The x-axis remains a genuine date/time.
    agg <- switch(
      input$aggregation,
      
      hour = {
        data |>
          mutate(period = floor_date(datetime, "hour")) |>
          group_by(period) |>
          summarise(
            demand = mean(nat_demand, na.rm = TRUE),
            .groups = "drop"
          )
      },
      
      day = {
        data |>
          group_by(period = date) |>
          summarise(
            demand = mean(nat_demand, na.rm = TRUE),
            .groups = "drop"
          )
      },
      
      week = {
        data |>
          mutate(
            period = floor_date(
              datetime,
              "week",
              week_start = 1
            )
          ) |>
          group_by(period) |>
          summarise(
            demand = mean(nat_demand, na.rm = TRUE),
            .groups = "drop"
          )
      },
      
      month = {
        data |>
          mutate(period = floor_date(datetime, "month")) |>
          group_by(period) |>
          summarise(
            demand = mean(nat_demand, na.rm = TRUE),
            .groups = "drop"
          )
      }
    )
    
    agg <- agg |>
      filter(
        !is.na(period),
        is.finite(demand)
      ) |>
      arrange(period)
    
    validate(
      need(
        nrow(agg) > 0,
        "No time-series points could be calculated."
      )
    )
    
    # Convert Date to POSIXct consistently for Plotly's date axis.
    if (inherits(agg$period, "Date")) {
      agg$plot_time <- as.POSIXct(
        paste(as.character(agg$period), "00:00:00"),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      )
    } else {
      agg$plot_time <- as.POSIXct(agg$period, tz = "UTC")
    }
    
    validate(
      need(
        any(!is.na(agg$plot_time)),
        "The aggregated timestamps could not be plotted."
      )
    )
    
    plot_ly(
      data = agg,
      x = ~plot_time,
      y = ~demand,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#2457c5",
        width = 2
      ),
      connectgaps = FALSE,
      hovertemplate = paste0(
        "Time: %{x|%Y-%m-%d %H:%M}",
        "<br>Mean demand: %{y:,.2f}",
        "<extra></extra>"
      )
    ) |>
      layout(
        title = list(
          text = "Electricity demand over time",
          x = 0.02,
          xanchor = "left"
        ),
        xaxis = list(
          title = "Date and time",
          type = "date",
          gridcolor = "#edf0f5",
          automargin = TRUE,
          rangeslider = list(visible = TRUE)
        ),
        yaxis = list(
          title = "Mean electricity demand",
          gridcolor = "#edf0f5",
          automargin = TRUE
        ),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        font = list(
          family = "Arial",
          color = "#263244"
        ),
        margin = list(
          l = 65,
          r = 25,
          t = 65,
          b = 70
        )
      ) |>
      config(
        displaylogo = FALSE,
        responsive = TRUE,
        scrollZoom = TRUE
      )
  })
  
  # ----------------------------------------------------------
  # DEMAND DISTRIBUTION
  # ----------------------------------------------------------
  
  output$demand_distribution <- renderPlotly({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot("No data for the selected filters."))
    }
    
    plot_ly(
      data,
      x = ~nat_demand,
      type = "histogram",
      nbinsx = 35,
      marker = list(color = "#5b8def"),
      hovertemplate = paste0(
        "Demand: %{x:,.2f}",
        "<br>Count: %{y}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Distribution of demand",
        "Demand",
        "Frequency"
      )
  })
  
  # ----------------------------------------------------------
  # MONTHLY PROFILE BY YEAR
  # ----------------------------------------------------------
  
  output$annual_profile <- renderPlotly({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot("No data for the selected filters."))
    }
    
    annual <- data |>
      group_by(year, month_number, month) |>
      summarise(
        demand = mean(nat_demand, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(year, month_number)
    
    plot_ly(
      annual,
      x = ~month,
      y = ~demand,
      color = ~factor(year),
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = paste0(
        "Month: %{x}",
        "<br>Mean demand: %{y:,.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Monthly average demand by year",
        "Month",
        "Mean demand"
      )
  })
  
  # ----------------------------------------------------------
  # SUMMARY TABLE
  # ----------------------------------------------------------
  
  output$summary_table <- renderDT({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(
        datatable(data.frame(Status = "No matching records."))
      )
    }
    
    result <- data |>
      summarise(
        Observations = n(),
        `Mean demand` = mean(nat_demand, na.rm = TRUE),
        `Median demand` = median(nat_demand, na.rm = TRUE),
        `Minimum demand` = min(nat_demand, na.rm = TRUE),
        `Maximum demand` = max(nat_demand, na.rm = TRUE),
        `Standard deviation` = sd(nat_demand, na.rm = TRUE)
      )
    
    datatable(
      result,
      options = list(dom = "t", scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatRound(
        columns = names(result)[-1],
        digits = 2
      )
  })
  
  # ----------------------------------------------------------
  # AVERAGE DEMAND BY HOUR
  # ----------------------------------------------------------
  
  output$hourly_profile <- renderPlotly({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot("No data for the selected filters."))
    }
    
    profile <- data |>
      group_by(hour) |>
      summarise(
        demand = mean(nat_demand, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(hour)
    
    plot_ly(
      profile,
      x = ~hour,
      y = ~demand,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#2457c5", width = 3),
      marker = list(size = 5),
      hovertemplate = paste0(
        "Hour: %{x}:00",
        "<br>Mean demand: %{y:,.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Average demand by hour",
        "Hour of day",
        "Mean demand"
      )
  })
  
  # ----------------------------------------------------------
  # AVERAGE DEMAND BY WEEKDAY
  # ----------------------------------------------------------
  
  output$weekday_profile <- renderPlotly({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot("No data for the selected filters."))
    }
    
    profile <- data |>
      group_by(weekday) |>
      summarise(
        demand = mean(nat_demand, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_ly(
      profile,
      x = ~weekday,
      y = ~demand,
      type = "bar",
      marker = list(color = "#16a085"),
      hovertemplate = paste0(
        "Day: %{x}",
        "<br>Mean demand: %{y:,.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Average demand by weekday",
        "Weekday",
        "Mean demand"
      )
  })
  
  # ----------------------------------------------------------
  # HOUR × WEEKDAY HEATMAP
  # ----------------------------------------------------------
  
  output$temporal_heatmap <- renderPlotly({
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(empty_plot("No data for the selected filters."))
    }
    
    heat <- data |>
      group_by(weekday, hour) |>
      summarise(
        demand = mean(nat_demand, na.rm = TRUE),
        .groups = "drop"
      ) |>
      complete(
        weekday,
        hour = 0:23,
        fill = list(demand = NA_real_)
      ) |>
      arrange(weekday, hour)
    
    wide <- heat |>
      mutate(hour = as.character(hour)) |>
      select(weekday, hour, demand) |>
      pivot_wider(
        names_from = hour,
        values_from = demand
      )
    
    z <- as.matrix(wide[, -1, drop = FALSE])
    
    plot_ly(
      x = 0:23,
      y = as.character(wide$weekday),
      z = z,
      type = "heatmap",
      colors = colorRamp(c("#eff6ff", "#60a5fa", "#1d4ed8")),
      hovertemplate = paste0(
        "Hour: %{x}:00",
        "<br>Weekday: %{y}",
        "<br>Mean demand: %{z:,.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Demand by hour and weekday",
        "Hour of day",
        "Weekday"
      )
  })
  
  # ----------------------------------------------------------
  # WEATHER SCATTER
  # ----------------------------------------------------------
  
  output$weather_scatter <- renderPlotly({
    
    if (length(weather_cols) == 0) {
      return(empty_plot("No weather variables are available."))
    }
    
    data <- filtered_data()
    
    req(input$weather_variable)
    req(input$weather_variable %in% names(data))
    
    weather <- data[[input$weather_variable]]
    
    valid <- is.finite(weather) & is.finite(data$nat_demand)
    
    if (sum(valid) < 3) {
      return(empty_plot("Not enough valid weather observations."))
    }
    
    plot_ly(
      x = weather[valid],
      y = data$nat_demand[valid],
      type = "scatter",
      mode = "markers",
      marker = list(
        color = "#2457c5",
        opacity = 0.45,
        size = 5
      ),
      hovertemplate = paste0(
        "Weather: %{x:.2f}",
        "<br>Demand: %{y:,.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        paste("Demand vs", input$weather_variable),
        input$weather_variable,
        "Demand"
      )
  })
  
  # ----------------------------------------------------------
  # WEATHER CORRELATION
  # ----------------------------------------------------------
  
  output$weather_correlation <- renderPlotly({
    
    if (length(weather_cols) == 0) {
      return(empty_plot("No weather variables are available."))
    }
    
    data <- filtered_data()
    
    numeric_data <- data |>
      select(all_of(c("nat_demand", weather_cols))) |>
      select(where(is.numeric))
    
    if (ncol(numeric_data) < 2 || nrow(numeric_data) < 3) {
      return(empty_plot("Not enough data for correlation analysis."))
    }
    
    corr <- suppressWarnings(
      cor(numeric_data, use = "pairwise.complete.obs")
    )
    
    corr[!is.finite(corr)] <- NA_real_
    
    plot_ly(
      x = colnames(corr),
      y = rownames(corr),
      z = corr,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      zmid = 0,
      colors = colorRamp(c("#b91c1c", "#f8fafc", "#1d4ed8")),
      hovertemplate = paste0(
        "%{y} vs %{x}",
        "<br>Correlation: %{z:.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Correlation matrix",
        "Variable",
        "Variable"
      )
  })
  
  # ----------------------------------------------------------
  # WEATHER SUMMARY
  # ----------------------------------------------------------
  
  output$weather_summary <- renderDT({
    
    if (length(weather_cols) == 0) {
      return(
        datatable(data.frame(
          Status = "No numeric weather columns found."
        ))
      )
    }
    
    data <- filtered_data()
    
    result <- lapply(weather_cols, function(variable) {
      
      x <- data[[variable]]
      valid <- x[is.finite(x)]
      
      data.frame(
        Variable = variable,
        Observations = length(valid),
        Mean = if (length(valid)) mean(valid) else NA_real_,
        SD = if (length(valid) > 1) sd(valid) else NA_real_,
        Minimum = if (length(valid)) min(valid) else NA_real_,
        Maximum = if (length(valid)) max(valid) else NA_real_
      )
    }) |>
      bind_rows()
    
    datatable(
      result,
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatRound(
        columns = c("Mean", "SD", "Minimum", "Maximum"),
        digits = 2
      )
  })
  
  # ----------------------------------------------------------
  # FORECAST KPI CARDS
  # ----------------------------------------------------------
  
  output$forecast_kpis <- renderUI({
    
    if (nrow(predictions) == 0) {
      return(
        div(
          class = "insight",
          "Forecast diagnostics unavailable. Check the saved prediction CSV."
        )
      )
    }
    
    data <- predictions
    
    rmse <- sqrt(mean(data$error^2, na.rm = TRUE))
    mae <- mean(data$abs_error, na.rm = TRUE)
    
    valid_mape <- is.finite(data$actual_demand) &
      data$actual_demand != 0
    
    mape <- if (any(valid_mape)) {
      mean(
        abs(
          data$error[valid_mape] /
            data$actual_demand[valid_mape]
        ),
        na.rm = TRUE
      ) * 100
    } else {
      NA_real_
    }
    
    div(
      class = "metric-grid",
      
      make_card(
        "RMSE",
        format_number(rmse),
        "Lower is better"
      ),
      
      make_card(
        "MAE",
        format_number(mae),
        "Average absolute error"
      ),
      
      make_card(
        "MAPE",
        if (is.finite(mape)) {
          paste0(format_number(mape), "%")
        } else {
          "N/A"
        },
        "Excludes zero actual values"
      ),
      
      make_card(
        "Predictions",
        format(nrow(data), big.mark = ","),
        "Saved validation results"
      )
    )
  })
  
  output$forecast_insight <- renderUI({
    
    if (nrow(predictions) == 0) {
      return("No saved predictions are available.")
    }
    
    bias <- mean(
      predictions$predicted_demand -
        predictions$actual_demand,
      na.rm = TRUE
    )
    
    direction <- if (bias > 0) {
      "overpredicts on average"
    } else if (bias < 0) {
      "underpredicts on average"
    } else {
      "has approximately zero average bias"
    }
    
    HTML(paste0(
      "<strong>Bias check:</strong> The model ",
      direction,
      ". Mean predicted minus actual demand is ",
      format_number(bias),
      ". Positive values indicate overprediction."
    ))
  })
  
  # ----------------------------------------------------------
  # ACTUAL VS PREDICTED TIMELINE
  # ----------------------------------------------------------
  
  output$forecast_timeline <- renderPlotly({
    
    if (
      nrow(predictions) == 0 ||
      !"datetime" %in% names(predictions)
    ) {
      return(empty_plot("No usable forecast timeline is available."))
    }
    
    data <- predictions |>
      filter(!is.na(datetime)) |>
      arrange(datetime)
    
    if (nrow(data) == 0) {
      return(empty_plot("No valid forecast timestamps were found."))
    }
    
    plot_ly(data, x = ~datetime) |>
      add_lines(
        y = ~actual_demand,
        name = "Actual",
        line = list(color = "#0f766e", width = 2)
      ) |>
      add_lines(
        y = ~predicted_demand,
        name = "Predicted",
        line = list(color = "#2457c5", width = 2)
      ) |>
      style_plot(
        "Actual vs predicted demand",
        "Time",
        "Demand"
      )
  })
  
  # ----------------------------------------------------------
  # ERROR DISTRIBUTION
  # ----------------------------------------------------------
  
  output$error_distribution <- renderPlotly({
    
    if (nrow(predictions) == 0) {
      return(empty_plot("No prediction errors are available."))
    }
    
    plot_ly(
      predictions,
      x = ~error,
      type = "histogram",
      nbinsx = 35,
      marker = list(color = "#8b5cf6"),
      hovertemplate = paste0(
        "Error: %{x:,.2f}",
        "<br>Count: %{y}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Prediction error distribution",
        "Actual − predicted",
        "Frequency"
      )
  })
  
  # ----------------------------------------------------------
  # ERROR BY FORECAST HORIZON
  # ----------------------------------------------------------
  
  output$error_by_horizon <- renderPlotly({
    
    if (
      nrow(predictions) == 0 ||
      all(is.na(predictions$horizon_hour))
    ) {
      return(empty_plot(
        "Forecast horizon values are not available in the saved predictions."
      ))
    }
    
    profile <- predictions |>
      filter(
        is.finite(horizon_hour),
        is.finite(abs_error)
      ) |>
      group_by(horizon_hour) |>
      summarise(
        MAE = mean(abs_error, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(horizon_hour)
    
    if (nrow(profile) == 0) {
      return(empty_plot("No valid forecast horizon/error pairs were found."))
    }
    
    plot_ly(
      profile,
      x = ~horizon_hour,
      y = ~MAE,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#d97706", width = 3),
      hovertemplate = paste0(
        "Forecast step: %{x}",
        "<br>Mean absolute error: %{y:,.2f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        "Mean absolute error by forecast step",
        "Forecast step (hours)",
        "MAE"
      )
  })
  
  # ----------------------------------------------------------
  # FORECAST METRICS TABLE
  # ----------------------------------------------------------
  
  output$forecast_metrics <- renderDT({
    
    if (nrow(predictions) == 0) {
      return(
        datatable(data.frame(
          Status = "No prediction data available."
        ))
      )
    }
    
    data <- predictions
    
    valid_mape <- is.finite(data$actual_demand) &
      data$actual_demand != 0
    
    mape <- if (any(valid_mape)) {
      mean(
        abs(
          data$error[valid_mape] /
            data$actual_demand[valid_mape]
        ),
        na.rm = TRUE
      ) * 100
    } else {
      NA_real_
    }
    
    result <- data.frame(
      Metric = c("RMSE", "MAE", "MAPE (%)", "Mean bias"),
      Value = c(
        sqrt(mean(data$error^2, na.rm = TRUE)),
        mean(data$abs_error, na.rm = TRUE),
        mape,
        mean(
          data$predicted_demand - data$actual_demand,
          na.rm = TRUE
        )
      )
    )
    
    datatable(
      result,
      options = list(dom = "t", ordering = FALSE),
      rownames = FALSE
    ) |>
      formatRound("Value", digits = 3)
  })
  
  # ----------------------------------------------------------
  # MODEL COMPARISON PLOT
  # ----------------------------------------------------------
  
  output$comparison_plot <- renderPlotly({
    
    if (nrow(comparison) == 0) {
      return(empty_plot("No saved model comparison data is available."))
    }
    
    model_candidates <- intersect(
      c("model", "Model", "model_name", "algorithm", "method"),
      names(comparison)
    )
    
    if (length(model_candidates) == 0) {
      return(empty_plot(paste(
        "No model-name column was found.",
        "Available columns:",
        paste(names(comparison), collapse = ", ")
      )))
    }
    
    metric_map <- list(
      "RMSE" = c("RMSE", "rmse"),
      "MAE" = c("MAE", "mae"),
      "MAPE" = c("MAPE", "mape"),
      "R2" = c("R2", "r2", "R²", "rsquared"),
      "R²" = c("R2", "r2", "R²", "rsquared")
    )
    
    metric_candidates <- metric_map[[input$comparison_metric]]
    
    metric_col <- intersect(
      metric_candidates,
      names(comparison)
    )
    
    if (length(metric_col) == 0) {
      return(empty_plot(paste(
        "Metric column not found.",
        "Available columns:",
        paste(names(comparison), collapse = ", ")
      )))
    }
    
    metric_col <- metric_col[1]
    model_col <- model_candidates[1]
    
    plot_data <- data.frame(
      Model = as.character(comparison[[model_col]]),
      Metric = suppressWarnings(
        as.numeric(as.character(comparison[[metric_col]]))
      )
    ) |>
      filter(
        !is.na(Model),
        is.finite(Metric)
      )
    
    if (nrow(plot_data) == 0) {
      return(empty_plot("No numeric model metric values were found."))
    }
    
    plot_ly(
      plot_data,
      x = ~reorder(Model, Metric),
      y = ~Metric,
      type = "bar",
      marker = list(color = "#2457c5"),
      hovertemplate = paste0(
        "Model: %{x}",
        "<br>Metric: %{y:.4f}",
        "<extra></extra>"
      )
    ) |>
      style_plot(
        paste("Model comparison:", metric_col),
        "Model",
        metric_col
      )
  })
  
  # ----------------------------------------------------------
  # MODEL COMPARISON TABLE
  # ----------------------------------------------------------
  
  output$comparison_table <- renderDT({
    
    if (nrow(comparison) == 0) {
      return(
        datatable(data.frame(
          Status = "Comparison file is missing or empty."
        ))
      )
    }
    
    datatable(
      comparison,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

# ------------------------------------------------------------
# 10. RUN APP
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)
