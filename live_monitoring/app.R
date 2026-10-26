
# ============================================================
# Panama Electricity Demand — Live Monitoring Dashboard
# Source: CND / SITR public live feed
#
# Standalone monitoring module.
# Does not modify the historical dataset, SQLite database,
# trained models, or the existing forecasting pipeline.
# ============================================================

# -----------------------------
# 1. Packages
# -----------------------------
required_packages <- c(
  "shiny",
  "httr",
  "jsonlite",
  "plotly"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the missing packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(httr)
library(jsonlite)
library(plotly)


# -----------------------------
# 2. Configuration
# -----------------------------
CND_URL <- "https://sitr.cnd.com.pa/m/pub/data/sin.json"
PANAMA_TZ <- "America/Panama"
REFRESH_INTERVAL_MS <- 60000L


# -----------------------------
# 3. Utility functions
# -----------------------------

format_number <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) {
    return("—")
  }
  
  format(
    round(x, digits),
    big.mark = ",",
    decimal.mark = ".",
    nsmall = digits,
    trim = TRUE,
    scientific = FALSE
  )
}


get_field <- function(x, candidates) {
  if (is.null(x) || is.null(names(x))) {
    return(NULL)
  }
  
  matched <- intersect(candidates, names(x))
  
  if (length(matched) == 0) {
    return(NULL)
  }
  
  x[[matched[[1]]]]
}


# Convert a CND time object to a Panama-local timestamp.
# The CND JavaScript feed uses zero-based months.
parse_cnd_time <- function(x) {
  if (is.null(x)) {
    return(as.POSIXct(NA, tz = PANAMA_TZ))
  }
  
  # A timestamp supplied directly as text
  if (is.character(x) && length(x) == 1) {
    parsed <- suppressWarnings(
      as.POSIXct(
        x,
        tz = PANAMA_TZ,
        tryFormats = c(
          "%Y-%m-%dT%H:%M:%S",
          "%Y-%m-%d %H:%M:%S",
          "%Y-%m-%dT%H:%M:%OS"
        )
      )
    )
    
    return(parsed)
  }
  
  # A Unix timestamp, if the feed supplies one
  if (is.numeric(x) && length(x) == 1) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = PANAMA_TZ))
  }
  
  year <- get_field(x, c("y", "year"))
  month <- get_field(x, c("m", "month"))
  day <- get_field(x, c("d", "day"))
  hour <- get_field(x, c("h", "hour"))
  minute <- get_field(x, c("mi", "minute", "min"))
  second <- get_field(x, c("s", "second", "sec"))
  
  required_parts <- list(year, month, day, hour, minute)
  
  if (any(vapply(required_parts, is.null, logical(1)))) {
    return(as.POSIXct(NA, tz = PANAMA_TZ))
  }
  
  second <- if (is.null(second)) 0 else second
  
  # CND JavaScript uses Date(year, month, ...):
  # January = 0, February = 1, etc.
  actual_month <- as.integer(month) + 1L
  
  timestamp_text <- sprintf(
    "%04d-%02d-%02d %02d:%02d:%02d",
    as.integer(year),
    actual_month,
    as.integer(day),
    as.integer(hour),
    as.integer(minute),
    as.integer(second)
  )
  
  as.POSIXct(
    timestamp_text,
    format = "%Y-%m-%d %H:%M:%S",
    tz = PANAMA_TZ
  )
}


translate_cnd_timestamp <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("Update time unavailable")
  }
  
  month_names <- c(
    "enero", "febrero", "marzo", "abril",
    "mayo", "junio", "julio", "agosto",
    "septiembre", "octubre", "noviembre", "diciembre"
  )
  
  month_numbers <- sprintf("%02d", 1:12)
  
  for (i in seq_along(month_names)) {
    x <- gsub(
      month_names[[i]],
      month_numbers[[i]],
      x,
      ignore.case = TRUE
    )
  }
  
  parsed <- suppressWarnings(
    as.POSIXct(
      x,
      format = "%d-%m-%Y %H:%M:%S",
      tz = PANAMA_TZ
    )
  )
  
  if (is.na(parsed)) {
    return(as.character(x))
  }
  
  format(parsed, "%d %b %Y, %H:%M:%S")
}


# Extract the values from one minute of the CND trend.
# Supports common field names so the app can report a useful
# error if the public feed changes its structure.
extract_point_values <- function(point) {
  if (is.numeric(point) || is.integer(point)) {
    return(as.numeric(point))
  }
  
  values <- get_field(
    point,
    c("values", "value", "data", "y", "val")
  )
  
  if (is.null(values)) {
    stop(
      "A CND trend point did not contain a recognised values field.",
      call. = FALSE
    )
  }
  
  as.numeric(unlist(values, use.names = FALSE))
}


# -----------------------------
# 4. Fetch and validate live feed
# -----------------------------
fetch_cnd_live <- function() {
  response <- httr::GET(
    CND_URL,
    query = list(cache_bust = as.numeric(Sys.time())),
    httr::add_headers(
      "Cache-Control" = "no-cache, no-store",
      "Pragma" = "no-cache"
    ),
    httr::timeout(30)
  )
  
  httr::stop_for_status(response)
  
  response_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  payload <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )
  
  if (is.null(payload$trend)) {
    stop("The CND response did not contain a trend section.")
  }
  
  trend <- payload$trend
  series <- trend$series
  series_names <- unlist(trend$names, use.names = FALSE)
  
  if (is.null(series) || length(series) < 2) {
    stop("The CND feed returned insufficient trend data.")
  }
  
  if (length(series_names) == 0) {
    stop("The CND feed did not provide trend labels.")
  }
  
  # Locate the actual and forecast series by their CND labels.
  normalise_label <- function(x) {
    tolower(trimws(as.character(x)))
  }
  
  normalised_names <- normalise_label(series_names)
  
  actual_index <- match(
    normalise_label("Demanda Real"),
    normalised_names
  )
  
  forecast_index <- match(
    normalise_label("Demanda Pronosticada"),
    normalised_names
  )
  
  if (is.na(actual_index) || is.na(forecast_index)) {
    stop(
      "Could not locate 'Demanda Real' and ",
      "'Demanda Pronosticada' in the CND feed."
    )
  }
  
  # Each point should contain a time object and the values
  # corresponding to trend$names.
  timestamps <- lapply(series, function(point) {
    time_value <- get_field(
      point,
      c("time", "timestamp", "datetime", "date")
    )
    
    parse_cnd_time(time_value)
  })
  
  actual_values <- lapply(series, function(point) {
    values <- extract_point_values(point)
    
    if (length(values) < max(actual_index, forecast_index)) {
      return(NA_real_)
    }
    
    values[[actual_index]]
  })
  
  forecast_values <- lapply(series, function(point) {
    values <- extract_point_values(point)
    
    if (length(values) < max(actual_index, forecast_index)) {
      return(NA_real_)
    }
    
    values[[forecast_index]]
  })
  
  live_data <- data.frame(
    timestamp = as.POSIXct(
      unlist(timestamps),
      origin = "1970-01-01",
      tz = PANAMA_TZ
    ),
    actual = as.numeric(unlist(actual_values)),
    forecast = as.numeric(unlist(forecast_values))
  )
  
  # Remove invalid records rather than plotting misleading values.
  valid_rows <- !is.na(live_data$timestamp) &
    is.finite(live_data$actual) &
    is.finite(live_data$forecast)
  
  live_data <- live_data[valid_rows, , drop = FALSE]
  live_data <- live_data[order(live_data$timestamp), , drop = FALSE]
  rownames(live_data) <- NULL
  
  if (nrow(live_data) < 2) {
    stop(
      "Fewer than two valid records were received from the CND feed."
    )
  }
  
  if (anyDuplicated(live_data$timestamp)) {
    stop(
      "The CND feed contains duplicate timestamps. ",
      "The current snapshot was rejected."
    )
  }
  
  # Reject implausibly large readings rather than silently
  # presenting corrupted values.
  if (any(live_data$actual < 0) || any(live_data$forecast < 0)) {
    stop("The CND feed contains negative demand values.")
  }
  
  update_text <- translate_cnd_timestamp(payload$update)
  
  list(
    data = live_data,
    update_text = update_text,
    fetched_at = Sys.time(),
    record_count = nrow(live_data)
  )
}


# -----------------------------
# 5. User interface
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$title("Panama Electricity | Live Monitor"),
    
    tags$style(HTML("
      body {
        background: #F3F6FA;
        color: #172033;
        font-family: 'Segoe UI', Arial, sans-serif;
      }

      .main-container {
        max-width: 1450px;
        margin: 0 auto;
        padding: 22px;
      }

      .hero {
        background: #14243A;
        color: white;
        padding: 25px 28px;
        border-radius: 14px;
        margin-bottom: 18px;
        box-shadow: 0 4px 14px rgba(20, 36, 58, 0.12);
      }

      .hero h2 {
        margin: 0 0 8px 0;
        font-weight: 650;
        letter-spacing: 0.1px;
      }

      .hero p {
        color: #D4DFEE;
        margin: 0;
        line-height: 1.5;
      }

      .toolbar {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 18px;
      }

      .toolbar .btn {
        background: #2563EB;
        color: white;
        border: none;
        border-radius: 8px;
        padding: 10px 16px;
        font-weight: 600;
      }

      .toolbar .btn:hover {
        background: #1D4ED8;
        color: white;
      }

      .status {
        display: inline-block;
        padding: 8px 12px;
        border-radius: 8px;
        font-size: 13px;
        font-weight: 600;
      }

      .status-ok {
        color: #166534;
        background: #DCFCE7;
      }

      .status-error {
        color: #991B1B;
        background: #FEE2E2;
      }

      .status-wait {
        color: #92400E;
        background: #FEF3C7;
      }

      .metric-card {
        background: white;
        border: 1px solid #E2E8F0;
        border-radius: 12px;
        padding: 19px 20px;
        min-height: 125px;
        margin-bottom: 15px;
        box-shadow: 0 2px 7px rgba(15, 23, 42, 0.035);
      }

      .metric-label {
        color: #64748B;
        font-size: 13px;
        font-weight: 650;
        text-transform: uppercase;
        letter-spacing: 0.45px;
        margin-bottom: 12px;
      }

      .metric-value {
        color: #172033;
        font-size: 29px;
        line-height: 1.2;
        font-weight: 700;
        overflow-wrap: anywhere;
      }

      .metric-unit {
        color: #64748B;
        font-size: 13px;
        margin-top: 6px;
      }

      .panel {
        background: white;
        border: 1px solid #E2E8F0;
        border-radius: 12px;
        padding: 18px 20px;
        margin-bottom: 18px;
        box-shadow: 0 2px 7px rgba(15, 23, 42, 0.035);
      }

      .panel h4 {
        margin-top: 0;
        margin-bottom: 6px;
        color: #172033;
        font-weight: 700;
      }

      .panel-subtitle {
        color: #64748B;
        font-size: 13px;
        margin-bottom: 15px;
      }

      .caveat {
        background: #FFF7ED;
        border: 1px solid #FED7AA;
        color: #7C2D12;
        border-radius: 10px;
        padding: 14px 16px;
        margin-bottom: 18px;
        line-height: 1.55;
        font-size: 13px;
      }

      .footer {
        color: #64748B;
        font-size: 12px;
        text-align: center;
        padding: 12px 0 5px 0;
      }

      table {
        width: 100%;
      }

      .table > thead > tr > th {
        color: #475569;
        font-size: 12px;
        border-bottom: 1px solid #CBD5E1;
      }

      .table > tbody > tr > td {
        font-size: 13px;
        vertical-align: middle;
      }

      @media (max-width: 768px) {
        .main-container {
          padding: 12px;
        }

        .hero {
          padding: 20px;
        }

        .metric-value {
          font-size: 25px;
        }
      }
    "))
  ),
  
  div(
    class = "main-container",
    
    # Header
    div(
      class = "hero",
      h2("Panama Electricity Demand Monitor"),
      p(
        "Live demand tracking using the public CND / SITR feed. ",
        "Monitor reported actual demand alongside the CND forecast."
      )
    ),
    
    # Refresh controls and feed status
    div(
      class = "toolbar",
      
      actionButton(
        "refresh_live",
        "Refresh data",
        icon = icon("refresh")
      ),
      
      uiOutput("feed_status")
    ),
    
    # Metrics
    fluidRow(
      column(
        width = 4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Latest actual demand"),
          uiOutput("actual_metric"),
          div(class = "metric-unit", "Megawatts (MW)")
        )
      ),
      
      column(
        width = 4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Latest CND forecast"),
          uiOutput("forecast_metric"),
          div(class = "metric-unit", "Megawatts (MW)")
        )
      ),
      
      column(
        width = 4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Actual − forecast"),
          uiOutput("difference_metric"),
          div(class = "metric-unit", "Megawatts (MW)")
        )
      )
    ),
    
    # Live chart
    div(
      class = "panel",
      
      h4("Demand over the available live window"),
      
      div(
        class = "panel-subtitle",
        "Minute-level observations supplied by CND. ",
        "The feed is a rolling window, not a permanent historical archive."
      ),
      
      plotlyOutput("live_plot", height = "460px")
    ),
    
    # Recent observations
    div(
      class = "panel",
      
      h4("Most recent readings"),
      
      div(
        class = "panel-subtitle",
        "The 12 latest valid timestamps received from the feed."
      ),
      
      tableOutput("recent_readings")
    ),
    
    # Interpretation caveat
    div(
      class = "caveat",
      tags$strong("Interpretation note: "),
      "The displayed forecast is the forecast published by CND, ",
      "not a prediction produced by this project's trained model. ",
      "Its exact lead time is not established here. ",
      "Differences between actual and forecast demand in a live snapshot ",
      "should not be interpreted as a formal model evaluation."
    ),
    
    div(
      class = "footer",
      "Standalone live-monitoring module · Historical analysis, ",
      "database tables, and trained models remain unchanged."
    )
  )
)


# -----------------------------
# 6. Server
# -----------------------------
server <- function(input, output, session) {
  
  # Refresh automatically every minute or when the button is clicked.
  live_bundle <- reactive({
    invalidateLater(REFRESH_INTERVAL_MS, session)
    
    input$refresh_live
    
    tryCatch(
      fetch_cnd_live(),
      error = function(e) {
        list(
          error = conditionMessage(e),
          fetched_at = Sys.time()
        )
      }
    )
  })
  
  
  # Feed status
  output$feed_status <- renderUI({
    bundle <- live_bundle()
    
    if (!is.null(bundle$error)) {
      return(
        span(
          class = "status status-error",
          paste("Feed unavailable:", bundle$error)
        )
      )
    }
    
    span(
      class = "status status-ok",
      paste0(
        "Feed connected · ",
        bundle$record_count,
        " valid readings · Updated ",
        format(bundle$fetched_at, "%H:%M:%S", tz = PANAMA_TZ),
        " Panama time"
      )
    )
  })
  
  
  # Latest actual demand
  output$actual_metric <- renderUI({
    bundle <- live_bundle()
    
    if (!is.null(bundle$error)) {
      return(div(class = "metric-value", "—"))
    }
    
    d <- bundle$data
    latest <- d[nrow(d), ]
    
    div(
      class = "metric-value",
      format_number(latest$actual)
    )
  })
  
  
  # Latest CND forecast
  output$forecast_metric <- renderUI({
    bundle <- live_bundle()
    
    if (!is.null(bundle$error)) {
      return(div(class = "metric-value", "—"))
    }
    
    d <- bundle$data
    latest <- d[nrow(d), ]
    
    div(
      class = "metric-value",
      format_number(latest$forecast)
    )
  })
  
  
  # Difference between actual and CND forecast
  output$difference_metric <- renderUI({
    bundle <- live_bundle()
    
    if (!is.null(bundle$error)) {
      return(div(class = "metric-value", "—"))
    }
    
    d <- bundle$data
    latest <- d[nrow(d), ]
    difference <- latest$actual - latest$forecast
    
    difference_color <- if (difference > 0) {
      "#B45309"
    } else if (difference < 0) {
      "#2563EB"
    } else {
      "#172033"
    }
    
    div(
      class = "metric-value",
      style = paste0("color:", difference_color, ";"),
      if (difference > 0) "+" else "",
      format_number(difference)
    )
  })
  
  
  # Direct Plotly chart.
  # This avoids ggplotly conversion and horizontal-legend warnings.
  output$live_plot <- renderPlotly({
    bundle <- live_bundle()
    
    validate(
      need(
        is.null(bundle$error),
        paste("Unable to load live chart:", bundle$error)
      ),
      need(
        !is.null(bundle$data) && nrow(bundle$data) > 1,
        "Not enough live data to draw the chart."
      )
    )
    
    d <- bundle$data
    d <- d[order(d$timestamp), ]
    
    plot_ly(
      data = d,
      x = ~timestamp
    ) %>%
      add_lines(
        y = ~actual,
        name = "Actual demand",
        line = list(
          color = "#2563EB",
          width = 2
        ),
        hovertemplate = paste0(
          "%{x|%d %b %Y, %H:%M}",
          "<br>Actual demand: %{y:,.0f} MW",
          "<extra></extra>"
        )
      ) %>%
      add_lines(
        y = ~forecast,
        name = "CND forecast",
        line = list(
          color = "#EA580C",
          width = 2,
          dash = "dash"
        ),
        hovertemplate = paste0(
          "%{x|%d %b %Y, %H:%M}",
          "<br>CND forecast: %{y:,.0f} MW",
          "<extra></extra>"
        )
      ) %>%
      layout(
        xaxis = list(
          title = "",
          tickformat = "%d %b<br>%H:%M",
          nticks = 7,
          automargin = TRUE,
          showgrid = TRUE,
          gridcolor = "#E8EDF4"
        ),
        yaxis = list(
          title = "Electricity demand (MW)",
          tickformat = ",.0f",
          automargin = TRUE,
          rangemode = "tozero",
          showgrid = TRUE,
          gridcolor = "#E8EDF4"
        ),
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1,
          font = list(size = 11)
        ),
        margin = list(
          l = 65,
          r = 140,
          t = 25,
          b = 65
        ),
        hovermode = "x unified",
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) %>%
      config(
        displaylogo = FALSE,
        responsive = TRUE,
        scrollZoom = TRUE
      )
  })
  
  
  # Recent readings table
  output$recent_readings <- renderTable({
    bundle <- live_bundle()
    
    validate(
      need(
        is.null(bundle$error),
        paste("Unable to load readings:", bundle$error)
      )
    )
    
    d <- bundle$data
    d <- d[order(d$timestamp, decreasing = TRUE), ]
    d <- head(d, 12)
    
    data.frame(
      `Panama time` = format(
        d$timestamp,
        "%d %b %Y, %H:%M",
        tz = PANAMA_TZ
      ),
      `Actual demand (MW)` = vapply(
        d$actual,
        format_number,
        character(1)
      ),
      `CND forecast (MW)` = vapply(
        d$forecast,
        format_number,
        character(1)
      ),
      `Difference (MW)` = vapply(
        d$actual - d$forecast,
        function(x) {
          paste0(
            if (x > 0) "+" else "",
            format_number(x)
          )
        },
        character(1)
      ),
      check.names = FALSE
    )
  },
  striped = TRUE,
  bordered = FALSE,
  hover = TRUE,
  spacing = "s",
  rownames = FALSE
  )
}


# -----------------------------
# 7. Run app
# -----------------------------
shinyApp(ui = ui, server = server)
