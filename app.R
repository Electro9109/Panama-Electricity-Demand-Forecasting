# ============================================================
# Panama Electricity Demand Forecasting Dashboard
# Shiny + Plotly + SQLite
#
# Sections:
#   1. Historical Demand
#   2. Forecast Backtesting
#   3. Model Comparison
#   4. Live Monitoring
#
# This application uses saved results. It does not retrain models.
# ============================================================

library(shiny)
library(plotly)
library(DBI)
library(RSQLite)
library(ggplot2)
library(httr)
library(jsonlite)

# Live dashboard is a Shiny module. Paths are relative to the project root.
if (!file.exists("live_monitoring/live_module.R")) {
  stop("Missing live_monitoring/live_module.R. Keep the module in the live_monitoring folder.")
}
source("live_monitoring/live_module.R", local = TRUE)

# ============================================================
# 1. FILE PATHS
# ============================================================

db_path <- "outputs/pipeline.sqlite"
backtest_path <- "outputs/results/validation_backtest_rf.rds"
predictions_path <- "outputs/results/validation_backtest_rf_predictions.csv"
comparison_path <- "outputs/results/rf_vs_seasonal_naive.csv"

# ============================================================
# 2. LOAD HISTORICAL DATA FROM SQLITE
# ============================================================

historical <- data.frame()

if (file.exists(db_path)) {
  
  con <- dbConnect(SQLite(), db_path)
  
  tryCatch({
    
    available_tables <- dbListTables(con)
    
    # Prefer a table containing both required columns.
    for (table_name in c("raw_panama", "processed_panama")) {
      
      if (table_name %in% available_tables) {
        
        candidate <- dbReadTable(con, table_name)
        
        if (all(c("datetime", "nat_demand") %in% names(candidate))) {
          historical <- candidate
          break
        }
      }
    }
    
  }, finally = {
    dbDisconnect(con)
  })
}

if (nrow(historical) > 0) {
  
  # Handle timestamps stored as character or POSIXct.
  if (!inherits(historical$datetime, "POSIXct")) {
    historical$datetime <- as.POSIXct(
      historical$datetime,
      tz = "UTC"
    )
  }
  
  historical$nat_demand <- suppressWarnings(
    as.numeric(historical$nat_demand)
  )
  
  historical <- historical[
    !is.na(historical$datetime) &
      !is.na(historical$nat_demand),
    ,
    drop = FALSE
  ]
  
  historical <- historical[
    order(historical$datetime),
    ,
    drop = FALSE
  ]
}

# ============================================================
# 3. LOAD SAVED BACKTEST RESULTS
# ============================================================

rf_summary <- data.frame()

if (file.exists(backtest_path)) {
  
  backtest <- tryCatch(
    readRDS(backtest_path),
    error = function(e) NULL
  )
  
  if (!is.null(backtest) &&
      "summary_metrics" %in% names(backtest)) {
    
    rf_summary <- as.data.frame(backtest$summary_metrics)
  }
}

# ============================================================
# 4. LOAD FORECAST PREDICTIONS
# ============================================================

predictions <- data.frame()

if (file.exists(predictions_path)) {
  
  predictions <- tryCatch(
    read.csv(
      predictions_path,
      stringsAsFactors = FALSE
    ),
    error = function(e) data.frame()
  )
}

required_prediction_columns <- c(
  "origin",
  "horizon",
  "datetime",
  "actual_demand",
  "predicted_demand"
)

if (nrow(predictions) > 0 &&
    all(required_prediction_columns %in% names(predictions))) {
  
  predictions$horizon <- as.numeric(predictions$horizon)
  
  predictions$datetime <- as.POSIXct(
    predictions$datetime,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  )
  
  predictions$actual_demand <- as.numeric(
    predictions$actual_demand
  )
  
  predictions$predicted_demand <- as.numeric(
    predictions$predicted_demand
  )
  
  predictions <- predictions[
    !is.na(predictions$datetime) &
      !is.na(predictions$horizon) &
      !is.na(predictions$actual_demand) &
      !is.na(predictions$predicted_demand),
    ,
    drop = FALSE
  ]
  
} else {
  
  predictions <- data.frame()
}

# ============================================================
# 5. LOAD MODEL COMPARISON RESULTS
# ============================================================

comparison <- data.frame()

if (file.exists(comparison_path)) {
  
  comparison <- tryCatch(
    read.csv(
      comparison_path,
      stringsAsFactors = FALSE
    ),
    error = function(e) data.frame()
  )
}

# ============================================================
# 6. HELPER FUNCTIONS
# ============================================================

format_number <- function(x, digits = 2) {
  
  if (length(x) == 0 || is.na(x[1])) {
    return("N/A")
  }
  
  format(
    round(as.numeric(x[1]), digits),
    big.mark = ",",
    nsmall = digits
  )
}

get_rf_metric <- function(horizon, metric) {
  
  if (nrow(rf_summary) == 0 ||
      !all(c("horizon", metric) %in% names(rf_summary))) {
    return(NA_real_)
  }
  
  result <- rf_summary[
    as.numeric(rf_summary$horizon) == as.numeric(horizon),
    ,
    drop = FALSE
  ]
  
  if (nrow(result) == 0) {
    return(NA_real_)
  }
  
  as.numeric(result[[metric]][1])
}

metric_card <- function(label, value, description) {
  
  div(
    class = "metric-card",
    h4(label),
    div(class = "metric-value", value),
    p(class = "metric-description", description)
  )
}

# Convert comparison CSV columns into plotting format.
make_comparison_long <- function(metric) {
  
  rf_column <- paste0("RF_", metric)
  naive_column <- paste0("Naive_", metric)
  
  required <- c("horizon", rf_column, naive_column)
  
  if (nrow(comparison) == 0 ||
      !all(required %in% names(comparison))) {
    return(data.frame())
  }
  
  horizons <- as.numeric(comparison$horizon)
  
  data.frame(
    horizon = rep(horizons, times = 2),
    model = factor(
      c(
        rep("Random Forest", length(horizons)),
        rep("Seasonal Naive", length(horizons))
      ),
      levels = c("Random Forest", "Seasonal Naive")
    ),
    value = c(
      as.numeric(comparison[[rf_column]]),
      as.numeric(comparison[[naive_column]])
    )
  )
}

# ============================================================
# 7. USER INTERFACE
# ============================================================

if (nrow(historical) > 0) {
  
  date_min <- as.Date(min(historical$datetime, na.rm = TRUE))
  date_max <- as.Date(max(historical$datetime, na.rm = TRUE))
  
} else {
  
  date_min <- as.Date("2015-01-01")
  date_max <- as.Date("2020-06-30")
}

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
        color: #263238;
      }

      .dashboard-header {
        background-color: #17324d;
        color: white;
        padding: 22px;
        margin: -15px -15px 20px -15px;
      }

      .dashboard-header h2 {
        margin-top: 0;
        font-weight: 600;
      }

      .dashboard-header p {
        margin-bottom: 0;
        color: #dce6ef;
      }

      .metric-card {
        background: white;
        border: 1px solid #e0e5eb;
        border-radius: 7px;
        padding: 15px;
        min-height: 120px;
        margin-bottom: 15px;
      }

      .metric-card h4 {
        color: #586675;
        font-size: 14px;
        margin-top: 0;
      }

      .metric-value {
        font-size: 25px;
        font-weight: 600;
        color: #17324d;
        margin: 8px 0;
      }

      .metric-description {
        font-size: 12px;
        color: #73808c;
        margin-bottom: 0;
      }

      .section-note {
        background: white;
        border-left: 4px solid #4f7cac;
        padding: 14px;
        margin-top: 12px;
        margin-bottom: 18px;
      }
      .live-header { background: #17324d; color: white; padding: 18px 22px; border-radius: 9px; margin-bottom: 16px; }
      .live-header h3 { margin-top: 0; font-weight: 650; }
      .live-header p { margin-bottom: 0; color: #dce6ef; }
      .live-metric-card { background: white; border: 1px solid #e0e5eb; border-radius: 9px; padding: 16px; min-height: 115px; margin-bottom: 15px; }
      .live-metric-label { color: #586675; font-size: 12px; font-weight: 650; text-transform: uppercase; margin-bottom: 10px; }
      .live-metric-value { font-size: 27px; font-weight: 700; color: #17324d; overflow-wrap: anywhere; }
      .live-metric-unit { color: #73808c; font-size: 12px; margin-top: 4px; }
      .live-panel { background: white; border: 1px solid #e0e5eb; border-radius: 9px; padding: 16px; margin-bottom: 15px; }
      .live-status { padding: 9px 12px; border-radius: 7px; margin: 3px 0 12px 0; font-size: 12px; overflow-wrap: anywhere; }
      .live-status-ok { color: #166534; background: #dcfce7; }
      .live-status-error { color: #991b1b; background: #fee2e2; }
      .live-caveat { background: #fff7ed; border: 1px solid #fed7aa; color: #7c2d12; padding: 13px 15px; border-radius: 8px; margin-bottom: 15px; font-size: 13px; line-height: 1.5; }
    "))
  ),
  
  # Dashboard header
  div(
    class = "dashboard-header",
    h2("Panama Electricity Demand Forecasting"),
    p(
      "Historical demand analysis, forecast backtesting, ",
      "and model performance comparison"
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Dashboard Controls"),
      
      selectInput(
        "section",
        "Analysis",
        choices = c(
          "Historical Demand",
          "Forecast Backtesting",
          "Model Comparison",
          "Live Monitoring"
        ),
        selected = "Historical Demand"
      ),
      
      conditionalPanel(
        condition = "input.section == 'Historical Demand'",
        
        dateRangeInput(
          "date_range",
          "Historical date range",
          start = date_min,
          end = date_max,
          min = date_min,
          max = date_max,
          format = "yyyy-mm-dd"
        )
      ),
      
      conditionalPanel(
        condition = paste0(
          "input.section == 'Forecast Backtesting' || ",
          "input.section == 'Model Comparison'"
        ),
        
        selectInput(
          "horizon",
          "Forecast horizon",
          choices = c(
            "24 hours" = 24,
            "72 hours" = 72,
            "144 hours" = 144
          ),
          selected = 72
        )
      ),
      
      conditionalPanel(
        condition = "input.section == 'Model Comparison'",
        
        selectInput(
          "comparison_metric",
          "Comparison metric",
          choices = c(
            "Mean Absolute Error (MAE)" = "MAE",
            "Root Mean Squared Error (RMSE)" = "RMSE",
            "Mean Absolute Percentage Error (MAPE)" = "MAPE",
            "R-squared (R²)" = "R2"
          ),
          selected = "RMSE"
        )
      ),
      
      hr(),
      
      p(
        "Historical data is loaded from SQLite. ",
        "Forecasts and evaluation metrics are loaded from saved outputs."
      )
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "input.section !== 'Live Monitoring'",
        uiOutput("summary_cards"),
        plotlyOutput("main_plot", height = "520px"),
        div(class = "section-note", uiOutput("interpretation")),
        h4("Supporting results"),
        tableOutput("results_table"),
        br(),
        p(class = "text-muted",
          "Historical backtesting results measure performance on selected ",
          "past periods and do not guarantee future forecasting accuracy.")
      ),
      conditionalPanel(
        condition = "input.section === 'Live Monitoring'",
        live_monitor_ui("live")
      )
    )
  )
)

# ============================================================
# 8. SERVER LOGIC
# ============================================================

server <- function(input, output, session) {
  
  # Live feed is isolated in a module and remains read-only.
  live_monitor_server("live")
  
  # ----------------------------------------------------------
  # Filter historical observations by selected dates
  # ----------------------------------------------------------
  
  historical_filtered <- reactive({
    
    req(input$date_range)
    
    if (nrow(historical) == 0) {
      return(data.frame())
    }
    
    start_time <- as.POSIXct(
      input$date_range[1],
      tz = "UTC"
    )
    
    end_time <- as.POSIXct(
      input$date_range[2] + 1,
      tz = "UTC"
    )
    
    historical[
      historical$datetime >= start_time &
        historical$datetime < end_time,
      ,
      drop = FALSE
    ]
  })
  
  # ----------------------------------------------------------
  # Filter predictions by selected horizon
  # ----------------------------------------------------------
  
  predictions_filtered <- reactive({
    
    req(input$horizon)
    
    if (nrow(predictions) == 0) {
      return(data.frame())
    }
    
    result <- predictions[
      predictions$horizon == as.numeric(input$horizon),
      ,
      drop = FALSE
    ]
    
    if (nrow(result) > 0) {
      result <- result[
        order(result$origin, result$datetime),
        ,
        drop = FALSE
      ]
      
      result$Origin <- factor(result$origin)
    }
    
    result
  })
  
  # ==========================================================
  # 9. SUMMARY CARDS
  # ==========================================================
  
  output$summary_cards <- renderUI({
    
    if (identical(input$section, "Live Monitoring")) return(NULL)
    
    # Historical statistics
    if (input$section == "Historical Demand") {
      
      d <- historical_filtered()
      
      if (nrow(d) == 0) {
        return(
          p("No historical observations are available for this date range.")
        )
      }
      
      fluidRow(
        column(
          4,
          metric_card(
            "Observations",
            format(nrow(d), big.mark = ","),
            "Hourly records in selected period"
          )
        ),
        column(
          4,
          metric_card(
            "Average demand",
            format_number(mean(d$nat_demand, na.rm = TRUE)),
            "Mean national demand"
          )
        ),
        column(
          4,
          metric_card(
            "Peak demand",
            format_number(max(d$nat_demand, na.rm = TRUE)),
            "Maximum observed demand"
          )
        )
      )
      
      # Forecast performance metrics
    } else if (input$section == "Forecast Backtesting") {
      
      h <- as.numeric(input$horizon)
      
      fluidRow(
        column(
          4,
          metric_card(
            "Mean MAE",
            format_number(get_rf_metric(h, "mean_MAE")),
            "Lower is better"
          )
        ),
        column(
          4,
          metric_card(
            "Mean RMSE",
            format_number(get_rf_metric(h, "mean_RMSE")),
            "Penalizes larger errors"
          )
        ),
        column(
          4,
          metric_card(
            "Mean MAPE",
            paste0(format_number(get_rf_metric(h, "mean_MAPE")), "%"),
            "Mean absolute percentage error"
          )
        )
      )
      
      # Selected model comparison metrics
    } else {
      
      metric <- input$comparison_metric
      long_data <- make_comparison_long(metric)
      
      if (nrow(long_data) == 0) {
        return(p("Model comparison data is unavailable."))
      }
      
      selected <- long_data[
        long_data$horizon == as.numeric(input$horizon),
        ,
        drop = FALSE
      ]
      
      rf_value <- selected$value[
        selected$model == "Random Forest"
      ]
      
      naive_value <- selected$value[
        selected$model == "Seasonal Naive"
      ]
      
      fluidRow(
        column(
          6,
          metric_card(
            "Random Forest",
            format_number(rf_value),
            paste("Selected metric:", metric)
          )
        ),
        column(
          6,
          metric_card(
            "Seasonal Naive",
            format_number(naive_value),
            paste("Selected metric:", metric)
          )
        )
      )
    }
  })
  
  # ==========================================================
  # 10. MAIN VISUALIZATION
  # ==========================================================
  
  output$main_plot <- renderPlotly({
    
    if (identical(input$section, "Live Monitoring")) return(NULL)
    
    # --------------------------------------------------------
    # Historical Demand
    # --------------------------------------------------------
    
    if (input$section == "Historical Demand") {
      
      d <- historical_filtered()
      
      validate(
        need(
          nrow(d) > 0,
          "No historical data is available for the selected dates."
        )
      )
      
      p <- ggplot(
        d,
        aes(
          x = datetime,
          y = nat_demand
        )
      ) +
        geom_line(
          colour = "#2457A7",
          linewidth = 0.45
        ) +
        labs(
          title = "Historical National Electricity Demand",
          x = "Date",
          y = "National demand"
        ) +
        theme_minimal(base_size = 12)
      
      return(ggplotly(p))
      
      # --------------------------------------------------------
      # Forecast Backtesting
      # --------------------------------------------------------
      
    } else if (input$section == "Forecast Backtesting") {
      
      d <- predictions_filtered()
      
      validate(
        need(
          nrow(d) > 0,
          paste(
            "Forecast predictions are unavailable.",
            "Check the saved prediction CSV and its column names."
          )
        )
      )
      
      p <- ggplot(
        d,
        aes(x = datetime)
      ) +
        geom_line(
          aes(
            y = actual_demand,
            colour = "Actual demand"
          ),
          linewidth = 0.7
        ) +
        geom_line(
          aes(
            y = predicted_demand,
            colour = "Random Forest forecast"
          ),
          linewidth = 0.7,
          linetype = "dashed"
        ) +
        facet_wrap(
          ~ Origin,
          scales = "free_x"
        ) +
        scale_colour_manual(
          name = NULL,
          values = c(
            "Actual demand" = "#2457A7",
            "Random Forest forecast" = "#D97706"
          )
        ) +
        labs(
          title = paste(
            "Actual vs Random Forest Forecast —",
            input$horizon,
            "hours"
          ),
          subtitle = paste(
            "Each panel represents a historical forecast origin."
          ),
          x = "Forecast time",
          y = "National demand"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          ),
          strip.text = element_text(face = "bold")
        )
      
      return(ggplotly(p))
      
      # --------------------------------------------------------
      # Model Comparison
      # --------------------------------------------------------
      
    } else {
      
      metric <- input$comparison_metric
      long_data <- make_comparison_long(metric)
      
      validate(
        need(
          nrow(long_data) > 0,
          paste(
            "Comparison data is unavailable.",
            "Check the saved model comparison CSV."
          )
        )
      )
      
      # Display all horizons, while highlighting the selected one
      long_data$Selected <- ifelse(
        long_data$horizon == as.numeric(input$horizon),
        "Selected horizon",
        "Other horizons"
      )
      
      p <- ggplot(
        long_data,
        aes(
          x = factor(
            horizon,
            levels = c(24, 72, 144),
            labels = c("24 hours", "72 hours", "144 hours")
          ),
          y = value,
          fill = model,
          alpha = Selected
        )
      ) +
        geom_col(
          position = position_dodge(width = 0.75),
          width = 0.65
        ) +
        scale_fill_manual(
          values = c(
            "Random Forest" = "#2457A7",
            "Seasonal Naive" = "#D97706"
          )
        ) +
        scale_alpha_manual(
          values = c(
            "Selected horizon" = 1,
            "Other horizons" = 0.65
          ),
          guide = "none"
        ) +
        labs(
          title = paste("Model Comparison:", metric),
          subtitle = if (metric == "R2") {
            "Higher R-squared is generally better"
          } else {
            "Lower error values are better"
          },
          x = "Forecast horizon",
          y = metric,
          fill = "Model"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "bottom")
      
      return(ggplotly(p))
    }
  })
  
  # ==========================================================
  # 11. INTERPRETATION
  # ==========================================================
  
  output$interpretation <- renderUI({
    
    if (identical(input$section, "Live Monitoring")) return(NULL)
    
    if (input$section == "Historical Demand") {
      
      HTML(
        paste0(
          "<strong>Interpretation:</strong> ",
          "Use the time series to inspect demand changes, recurring ",
          "patterns, peaks, and unusual periods. The summary statistics ",
          "refer only to the selected date range."
        )
      )
      
    } else if (input$section == "Forecast Backtesting") {
      
      h <- as.numeric(input$horizon)
      
      mae <- get_rf_metric(h, "mean_MAE")
      rmse <- get_rf_metric(h, "mean_RMSE")
      mape <- get_rf_metric(h, "mean_MAPE")
      r2 <- get_rf_metric(h, "mean_R2")
      
      HTML(
        paste0(
          "<strong>Interpretation:</strong> For the ",
          h, "-hour horizon, the saved mean MAE is ",
          format_number(mae),
          ", mean RMSE is ",
          format_number(rmse),
          ", mean MAPE is ",
          format_number(mape),
          "%, and mean R² is ",
          format_number(r2),
          ". MAE and RMSE use the demand target's units; ",
          "MAPE is expressed as a percentage. A negative R² indicates ",
          "that performance can be worse than a simple mean-based ",
          "reference on the evaluated observations. The panels compare ",
          "actual and predicted demand across forecast origins."
        )
      )
      
    } else {
      
      metric <- input$comparison_metric
      long_data <- make_comparison_long(metric)
      
      if (nrow(long_data) == 0) {
        return(
          HTML(
            "<strong>Interpretation:</strong> Model comparison results are unavailable."
          )
        )
      }
      
      selected <- long_data[
        long_data$horizon == as.numeric(input$horizon),
        ,
        drop = FALSE
      ]
      
      rf_value <- selected$value[
        selected$model == "Random Forest"
      ]
      
      naive_value <- selected$value[
        selected$model == "Seasonal Naive"
      ]
      
      if (length(rf_value) == 0 || length(naive_value) == 0) {
        return(
          HTML(
            "<strong>Interpretation:</strong> No comparison results exist for this horizon."
          )
        )
      }
      
      if (metric == "R2") {
        
        outcome <- if (rf_value > naive_value) {
          "Random Forest has the higher R²."
        } else if (rf_value < naive_value) {
          "Seasonal Naive has the higher R²."
        } else {
          "Both models have the same R²."
        }
        
      } else {
        
        outcome <- if (rf_value < naive_value) {
          "Random Forest has the lower error."
        } else if (rf_value > naive_value) {
          "Seasonal Naive has the lower error."
        } else {
          "Both models have the same error."
        }
      }
      
      HTML(
        paste0(
          "<strong>Interpretation:</strong> At the ",
          input$horizon,
          "-hour horizon, the selected metric is ",
          metric,
          ". Random Forest = ",
          format_number(rf_value),
          "; Seasonal Naive = ",
          format_number(naive_value),
          ". ",
          outcome,
          " This is a descriptive comparison of the saved backtests, ",
          "not a test of statistical significance."
        )
      )
    }
  })
  
  # ==========================================================
  # 12. SUPPORTING RESULTS TABLE
  # ==========================================================
  
  output$results_table <- renderTable({
    
    if (identical(input$section, "Live Monitoring")) return(data.frame())
    
    if (input$section == "Historical Demand") {
      
      d <- historical_filtered()
      
      if (nrow(d) == 0) {
        return(data.frame())
      }
      
      data.frame(
        Statistic = c(
          "Observations",
          "Mean demand",
          "Median demand",
          "Minimum demand",
          "Maximum demand",
          "Standard deviation"
        ),
        Value = c(
          format(nrow(d), big.mark = ","),
          format_number(mean(d$nat_demand, na.rm = TRUE)),
          format_number(median(d$nat_demand, na.rm = TRUE)),
          format_number(min(d$nat_demand, na.rm = TRUE)),
          format_number(max(d$nat_demand, na.rm = TRUE)),
          format_number(sd(d$nat_demand, na.rm = TRUE))
        )
      )
      
    } else if (input$section == "Forecast Backtesting") {
      
      if (nrow(rf_summary) == 0) {
        return(data.frame())
      }
      
      rf_summary
      
    } else {
      
      if (nrow(comparison) == 0) {
        return(data.frame())
      }
      
      comparison
    }  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

# ============================================================
# 13. LAUNCH APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
