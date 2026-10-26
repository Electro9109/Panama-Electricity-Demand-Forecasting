# Panama Electricity Demand Forecasting

A modular R-based project for exploring, forecasting, and monitoring
Panama's national electricity demand using historical demand, weather,
calendar, and time-series features. The project combines a reproducible
data pipeline, SQLite storage, forecasting and backtesting components,
and a Shiny dashboard. It is also being extended to investigate a
live-data connection to Panama's official CND/SITR system.

> **Project status:** Historical-data processing, the SQLite pipeline,
> forecasting/backtesting artifacts, and the Shiny application are part
> of the existing project. The CND/SITR JSON feed has been reached and
> parsed successfully in a standalone test. Live-feed integration with
> the existing database, feature pipeline, trained model, and dashboard
> should be treated as **in progress**, not as a completed end-to-end
> live forecasting feature.

## Table of Contents

-   [Objectives](#objectives)
-   [Use Case and Stakeholders](#use-case-and-stakeholders)
-   [Data](#data)
-   [System Overview](#system-overview)
-   [Forecasting and Evaluation](#forecasting-and-evaluation)
-   [Live CND/SITR Data Investigation](#live-cndsitr-data-investigation)
-   [Project Structure](#project-structure)
-   [Environment and Setup](#environment-and-setup)
-   [Running the Application](#running-the-application)
-   [Reproducibility and Data
    Integrity](#reproducibility-and-data-integrity)
-   [Limitations](#limitations)
-   [Roadmap](#roadmap)

## Objectives

The project is intended to provide a modular electricity-demand
forecasting and decision-support framework. Its current implementation
focuses on Panama and aims to:

1.  Organize historical demand and supporting variables in a consistent
    data pipeline.
2.  Create time-aware features from demand history, weather, and
    calendar information.
3.  Train and evaluate forecasting models using chronological evaluation
    rather than relying only on random train/test splits.
4.  Compare forecasts with observed demand and make model performance
    easier to inspect.
5.  Provide a dashboard for reviewing historical demand, backtesting
    results, and model comparisons.
6.  Establish a safe path toward using current observations from an
    external source without silently corrupting the historical dataset
    or changing model inputs.

The design goal is modularity: the Panama dataset is the current case
study, while future datasets should be able to use dataset-specific
adapters and feature mappings around a reusable forecasting workflow.

## Use Case and Stakeholders

### Intended users

-   **Energy analysts and planners:** review demand patterns and
    forecast behaviour.
-   **System operators and engineers:** use forecasts as an additional
    reference when assessing expected versus observed demand.
-   **Researchers and students:** compare modelling approaches and
    document forecasting performance.
-   **Developers and maintainers:** extend data adapters, features, and
    models without rewriting the entire application.

### Inputs and outputs

**Inputs** - Historical hourly national demand. - Weather observations
from the Tocumen, Santiago, and David stations where available. -
Calendar and time-derived features. - Lagged demand and rolling
statistics. - Potential live system observations from CND/SITR, subject
to validation and integration.

**Outputs** - Processed data and engineered features. - Forecasts and
backtesting predictions. - Model evaluation metrics and comparison
artifacts. - Dashboard views of historical demand, backtesting, and
model performance. - In a future integrated workflow, a monitored
live-data input and forecast view, once the live data and model feature
requirements have been validated.

The system is a decision-support tool. A difference between observed and
predicted demand is a discrepancy to investigate, not proof of a
specific cause. The current data alone cannot establish illegal
activity, reporting errors, or other causal explanations.

## Data

### Historical dataset

The current SQLite database is:

``` text
outputs/pipeline.sqlite
```

Known tables include:

-   `raw_panama` --- source-level/raw fields used by the pipeline.
-   `processed_panama` --- processed hourly observations and engineered
    features.

The processed table has been inspected as containing **48,048 hourly
rows**, covering **2015-01-03 through 2020-06-27**, with the target
column `nat_demand`. Its 32 columns include demand, weather-related
variables for Tocumen/Santiago/David, calendar flags, cyclical time
features, demand lags (`lag_1`, `lag_24`, `lag_168`), and rolling
statistics (`roll_mean`, `roll_sd`). Exact columns should be verified
against the current database because the schema may evolve.

### Live CND/SITR source

The public CND/SITR system page is:

-   [CND/SITR live system page](https://sitr.cnd.com.pa/m/pub/sin.html)
-   Tested JSON endpoint: `https://sitr.cnd.com.pa/m/pub/data/sin.json`

The JSON response has been successfully retrieved in R with HTTP 200 and
parsed with `jsonlite`. The response includes current system indicators
and a trend series containing:

-   Generation (`Generación`)
-   Actual demand (`Demanda Real`)
-   CND forecast demand (`Demanda Pronosticada`)

The trend test returned 1,441 timestamped observations, apparently at
one-minute intervals across roughly a 24-hour window. Confirm the
interval, timestamps, timezone interpretation, freshness, and
completeness at runtime; do not assume the number of entries alone
guarantees complete coverage. Current indicator labels include
`Carga total`, while the trend labels actual demand as `Demanda Real`;
map these fields deliberately rather than treating them as
interchangeable without validation.

The endpoint currently supplies a short rolling window, not a
replacement for the multi-year historical training dataset. It must not
be appended directly to the hourly historical table without validation,
timestamp alignment, aggregation rules, and duplicate handling.

## System Overview

The project is implemented in R. Its main responsibilities are separated
into data preparation, feature engineering, model operations,
forecasting/backtesting, database access, and visualization.

At a high level:

1.  **Load and store data** in SQLite.
2.  **Prepare and validate observations**, including timestamp and
    missing-value checks.
3.  **Engineer features** from time, calendar, weather, and demand
    history.
4.  **Train or load forecasting models** using the project's model
    scripts and saved artifacts.
5.  **Evaluate chronologically** using backtesting and appropriate error
    metrics.
6.  **Present results** in a Shiny dashboard.
7.  **Investigate live ingestion separately** and validate it before
    connecting it to production-facing application logic.

The current dashboard includes historical-demand exploration, forecast
backtesting, and model comparison. It reads saved results rather than
retraining every model on application startup.

## Forecasting and Evaluation

The project has used Random Forest and seasonal-naive forecasting in
saved comparison/backtesting outputs. The model and evaluation scripts
should be treated as the source of truth for the current configuration.

Metrics used in project artifacts include:

-   **MAE** --- mean absolute error.
-   **RMSE** --- root mean squared error.
-   **MAPE** --- mean absolute percentage error.
-   **R²** --- coefficient of determination.

Forecast performance can vary by forecast horizon. In particular, a
model that performs well at one horizon should not be assumed to perform
equally well at another. Compare models on the same chronological test
windows and horizons, and document the evaluation protocol alongside the
reported metrics.

The project has previously produced a Random Forest versus
seasonal-naive comparison artifact at:

``` text
outputs/results/rf_vs_seasonal_naive.csv
```

The dashboard also uses saved Random Forest backtesting predictions at:

``` text
outputs/results/validation_backtest_rf_predictions.csv
```

and saved metric objects under `outputs/results/`. Confirm the current
files and exact metric definitions before publishing results.

## Live CND/SITR Data Investigation

The live-source investigation has confirmed that R can access the JSON
endpoint and that the response contains current indicators and
timestamped trend values. This establishes source connectivity, but does
**not** by itself establish that the existing trained model can produce
a valid live forecast.

Before integration, the live adapter should:

1.  Request the endpoint with a timeout and handle HTTP/network errors.
2.  Validate the response structure and required series labels.
3.  Parse timestamps and numeric values explicitly.
4.  Check source update time, observation freshness, duplicates, missing
    values, and interval consistency.
5.  Convert minute-level observations to hourly values only with a
    documented aggregation and timestamp convention.
6.  Keep live/staging data separate from `processed_panama` until schema
    and feature compatibility are demonstrated.
7.  Verify that all required model features are available at forecast
    time, including the lagged-demand and weather/calendar features
    expected by the trained model.
8.  Log failures and refuse to produce a misleading forecast when
    required inputs are missing or stale.

The current model's historical feature set includes weather and
lagged-demand features. The CND feed's recent demand trend alone may not
provide all those inputs. A live forecast therefore requires either a
validated way to supply the required features or a separately evaluated
forecasting approach designed for the live feed.

## Project Structure

The following paths are known from the project. This is a guide to the
principal components, not an exhaustive inventory; use the current
repository contents to confirm every file.

``` text
Panama/
├── app.R
├── visualisations_app.R
├── R/
│   ├── backtesting.R
│   ├── config.R
│   ├── data_prep.R
│   ├── database.R
│   ├── features.R
│   ├── forecasting.R
│   ├── models.R
│   ├── tune.R
│   └── viz.R
├── outputs/
│   ├── pipeline.sqlite
│   ├── results/
│   │   ├── rf_vs_seasonal_naive.csv
│   │   └── validation_backtest_rf_predictions.csv
│   └── ...
└── README.md
```

### Main components

  -----------------------------------------------------------------------
  Component                           Responsibility
  ----------------------------------- -----------------------------------
  `app.R`                             Main Shiny dashboard entry point.

  `visualisations_app.R`              Additional/alternative
                                      visualization application entry
                                      point.

  `R/config.R`                        Shared configuration.

  `R/database.R`                      Database access helpers.

  `R/data_prep.R`                     Data preparation.

  `R/features.R`                      Feature engineering.

  `R/models.R`                        Model-related functions.

  `R/tune.R`                          Model tuning.

  `R/forecasting.R`                   Forecasting operations.

  `R/backtesting.R`                   Backtesting workflow.

  `R/viz.R`                           Visualization helpers.

  `outputs/pipeline.sqlite`           SQLite database containing raw and
                                      processed tables.

  `outputs/results/`                  Saved model comparison, prediction,
                                      and evaluation artifacts.
  -----------------------------------------------------------------------

## Environment and Setup

### Requirements

-   R (use the version installed and tested with the project).
-   RStudio is recommended but not required.
-   Access to the project files and SQLite database.
-   Network access for the optional CND/SITR live-source test.

The project uses R packages including Shiny, Plotly, DBI, RSQLite, and
data-manipulation/modeling packages referenced by the scripts. The
live-source test uses `httr` and `jsonlite`; HTML inspection may use
`xml2`. Check the `library()` calls in the current scripts and install
any missing dependencies. A pinned dependency manifest is recommended
for reproducibility if one is not already present.

### Set the working directory

In RStudio, set the working directory to the project root. For the
current Windows development environment:

``` r
setwd("C:/Users/ASUS/Coding/R/Panama")
```

Verify the location before running scripts:

``` r
getwd()
file.exists("outputs/pipeline.sqlite")
```

### Check the database

``` r
library(DBI)
library(RSQLite)

con <- dbConnect(
  SQLite(),
  "outputs/pipeline.sqlite",
  flags = RSQLite::SQLITE_RO
)

dbListTables(con)
dbGetQuery(con, "PRAGMA table_info('processed_panama')")

dbDisconnect(con)
```

This inspection uses read-only access. Use the project's existing
database functions for normal application workflows.

## Running the Application

From the project root, launch the main Shiny application with:

``` r
shiny::runApp("app.R")
```

The dashboard reads historical data and saved evaluation artifacts. If
it fails to start, check that the expected database and files under
`outputs/results/` exist and that the required R packages are installed.

Do not assume that launching the dashboard retrains the models or
refreshes live data. The current dashboard uses saved
predictions/metrics; live integration must be implemented and tested
separately.

## Reproducibility and Data Integrity

-   Preserve the original/raw table and avoid overwriting source data
    during preprocessing.
-   Use chronological train/validation/test splits for time-series
    evaluation.
-   Record model configuration, feature set, forecast horizon,
    evaluation window, and random seed where applicable.
-   Compare candidate models on identical evaluation windows.
-   Check timestamps, duplicates, missing observations, units, and
    interval consistency before joining datasets.
-   Keep experimental ingestion and live-source data separate until
    acceptance checks pass.
-   Do not treat a successful HTTP request as proof of data correctness
    or model compatibility.
-   Do not report a live forecast as successful unless its inputs are
    complete, fresh, and aligned with the trained model's feature
    schema.

## Limitations

-   The historical training dataset currently covers 2015--2020, so it
    may not represent current demand patterns without retraining and
    re-evaluation.
-   The CND/SITR live feed is a short rolling window and does not
    replace long-term training data.
-   Minute-level live values and hourly historical observations require
    an explicit aggregation and timestamp policy.
-   Live demand values alone do not satisfy the existing model's full
    feature requirements.
-   Forecast accuracy varies by horizon and should be reported with the
    evaluation protocol.
-   Forecast/actual discrepancies are indicators for further analysis,
    not proof of a particular cause.
-   The project is a forecasting and decision-support prototype; it
    should not be treated as an operational grid-control system.

## Roadmap

1.  Complete read-only schema and timestamp compatibility checks between
    the live feed and SQLite history.
2.  Implement a standalone live-source adapter with explicit validation
    and error handling.
3.  Test minute-to-hour aggregation, timezone handling, freshness, and
    duplicate/missing-observation behaviour.
4.  Decide how the existing model's required features will be supplied
    for a live forecast.
5.  Validate the live forecasting method against appropriate held-out
    historical periods before presenting it as operational.
6.  Integrate live status/forecast information into the Shiny dashboard
    only after the adapter and forecast path are reliable.
7.  Simplify the model-comparison presentation to a compact comparison
    summary where appropriate, keeping the dashboard focused on useful
    forecast information.

------------------------------------------------------------------------

## Maintainer note

This README describes the known project state and distinguishes verified
functionality from work still under investigation. Update the structure,
dependency instructions, model results, and live-integration status
whenever the implementation changes. Avoid documenting a feature as
complete until it has been tested end to end.
