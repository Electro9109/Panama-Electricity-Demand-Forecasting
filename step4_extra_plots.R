source("R/config.R")
source("R/models.R")
source("R/viz.R")

ck <- readRDS("outputs/step1.rds")
feat <- ck$feat; final_features <- ck$final_features; profile <- ck$profile
result <- readRDS("outputs/panama_result.rds")

splits <- chronological_split(feat, profile)
te <- splits$test[stats::complete.cases(splits$test[, c(final_features, profile$demand_col)]), ]

best_model <- result$metrics$model[1]
p1 <- plot_actual_vs_predicted_timeseries(te[[profile$datetime_col]], result$actual,
                                          result$preds[[best_model]], best_model, n_show = 500)
ggplot2::ggsave("outputs/figures/actual_vs_predicted_timeseries.png", p1, width = 9, height = 5)

weather_cols <- intersect(unlist(profile$weather_stations, use.names = FALSE), names(feat))
p2 <- plot_weather_correlation(feat, weather_cols)
ggplot2::ggsave("outputs/figures/weather_correlation.png", p2, width = 7, height = 6)

p3 <- plot_tuning_comparison(before_rmse = 40.31258, after_rmse = 30.66173, model_name = "Random Forest")
ggplot2::ggsave("outputs/figures/tuning_comparison_rf.png", p3, width = 5, height = 5)

cat("done\n")