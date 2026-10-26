library(ggplot2)

plot_error_by_model <- function(metric_tbl, metric = "RMSE") {
  df <- metric_tbl
  df$model <- factor(df$model, levels = df$model[order(df[[metric]])])
  ggplot(df, aes(x = model, y = .data[[metric]], fill = is_ml_dl)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "#2c7fb8", "FALSE" = "#969696"),
                      labels = c("TRUE" = "ML/DL", "FALSE" = "Classical baseline"),
                      name = NULL) +
    labs(x = NULL, y = metric, title = paste("Model comparison by", metric)) +
    theme_minimal()
}

plot_actual_vs_predicted <- function(actual, predicted, model_name) {
  df <- data.frame(actual = actual, predicted = predicted)
  ggplot(df, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Actual demand", y = "Predicted demand",
         title = paste("Actual vs predicted -", model_name)) +
    theme_minimal()
}

plot_residuals <- function(actual, predicted, model_name) {
  df <- data.frame(predicted = predicted, residual = actual - predicted)
  ggplot(df, aes(x = predicted, y = residual)) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Predicted demand", y = "Residual",
         title = paste("Residuals -", model_name)) +
    theme_minimal()
}

plot_feature_importance <- function(feature_names, importance_values, model_name) {
  df <- data.frame(feature = feature_names, importance = importance_values)
  df$feature <- factor(df$feature, levels = df$feature[order(df$importance)])
  ggplot(df, aes(x = feature, y = importance)) +
    geom_col(fill = "#2c7fb8") +
    coord_flip() +
    labs(x = NULL, y = "Importance",
         title = paste("Feature importance -", model_name)) +
    theme_minimal()
}

save_all_comparison_plots <- function(result, feature_names, importance_values,
                                      out_dir = "outputs/figures") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  best_model <- result$metrics$model[1]
  
  ggsave(file.path(out_dir, "error_by_model_rmse.png"),
         plot_error_by_model(result$metrics, "RMSE"), width = 7, height = 5)
  ggsave(file.path(out_dir, "error_by_model_mape.png"),
         plot_error_by_model(result$metrics, "MAPE"), width = 7, height = 5)
  ggsave(file.path(out_dir, "actual_vs_predicted.png"),
         plot_actual_vs_predicted(result$actual, result$preds[[best_model]], best_model),
         width = 6, height = 6)
  ggsave(file.path(out_dir, "residuals.png"),
         plot_residuals(result$actual, result$preds[[best_model]], best_model),
         width = 6, height = 5)
  ggsave(file.path(out_dir, "feature_importance.png"),
         plot_feature_importance(feature_names, importance_values, "Random Forest"),
         width = 7, height = 6)
}

plot_actual_vs_predicted_timeseries <- function(datetimes, actual, predicted, model_name, n_show = 500) {
  idx <- seq_len(min(n_show, length(actual)))
  df <- data.frame(time = datetimes[idx], actual = actual[idx], predicted = predicted[idx])
  df_long <- data.frame(
    time = rep(df$time, 2),
    value = c(df$actual, df$predicted),
    series = rep(c("Actual", "Predicted"), each = nrow(df))
  )
  ggplot(df_long, aes(x = time, y = value, color = series)) +
    geom_line() +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "#e41a1c")) +
    labs(x = NULL, y = "Demand", color = NULL,
         title = paste("Actual vs predicted over time -", model_name)) +
    theme_minimal()
}

plot_weather_correlation <- function(df, weather_cols) {
  cm <- cor(df[weather_cols], use = "pairwise.complete.obs")
  cm_df <- as.data.frame(as.table(cm))
  names(cm_df) <- c("var1", "var2", "correlation")
  ggplot(cm_df, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, y = NULL, title = "Weather variable correlation")
}

plot_tuning_comparison <- function(before_rmse, after_rmse, model_name) {
  df <- data.frame(stage = factor(c("Default", "Tuned"), levels = c("Default", "Tuned")),
                   RMSE = c(before_rmse, after_rmse))
  ggplot(df, aes(x = stage, y = RMSE, fill = stage)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values = c("Default" = "#969696", "Tuned" = "#2c7fb8")) +
    labs(x = NULL, y = "RMSE (test set)",
         title = paste("Before vs after tuning -", model_name)) +
    theme_minimal() +
    theme(legend.position = "none")
}