library(glmnet)
library(rpart)
library(ranger)
library(gbm)
library(e1071)
library(FNN)
library(nnet)
library(forecast)

chronological_split <- function(df, profile) {
  dt <- df[[profile$datetime_col]]
  train_end <- as.POSIXct(profile$train_end, tz = "UTC")
  val_end   <- as.POSIXct(profile$val_end, tz = "UTC")
  list(
    train = df[dt <= train_end, ],
    val   = df[dt > train_end & dt <= val_end, ],
    test  = df[dt > val_end, ]
  )
}

metrics <- function(actual, predicted) {
  err <- actual - predicted
  mae  <- mean(abs(err), na.rm = TRUE)
  rmse <- sqrt(mean(err^2, na.rm = TRUE))
  mape <- mean(abs(err / actual), na.rm = TRUE) * 100
  ss_res <- sum(err^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  c(MAE = mae, RMSE = rmse, MAPE = mape, R2 = r2)
}

train_all_models <- function(
    splits,
    profile,
    feature_cols,
    rf_params = list(mtry = 7, min.node.size = 10)
) {
  form <- as.formula(paste(profile$demand_col, "~", paste(feature_cols, collapse = " + ")))
  
  tr <- splits$train[stats::complete.cases(splits$train[, c(feature_cols, profile$demand_col)]), ]
  te <- splits$test[stats::complete.cases(splits$test[, c(feature_cols, profile$demand_col)]), ]
  
  x_tr <- as.matrix(tr[, feature_cols]); y_tr <- tr[[profile$demand_col]]
  x_te <- as.matrix(te[, feature_cols]); y_te <- te[[profile$demand_col]]
  
  models <- list()
  preds  <- list()
  
  models$linear <- lm(form, data = tr)
  preds$linear  <- predict(models$linear, te)
  
  models$ridge <- glmnet(x_tr, y_tr, alpha = 0)
  preds$ridge  <- as.numeric(predict(models$ridge, x_te, s = min(models$ridge$lambda)))
  
  models$lasso <- glmnet(x_tr, y_tr, alpha = 1)
  preds$lasso  <- as.numeric(predict(models$lasso, x_te, s = min(models$lasso$lambda)))
  
  models$elastic_net <- glmnet(x_tr, y_tr, alpha = 0.5)
  preds$elastic_net  <- as.numeric(predict(models$elastic_net, x_te, s = min(models$elastic_net$lambda)))
  
  models$decision_tree <- rpart(form, data = tr)
  preds$decision_tree  <- predict(models$decision_tree, te)
  
  models$random_forest <- ranger(
    form,
    data = tr,
    num.trees = 300,
    mtry = min(rf_params$mtry, length(feature_cols)),
    min.node.size = rf_params$min.node.size,
    splitrule = "variance",
    importance = "impurity"
  )
  
  preds$random_forest <-
    predict(models$random_forest, te)$predictions
  
  models$extra_trees <- ranger(form, data = tr, num.trees = 300, splitrule = "extratrees")
  preds$extra_trees  <- predict(models$extra_trees, te)$predictions
  
  models$gbm <- gbm(form, data = tr, distribution = "gaussian",
                    n.trees = 300, interaction.depth = 3,
                    shrinkage = 0.01, bag.fraction = 0.5, verbose = FALSE)
  preds$gbm  <- predict(models$gbm, te, n.trees = 300)
  
  models$svr <- svm(form, data = tr, kernel = "radial")
  preds$svr  <- predict(models$svr, te)
  
  knn_fit <- knn.reg(train = x_tr, test = x_te, y = y_tr, k = 10)
  models$knn <- knn_fit
  preds$knn  <- knn_fit$pred
  
  scale_rng <- range(y_tr)
  y_tr_sc <- (y_tr - scale_rng[1]) / diff(scale_rng)
  x_tr_sc <- scale(x_tr)
  x_te_sc <- scale(x_te, center = attr(x_tr_sc, "scaled:center"),
                   scale  = attr(x_tr_sc, "scaled:scale"))
  models$mlp <- nnet(x = x_tr_sc, y = y_tr_sc, size = 8, linout = TRUE,
                     maxit = 300, trace = FALSE)
  preds$mlp <- predict(models$mlp, x_te_sc) * diff(scale_rng) + scale_rng[1]
  
  full_y <- tr[[profile$demand_col]]
  arima_window <- min(length(full_y), 90 * profile$seasonal_period)
  y_ts_full   <- ts(full_y, frequency = profile$seasonal_period)
  y_ts_recent <- ts(tail(full_y, arima_window), frequency = profile$seasonal_period)
  
  models$seasonal_naive <- snaive(y_ts_full, h = nrow(te))
  preds$seasonal_naive  <- as.numeric(models$seasonal_naive$mean)[seq_len(nrow(te))]
  
  models$arima <- auto.arima(y_ts_recent, seasonal = TRUE, stepwise = TRUE,
                             approximation = TRUE, max.p = 3, max.q = 3,
                             max.P = 1, max.Q = 1)
  fc <- forecast(models$arima, h = nrow(te))
  preds$arima <- as.numeric(fc$mean)[seq_len(nrow(te))]
  
  metric_tbl <- do.call(rbind, lapply(names(preds), function(nm) {
    c(model = nm, metrics(y_te, preds[[nm]]))
  }))
  metric_tbl <- as.data.frame(metric_tbl, stringsAsFactors = FALSE)
  for (col in c("MAE", "RMSE", "MAPE", "R2")) {
    metric_tbl[[col]] <- as.numeric(metric_tbl[[col]])
  }
  metric_tbl$is_ml_dl <- !(metric_tbl$model %in% c("seasonal_naive", "arima"))
  metric_tbl <- metric_tbl[order(metric_tbl$RMSE), ]
  
  list(models = models, preds = preds, actual = y_te, metrics = metric_tbl)
}