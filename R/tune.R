library(ranger)
library(nnet)

tune_random_forest <- function(tr, val, feature_cols, demand_col) {
  form <- as.formula(paste(demand_col, "~", paste(feature_cols, collapse = " + ")))
  y_val <- val[[demand_col]]
  
  grid <- expand.grid(mtry = c(4, 7, floor(length(feature_cols) / 2)),
                      min.node.size = c(1, 5, 10))
  grid <- unique(grid)
  
  results <- lapply(seq_len(nrow(grid)), function(i) {
    fit <- ranger(form, data = tr, num.trees = 300,
                  mtry = grid$mtry[i], min.node.size = grid$min.node.size[i],
                  verbose = FALSE)
    pred <- predict(fit, val)$predictions
    rmse <- sqrt(mean((y_val - pred)^2))
    c(mtry = grid$mtry[i], min.node.size = grid$min.node.size[i], RMSE = rmse)
  })
  
  results <- as.data.frame(do.call(rbind, results))
  results[order(results$RMSE), ]
}

tune_mlp <- function(tr, val, feature_cols, demand_col) {
  y_tr <- tr[[demand_col]]
  y_val <- val[[demand_col]]
  x_tr <- as.matrix(tr[, feature_cols])
  x_val <- as.matrix(val[, feature_cols])
  
  scale_rng <- range(y_tr)
  y_tr_sc <- (y_tr - scale_rng[1]) / diff(scale_rng)
  x_tr_sc <- scale(x_tr)
  x_val_sc <- scale(x_val, center = attr(x_tr_sc, "scaled:center"),
                    scale  = attr(x_tr_sc, "scaled:scale"))
  
  grid <- expand.grid(size = c(5, 8, 12), decay = c(0, 0.001, 0.01))
  
  results <- lapply(seq_len(nrow(grid)), function(i) {
    fit <- nnet(x = x_tr_sc, y = y_tr_sc, size = grid$size[i], decay = grid$decay[i],
                linout = TRUE, maxit = 300, trace = FALSE)
    pred_sc <- predict(fit, x_val_sc)
    pred <- pred_sc * diff(scale_rng) + scale_rng[1]
    rmse <- sqrt(mean((y_val - pred)^2))
    c(size = grid$size[i], decay = grid$decay[i], RMSE = rmse)
  })
  
  results <- as.data.frame(do.call(rbind, results))
  results[order(results$RMSE), ]
}