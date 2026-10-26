source("R/config.R")
source("R/models.R")
source("R/tune.R")

t0 <- Sys.time()
tick <- function(l) cat(sprintf("[%s] %s (%.1fs)\n", format(Sys.time(),"%H:%M:%S"), l, as.numeric(Sys.time()-t0, units="secs")))

ck <- readRDS("outputs/step1.rds")
feat <- ck$feat; final_features <- ck$final_features; profile <- ck$profile
splits <- chronological_split(feat, profile)

tr  <- splits$train[stats::complete.cases(splits$train[, c(final_features, profile$demand_col)]), ]
val <- splits$val[stats::complete.cases(splits$val[, c(final_features, profile$demand_col)]), ]

mlp_results <- tune_mlp(tr, val, final_features, profile$demand_col)
tick("mlp tuning done")
cat("MLP grid results:\n"); print(mlp_results, row.names = FALSE)

saveRDS(mlp_results, "outputs/tuning_mlp.rds")
tick("saved")