source("R/config.R")
source("R/models.R")

t0 <- Sys.time()
tick <- function(l) cat(sprintf("[%s] %s (%.1fs)\n", format(Sys.time(),"%H:%M:%S"), l, as.numeric(Sys.time()-t0, units="secs")))

ck <- readRDS("outputs/step1.rds")
feat <- ck$feat; final_features <- ck$final_features; profile <- ck$profile

splits <- chronological_split(feat, profile)
cat(sprintf("Train: %d | Val: %d | Test: %d\n", nrow(splits$train), nrow(splits$val), nrow(splits$test)))
tick("split done")

result <- train_all_models(splits, profile, final_features)
tick("all models trained")

cat("\n---- Model comparison (sorted by RMSE) ----\n")
print(result$metrics, row.names = FALSE)

saveRDS(result, "outputs/panama_result.rds")
write.csv(result$metrics, "outputs/panama_model_comparison.csv", row.names = FALSE)
tick("saved")