# library
library(tidyverse)
library(readr)
library(nnet)

# Importing Dataset
vC <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_Center.csv", 
                        col_types = cols(overall = col_integer(), 
                                         AWR = col_integer(), RBK = col_integer(), 
                                         RBP = col_integer(), RBF = col_integer(), 
                                         PBK = col_integer(), PBP = col_integer(), 
                                         PBF = col_integer(), IBL = col_integer(), 
                                         AGI = col_integer(), ACC = col_integer()))
View(vC)

# Train/test Split
n <- nrow(vC)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vC[idx_train,]
set_test <- vC[-idx_train,]

# PCA

X <- set_train[, c(
  "AWR", "RBK", "RBP", "RBF",
  "PBK", "PBP", "PBF", "IBL",
  "AGI", "ACC"
)]

pca <- prcomp(
  X,
  center = TRUE,
  scale. = TRUE
)

summary(pca)

# Model Creation w/ PCA
pca_data <- data.frame(
  overall = set_train$overall,
  pca$x
)

model_pca <- lm(
  overall ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
  data = pca_data
)

summary(model_pca)

# Model Creation w/ Step
full_model <- lm(
  overall ~ AWR + RBK + RBP + RBF + PBK + PBP + PBF + IBL + AGI + ACC,
  data = set_train
)

step_model <- step(
  full_model,
  direction = "both"
)

summary(step_model)

AIC(full_model, step_model)

# MLR Model Predicition
set_test$pred_mlr <- floor(predict(
  full_model,
  newdata = set_test
))

set_test[, c("overall", "pred_mlr")]
rmse_mlr <- sqrt(
  mean((set_test$overall - set_test$pred_mlr)^2)
)

rmse_mlr

mae_mlr <- mean(
  abs(set_test$overall - set_test$pred_mlr)
)

mae_mlr

r2_mlr <- 1 -
  sum((set_test$overall - set_test$pred_mlr)^2) /
  sum((set_test$overall - mean(set_test$overall))^2)

r2_mlr

# PCA Model Prediction
X_test <- set_test[, c(
  "AWR", "RBK", "RBP", "RBF",
  "PBK", "PBP", "PBF", "IBL",
  "AGI", "ACC"
)]

test_pc <- predict(
  pca,
  newdata = X_test
)

test_pc <- as.data.frame(test_pc)
set_test$pred_pca <- floor( predict(
  model_pca,
  newdata = test_pc
))

rmse_pca <- sqrt(
  mean((set_test$overall - set_test$pred_pca)^2)
)

rmse_pca

mae_pca <- mean(
  abs(set_test$overall - set_test$pred_pca)
)

mae_pca

r2_pca <- 1 -
  sum((set_test$overall - set_test$pred_pca)^2) /
  sum((set_test$overall - mean(set_test$overall))^2)

r2_pca

# Model Comparison
comparison <- data.frame(
  Model = c("Multiple Linear Regression", "PCA Regression"),
  RMSE = c(rmse_mlr, rmse_pca),
  MAE = c(mae_mlr, mae_pca),
  R_squared = c(r2_mlr, r2_pca)
)

comparison

within1_mlr <- mean(
  abs(set_test$overall - set_test$pred_mlr) <= 1
)

within1_pca <- mean(
  abs(set_test$overall - set_test$pred_pca) <= 1
)

# New Player Predictor
new_player <- data.frame(
  AWR = 80,
  RBK = 73,
  RBP = 73,
  RBF = 75,
  PBK = 81,
  PBP = 81,
  PBF = 80,
  IBL = 67,
  AGI = 67,
  ACC = 63
) 

new_player_pc <- predict(
  pca,
  newdata = new_player
)

new_player_pc <- as.data.frame(new_player_pc)
predicted_overall <- floor(predict(
  model_pca,
  newdata = new_player_pc
))

predicted_overall