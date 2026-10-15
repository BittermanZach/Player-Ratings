# library
library(tidyverse)
library(readr)
library(nnet)

# Importing Dataset
vOT <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_OT.csv", 
                col_types = cols(overall = col_integer(), 
                                 AWR = col_integer(), RBK = col_integer(), 
                                 RBP = col_integer(), RBF = col_integer(), 
                                 PBK = col_integer(), PBP = col_integer(), 
                                 PBF = col_integer(), IBL = col_integer(), 
                                 AGI = col_integer(), ACC = col_integer()))
#View(vOT)

# Train/test Split
n <- nrow(vOT)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vOT[idx_train,]
set_test <- vOT[-idx_train,]

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
  overall ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8,
  data = pca_data
)

summary(model_pca)

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

within1_pca <- mean(
  abs(set_test$overall - set_test$pred_pca) <= 1
)
within2_pca <- mean(
  abs(set_test$overall - set_test$pred_pca) <= 2
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