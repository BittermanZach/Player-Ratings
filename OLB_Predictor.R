# library
library(tidyverse)
library(readr)
library(nnet)

# Import Dataset
vMLB <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_MLB.csv", 
                 col_types = cols(overall = col_integer(), 
                                  AWR = col_integer(), SPD = col_integer(), 
                                  ACC = col_integer(), STR = col_integer(), 
                                  PRC = col_integer(), TAK = col_integer(), 
                                  POW = col_integer(), PUR = col_integer(), 
                                  MCV = col_integer(), ZCV = col_integer()))
#View(vDE)

# Train/test Split
n <- nrow(vMLB)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vMLB[idx_train,]
set_test <- vMLB[-idx_train,]

# PCA

X <- set_train[, c(
  "AWR", "SPD", "ACC", "STR",
  "PRC", "TAK", "POW", "PUR",
  "MCV", "ZCV"
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
  "AWR", "SPD", "ACC", "STR",
  "PRC", "TAK", "POW", "PUR",
  "MCV", "ZCV"
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

# Range of values
within1_pca <- mean(
  abs(set_test$overall - set_test$pred_pca) <= 1
)
within2_pca <- mean(
  abs(set_test$overall - set_test$pred_pca) <= 2
)

# New Player Predictor
new_player <- data.frame(
  AWR = 60,
  SPD = 74,
  ACC = 83,
  STR = 68,
  PRC = 82,
  TAK = 85,
  POW = 62,
  PUR = 72,
  MCV = 80,
  ZCV = 81
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