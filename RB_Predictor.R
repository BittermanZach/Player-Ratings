# library
library(tidyverse)
library(readr)
library(nnet)

# Import Dataset
vRB <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_RB.csv", 
                col_types = cols(overall = col_integer(), 
                                 AWR = col_integer(), CAR = col_integer(), 
                                 BTK = col_integer(), COD = col_integer(), 
                                 JKM = col_integer(), SPM = col_integer(), 
                                 BCV = col_integer(), CTH = col_integer(), 
                                 SPD = col_integer(), ACC = col_integer()))
#View(vRB)

# Train/test Split
n <- nrow(vRB)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vRB[idx_train,]
set_test <- vRB[-idx_train,]

# PCA

X <- set_train[, c(
  "AWR", "CAR", "BTK", "COD",
  "JKM", "SPM", "BCV", "CTH",
  "SPD", "ACC"
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
  "AWR", "CAR", "BTK", "COD",
  "JKM", "SPM", "BCV", "CTH",
  "SPD", "ACC"
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
  CAR = 74,
  BTK = 83,
  COD = 68,
  JKM = 82,
  SPM = 85,
  BCV = 62,
  CTH = 72,
  SPD = 80,
  ACC = 81
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