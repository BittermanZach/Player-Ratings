# library
library(tidyverse)
library(readr)
library(nnet)

# Import Dataset
vTE <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_TE.csv", 
                col_types = cols(overall = col_integer(), 
                                 AWR = col_integer(), SPD = col_integer(), 
                                 STR = col_integer(), ACC = col_integer(), 
                                 RBK = col_integer(), PBK = col_integer(), 
                                 CTH = col_integer(), CIT = col_integer(), 
                                 SRR = col_integer(), MRR = col_integer()))
#View(vTE)

# Train/test Split
n <- nrow(vTE)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vTE[idx_train,]
set_test <- vTE[-idx_train,]

# PCA

X <- set_train[, c(
  "AWR", "SPD", "STR", "ACC",
  "RBK", "PBK", "CTH", "CIT",
  "SRR", "MRR"
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
  "AWR", "SPD", "STR", "ACC",
  "RBK", "PBK", "CTH", "CIT",
  "SRR", "MRR"
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
  STR = 83,
  ACC = 68,
  RBK = 82,
  PBK = 85,
  CTH = 62,
  CIT = 72,
  SRR = 80,
  MRR = 81
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