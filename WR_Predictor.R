# library
library(tidyverse)
library(readr)
library(nnet)

# Import Dataset
vWR <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_WR.csv", 
                col_types = cols(overall = col_integer(), 
                                 AWR = col_integer(), SPD = col_integer(), 
                                 ACC = col_integer(), CTH = col_integer(), 
                                 CIT = col_integer(), SPC = col_integer(), 
                                 SRR = col_integer(), MRR = col_integer(), 
                                 DRR = col_integer()))
#View(vWR)

# Train/test Split
n <- nrow(vWR)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vWR[idx_train,]
set_test <- vWR[-idx_train,]

# PCA

X <- set_train[, c(
  "AWR", "SPD", "ACC", "CTH",
  "CIT", "SPC", "SRR", "MRR",
  "DRR"
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
  overall ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7,
  data = pca_data
)

summary(model_pca)

# PCA Model Prediction
X_test <- set_test[, c(
  "AWR", "SPD", "ACC", "CTH",
  "CIT", "SPC", "SRR", "MRR",
  "DRR"
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
  CTH = 68,
  CIT = 82,
  SPC = 85,
  SRR = 62,
  MRR = 72,
  DRR = 80
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