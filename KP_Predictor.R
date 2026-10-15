# library
library(tidyverse)
library(readr)
library(nnet)

# Import Dataset
vKP <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_KP.csv", 
                 col_types = cols(overall = col_integer(), 
                                  KPW = col_integer(), KAC = col_integer(), 
                                  AWR = col_integer(), SPD = col_integer(), 
                                  ACC = col_integer(), AGI = col_integer(), 
                                  BTK = col_integer(), THP = col_integer(), 
                                  RUN = col_integer(), SAC = col_integer()))
#View(vDE)

# Train/test Split
n <- nrow(vKP)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vKP[idx_train,]
set_test <- vKP[-idx_train,]

# PCA

X <- set_train[, c(
  "KPW", "KAC", "AWR", "SPD",
  "ACC", "AGI", "BTK", "THP",
  "RUN", "SAC"
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
  "KPW", "KAC", "AWR", "SPD",
  "ACC", "AGI", "BTK", "THP",
  "RUN", "SAC"
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
  "KPW" = ,
  "KAC" = , 
  "AWR" = , 
  "SPD" = ,
  "ACC" = , 
  "AGI" = , 
  "BTK" = , 
  "THP" = ,
  "RUN" = , 
  "SAC" = 
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