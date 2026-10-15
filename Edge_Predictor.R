# library
library(tidyverse)
library(readr)
library(nnet)

# Import Dataset
vDE <- read_csv("~/Desktop/Personal Coding Projects/EACFB27 Player Rating Predictor/view_Edge.csv", 
                col_types = cols(overall = col_integer(), 
                                 AWR = col_integer(), STR = col_integer(), 
                                 ACC = col_integer(), BSH = col_integer(), 
                                 TAK = col_integer(), POW = col_integer(), 
                                 PMV = col_integer(), FMV = col_integer(), 
                                 SPD = col_integer(), PUR = col_integer()))
#View(vDE)

# Train/test Split
n <- nrow(vDE)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))
set_train <- vDE[idx_train,]
set_test <- vDE[-idx_train,]

# PCA

X <- set_train[, c(
  "AWR", "STR", "ACC", "BSH",
  "TAK", "POW", "PMV", "FMV",
  "SPD", "PUR"
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
  "AWR", "STR", "ACC", "BSH",
  "TAK", "POW", "PMV", "FMV",
  "SPD", "PUR"
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
  STR = 74,
  ACC = 83,
  BSH = 68,
  TAK = 82,
  POW = 85,
  PMV = 62,
  FMV = 72,
  SPD = 80,
  PUR = 81
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

#overall distributions
s = 5
hist(vDE$overall, col = 'blue', breaks = seq(floor(min(vDE$overall) / s) * s, ceiling(max(vDE$overall) / s) * s, by = s))
hist(set_train$overall, col = "red", breaks = seq(floor(min(vDE$overall) / s) * s, ceiling(max(vDE$overall) / s) * s, by = s), add = TRUE)
hist(set_test$overall, col = 'green', breaks = seq(floor(min(vDE$overall) / s) * s, ceiling(max(vDE$overall) / s) * s, by = s), add = TRUE)


# plot matrix
variables <- c(
  "AWR", 
  "STR", 
  "ACC", 
  "BSH",
  "TAK", 
  "POW", 
  "PMV", 
  "FMV",
  "SPD", 
  "PUR"
)

par(mfrow = c(1, 1))

for (var in variables) {
  plot(vDE[[var]], vDE$overall,
       xlab = var,
       ylab = "Overall",
       main = paste(var, "vs Overall"))
}