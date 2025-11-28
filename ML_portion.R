#install.packages("xgboost")
library(tidyverse)
library(xgboost)
library(caret)
unzip("features.zip")
features_df <- read_csv("features.csv", col_names = TRUE)

View(features_df)

dim(features_df)

# drop na
features_df <- features_df %>% drop_na()

# aim to predict player actions based on current game state (20 possibilities; hint specific color/number, play specific number, discard specific number)
# model will only be applicable for games w/ totalpoints >= 20 (as data is only from games where totalpoints >= 20)
# last 20 cols of features are player actions (1-hot encoded), which is what we want to predict


# converting the potential actions (binary cols) to multilabel factor 0-19 since xgb is 0 indexed
y <- apply(features_df[ , tail(names(features_df), 20)], MARGIN = 1, which.max) - 1

action_cols <- tail(names(features_df), 20)

# class labels
action_map <- data.frame(
    class_id = 0:19,
    action_col = action_cols
)
action_map

features_df$label <- y

set.seed(111)
training_rows <- sample(seq_len(nrow(features_df)), size = 0.8 * nrow(features_df))
train_df <- features_df[training_rows, ]
test_df <- features_df[-training_rows, ]

training_input <- xgb.DMatrix(data = as.matrix(train_df[, 1:373]), label= train_df$label)
testing_input <- xgb.DMatrix(data = as.matrix(test_df[,1:373]), label= test_df$label)

# now building first model
parameters <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = 20,
    eval_metric = "mlogloss",
    max_depth = 6,
    min_child_weight = 1,
    gamma = 1,
    eta = 0.1,
    subsample = 0.7,
    colsample_bytree = 0.7
)

watchlist <- list(
  train = training_input,
  eval = testing_input
)


model1 <- xgb.train(
  params = parameters,
  data = training_input,
  nrounds = 1000,  
  watchlist = watchlist,
  early_stopping_rounds = 30,
  print_every_n = 30,
  nthread = parallel::detectCores(),
  tree_method = "hist"
)

importance_matrix <- xgb.importance(
  feature_names = colnames(train_df[, 1:373]),
  model = model1
)
print(importance_matrix[1:20, ])
xgb.plot.importance(
  importance_matrix = importance_matrix[1:20, ],
  main = "top 20 feature importance"
)

pred_probs <- predict(model1, testing_input)
pred_probs_matrix <- matrix(pred_probs, nrow = length(pred_probs) / 20, ncol = 20, byrow = TRUE)
pred_class <- max.col(pred_probs_matrix) - 1

test_df$pred_classes <- pred_class
View(test_df)

conf_mat <- confusionMatrix(
  data = factor(test_df$pred_classes),
  reference = factor(test_df$label)
)
conf_mat

f1_scores_per_class <- conf_mat$byClass[, "F1"]
macro_f1 <- mean(f1_scores_per_class)
macro_f1


top_k_accuracy_xgb <- function(pred_matrix, true_labels, k) {
  top_k_preds <- apply(pred_matrix, 1, function(x) order(x, decreasing = TRUE)[1:k])
  correct <- sapply(1:length(true_labels), function(i) {
    (true_labels[i] + 1) %in% top_k_preds[, i]
  })
  mean(correct)
}

top_k_accuracy_xgb(pred_probs_matrix, test_df$label, 3)
