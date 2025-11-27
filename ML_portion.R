#install.packages("xgboost")
library(tidyverse)
library(xgboost)
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
    gamma = 0,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
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

pred_prob <- predict(model1, testing_input)
pred_prob <- matrix(pred_prob, ncol = 20, byrow = TRUE)
