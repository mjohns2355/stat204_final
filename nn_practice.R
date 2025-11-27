install.packages("keras3")
#Load the necessary libraries
library(tidyverse)
library(dplyr)
library(keras3)

features_data <- read_csv("features.zip", "features.csv")
features_complete <- features_data[complete.cases(features_data),]

# remove first row, it has text labels
first_row <- features_complete[1, ]
# data frame with all the 1s and 0s
df_num <- as.data.frame(lapply(features_complete[-1, ], as.numeric))

#df_num_complete <- complete.cases(df_num)

# calculate number of samples we have
num_samples = nrow(df_num)

# Split the dataset into training and testing sets
# df_num is a data frame: 192731 x 393
# all data is one-hot-encoded categorical, all 1 or 0
split_ratio <- 0.8
num_train_samples <- floor(num_samples * split_ratio)
train_data <- df_num[1:num_train_samples, ]
test_data <- df_num[(num_train_samples + 1):num_samples, ]

# Prepare the data for training
# train_data is a data frame: 154184 x 393
# 373 columns are features, the last 20 are the "action" we are predicting
train_features <- as.matrix(train_data[, 1:373])
train_labels <- as.matrix(train_data[, 374:393])
test_features <- as.matrix(test_data[, 1:373])
test_labels <- as.matrix(test_data[, 374:393])

# Build the neurla network model
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = ncol(train_features)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = ncol(train_labels), activation = "softmax") # Use 'sigmoid' if multi-label

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy", # Use 'binary_crossentropy' for multi-label with sigmoid
  metrics = c("accuracy")
)

# Print the model summary
summary(model)

# Train the model
history <- model %>% fit(
  x = train_features,
  y = train_labels,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Display results

history
plot(history)


predictions <- model %>% predict(test_features)

eval_result <- model %>% evaluate(
  x = test_features,
  y = test_labels
)

cat("Test accuracy:", eval_result$accuracy, "\n")
cat("Test loss:", eval_result$loss, "\n")

first_row[373 + which.max(predictions[1,])]

first_row[373 + which.max(predictions[2,])]

