library(dplyr)
library(data.table)
library(tidyr)
library(caret)
library(tidyverse)
library(randomForest)

data <- fread("C:/Users/Andrew/Downloads/Lottery_Powerball_Winning_Numbers__Beginning_2010.csv")

separated_data <- separate(data, col = `Winning Numbers`, into = c(paste0("number", 1:5), "Powerball"), sep = " ")

lottery_data <- separated_data

# Convert Draw Date to Date type
lottery_data$`Draw Date` <- as.Date(lottery_data$`Draw Date`, format="%m/%d/%Y")

# Extract features from Draw Date
lottery_data <- lottery_data %>%
  mutate(Year = as.numeric(format(`Draw Date`, "%Y")),
         Month = as.numeric(format(`Draw Date`, "%m")),
         Day = as.numeric(format(`Draw Date``, "%d")))

# Function to train a model for each number position
train_model_rf <- function(data, position) {
  data <- data %>%
    select(Year, Month, Day, all_of(position)) %>%
    rename(value = all_of(position)) %>%
    mutate(value = factor(value))
  
  # Train the model
  model <- train(value ~ ., data = data, method = "rf", trControl = trainControl(method = "cv", number = 5))
  
  return(model)
}

# Train models for each number position
models <- list()
for (i in 1:5) {
  models[[paste0("number", i)]] <- train_model_rf(lottery_data, paste0("number", i))
}

# Train a model for the Powerball
powerball_model <- train_model_rf(lottery_data, "Powerball")

# Function to predict the next draw numbers
predict_next_draw_rf <- function(models, powerball_model) {
  next_draw <- c()
  
  # Current date features
  current_date_features <- data.frame(Year = as.numeric(format(Sys.Date(), "%Y")),
                                      Month = as.numeric(format(Sys.Date(), "%m")),
                                      Day = as.numeric(format(Sys.Date(), "%d")))
  
  for (i in 1:5) {
    pred <- predict(models[[paste0("number", i)]], newdata = current_date_features)
    next_draw <- c(next_draw, as.numeric(levels(pred))[pred])
  }
  
  # Predict the Powerball number
  powerball_pred <- predict(powerball_model, newdata = current_date_features)
  powerball <- as.numeric(levels(powerball_pred))[powerball_pred]
  
  return(c(next_draw, powerball))
}

# Predict the next draw
next_draw <- predict_next_draw_rf(models, powerball_model)
print(next_draw)
