library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(randomForest)

setwd("C:/Users/Uzytkownik/Documents/R courses edX/Harvard/capstone/own project")
data <- read.csv("online_shoppers_intention.csv",header=TRUE)


# removing any rows with NA value
data <- data[complete.cases(data), ]

y <- data$Revenue

data$Revenue <- as.factor(data$Revenue)
# Create trainset and test set 
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
test_set <- data[test_index, ]
train_set <- data[-test_index, ]

# train dataset with method Random Forest using caret package
train_rf <- train(Revenue~., method = 'rf',data = train_set)
y_hat_knn <- predict(train_rf, test_set,type = "raw")

confusionMatrix(data=y_hat_knn, reference=test_set$Revenue)$overall["Accuracy"]
