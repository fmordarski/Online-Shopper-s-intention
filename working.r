if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("data.table", repos = "http://cran.us.r-project.org")
# Introduction

## provide below path to working directory
setwd("C:/Users/filip-mordarski/Documents/filip/capstone/own project/Online-Shopper-s-intention-master")

##loading data
data <- read.csv("online_shoppers_intention.csv",header=TRUE)


# Analysis

## Data cleaning

# removing any rows with NA value
data <- data[complete.cases(data), ]

# change type of 'Revenue' from boolean to factor



y <- data$Revenue

# Create trainset and test set 
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
test_set <- data[test_index, ]
train_set <- data[-test_index, ]

## Data exploration and visualization

# table with frequency percentage if shopper will generate revenue

table_freq <- data %>%
  summarize(FreqPercTrue=mean(Revenue),FreqPercFalse=1-mean(Revenue)) 


# train dataset with method Random Forest using caret package
train_set$Revenue <- as.factor(train_set$Revenue)
test_set$Revenue <- as.factor(test_set$Revenue)
train_rf <- train(Revenue~., method = 'rf',data = train_set)
y_hat_knn <- predict(train_rf, test_set,type = "raw")

confusionMatrix(data=y_hat_knn, reference=test_set$Revenue)$overall["Accuracy"]
