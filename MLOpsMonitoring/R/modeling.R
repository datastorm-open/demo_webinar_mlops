library(tidyverse)
library(caret)
library(rpart)
library(xgboost)

source("MLOpsMonitoring/R/dataset.R")


train <- read_csv("../input/train.csv")
test<-read_csv("../input/test.csv")

smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)

train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

trctrl <- trainControl(method = "cv", number = 5)

# Takes a long to time to run in kaggle
tune_grid <- expand.grid(nrounds=c(100,200,300,400),
                         max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))

xgboost_fit <- train(SalePrice ~., data = train, method = "xgbTree",
                     trControl=trctrl,
                     tuneGrid = tune_grid,
                     tuneLength = 10)

# have a look at the model 
xgboost_fit

# Testing
test_predict <- predict(xgboost_fit, test)
