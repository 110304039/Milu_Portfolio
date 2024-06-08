library(xgboost)
library(caret)
library(Matrix)

GiveMeSomeCredit <- function(train_file, test_file, out_file){
  
  train <- read.csv(train_file)
  test <- read.csv(test_file)
  
  traindata$MonthlyIncome[is.na(traindata$MonthlyIncome)] <- median(traindata$MonthlyIncome, na.rm = TRUE)
  traindata$NumberOfDependents[is.na(traindata$NumberOfDependents)] <- median(traindata$NumberOfDependents, na.rm = TRUE)
  testdata$MonthlyIncome[is.na(testdata$MonthlyIncome)] <- median(testdata$MonthlyIncome, na.rm = TRUE)
  testdata$NumberOfDependents[is.na(testdata$NumberOfDependents)] <- median(testdata$NumberOfDependents, na.rm = TRUE)
  
  traindata$SeriousDlqin2yrs <- as.numeric(traindata$SeriousDlqin2yrs)
  
  trainMatrix <- xgb.DMatrix(data = as.matrix(traindata[, -c(1, 2)]), label = traindata$SeriousDlqin2yrs)
  testMatrix <- xgb.DMatrix(data = as.matrix(testdata[, -c(1, 2)]))
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "auc",
    eta = 0.1,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  
  set.seed(123)
  xgb_model <- xgb.train(
    params = params,
    data = trainMatrix,
    nrounds = 100,
    watchlist = list(train = train_matrix),
    verbose = 1
  )
  
  predictions <- predict(xgb_model, newdata = testMatrix)
  result <- data.frame(ID = test$X, Probability = predictions)

  write.csv(result, file = out_file, row.names = FALSE)
}





