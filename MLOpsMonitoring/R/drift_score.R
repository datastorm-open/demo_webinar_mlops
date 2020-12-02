#' Area Under the Curve : compute AUC between actual values and predictions
#'
#' @param actual : vector, actual values (target)
#' @param pred : vector, predicted probabilities 

#' @return a value
#' 
auc <- function(actual, pred){
  pred_ROCR <- ROCR::prediction(pred, actual)
  auc_ROCR <- ROCR::performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  return(auc_ROCR)
}


#' Drift Score : model detecting drift between 
#' train and test data sets
#'
#' @param X_train : data.table, train data set
#' @param X_test : data.table, test data set

#' @return list of performance indicators (of drift model)
#' 
drift_score <- function(X_train, X_test){
  X_train_copy <- copy(X_train)
  X_test_copy <- copy(X_test)
  X_train_copy[, Y_DRIFTSCORE := 1]
  X_test_copy[, Y_DRIFTSCORE := 0]
  df_drift <- rbind(X_train_copy, X_test_copy)
  df_drift[, Y_DRIFTSCORE := as.factor(Y_DRIFTSCORE)]
  df_drift <- df_drift[sample(x=nrow(df_drift), 
                              size=nrow(df_drift), 
                              replace=FALSE), ]
  train_control <- caret::trainControl(method="cv", number=4)
  rf <- caret::train(x=df_drift[, -c("Y_DRIFTSCORE")], 
                                   y=df_drift[["Y_DRIFTSCORE"]], 
                     trControl=train_control, method="rf", ntree=50)
  # print(rf)
  pred_drift <- predict(rf, df_drift[, -c("Y_DRIFTSCORE")], type="prob")[, 2]
  matthews_score <- mccr::mccr(df_drift$Y_DRIFTSCORE, ifelse(pred_drift >= 0.5, 1, 0))
  auc_drift <- auc(df_drift$Y_DRIFTSCORE, pred_drift)
  return(list(matthews=matthews_score, auc=auc_drift))
}
