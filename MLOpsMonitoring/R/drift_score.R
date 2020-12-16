#' Area Under the Curve : compute AUC between actual values and predictions
#'
#' @param actual : vector, actual values (target)
#' @param pred : vector, predicted probabilities 
#'
#' @return a value
#' @export
#' 
#' @import ROCR
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
#'
#' @return list of performance indicators (of drift model)
#' @export
#' 
#' @import mccr
#' @importFrom caret trainControl train
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
  return(list(matthews=matthews_score, auc=auc_drift, importance=varImp(rf, scale=FALSE)))
}


#' Drift Score : model detecting drift between 
#' train and test data sets
#'
#' @param X_train : data.table, train data set
#' @param start : start of the period
#' @param end : end of the period
#' @param windows : see ?MLOpsMonitoring::create_features_on_period
#' @param kind : see ?MLOpsMonitoring::create_features_on_period
#' 
#' @return list of performance indicators (of drift model)
#' 
#' @export
#' 
drift_futur <- function(X_train, start="2011-01-01", end="2011-12-31", windows, kind){
  list_months <- seq.Date(from = as.Date(start), to = as.Date(end), by="month")
  print(list_months)
  all_auc <- c()
  all_matthews <- c()
  X1 <- X_train[, -c("Customer.ID", "VAR_REP", "MONTH", "YEAR")]
  for(month in list_months){
    month <- as.Date(month, origin = "1970-01-01")
    target_start <- month
    target_end <- end_of_month(target_start)
    print(c(target_start, target_end))
    agg_ <- MLOpsMonitoring::create_features_on_period(data, target_start, target_end, windows, kind=kind)
    X2 <- agg_[, -c("Customer.ID", "VAR_REP", "MONTH", "YEAR")]
    res_drift_score <- drift_score(X1, X2)
    print(month)
    print(res_drift_score)
    all_auc <- c(all_auc, res_drift_score$auc)
    all_matthews <- c(all_matthews, res_drift_score$matthews)
  }
  
  return(data.table(MONTH = list_months, AUC = all_auc, MATTHEWS = all_matthews))
  
}