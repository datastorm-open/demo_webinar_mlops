#' Train a RandomForest model. Compute AUC.
#'
#' @param agg : \code{data.table}. Features and target.
#' 
#' 
#' @return a list of a RF Object, AUC, predictions
#'
#' @import data.table
#' @import caret
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' agg_4trim <- create_features_on_period(data, "2011-05-01", "2011-05-30", c("T-1", "T-2", "T-3", "T-4"), kind="lag")
#' # Distribution of VAR_REP 
#' agg_4trim[, .N/nrow(agg_4trim), VAR_REP]
#' # Train RF
#' res_4trim <- train_rf(agg_4trim)
#' # Importance
#' varImp(res_4trim$model)
#' plot(varImp(res_4trim$model), top=30)
#' res_4trim$auc
#' }
#'
train_rf <- function(agg, print_rf=TRUE, seed=28, rep_factor=TRUE){
  agg <- apply(agg, 2,  function(x) replace(x, is.infinite(x), NA))
  agg[is.na(agg)] <- 0
  agg <- as.data.table(agg)
  
  set.seed(seed)
  customer_validation <- sample(agg$Customer.ID, round(length(unique(agg$Customer.ID))*0.25))
  agg_val <- agg[Customer.ID %in% customer_validation]
  agg_train <- agg[!(Customer.ID %in% customer_validation)]
  
  X_train <- agg_train[, -c("Customer.ID", "VAR_REP", "YEAR")]
  X_val <- agg_val[, -c("Customer.ID", "VAR_REP", "YEAR")]
  y_train <- agg_train$VAR_REP
  y_val <- agg_val$VAR_REP
  if(rep_factor){
    y_train <- as.factor(y_train)
    y_val <- as.factor(y_val) 
  }

  set.seed(seed)
  train_control <- caret::trainControl(method="cv", number=4)
  rf <- caret::train(x=X_train, 
                     y=y_train, 
                     trControl=train_control, method="rf", ntree=100)
  if(print_rf) {print(rf)}
  if(rep_factor){
    pred_validation <- predict(rf, X_val, type="prob")[, 2]
    metric_auc <- auc(actual = agg_val$VAR_REP, pred = pred_validation)
    metric_accuracy <- accuracy(actual = agg_val$VAR_REP, pred = pred_validation, best=TRUE)
    return(list(rf=rf, auc=metric_auc, acc=metric_accuracy$acc, threshold=metric_accuracy$threshold,
                pred=pred_validation, id_val=customer_validation))
  }
  else{
    pred_validation <- predict(rf, X_val)
    metric_rmse <- Metrics::rmse(actual = agg_val$VAR_REP, predicted = pred_validation)
    metric_mae <- Metrics::mae(actual = agg_val$VAR_REP, predicted = pred_validation)
    return(list(rf=rf, rmse=metric_rmse, mae=metric_mae,
                pred=pred_validation, id_val=customer_validation))
  }
  
  
}

#' Train a RandomForest model. Compute AUC.
#'
#' @param agg : \code{data.table}. Features and target.
#' 
#' 
#' @return a list of a RF Object, AUC, predictions
#'
#' @import data.table
#' @import xgboost
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#' }
#'
train_xgboost <- function(agg){
  set.seed(28)
  customer_validation <- sample(agg$Customer.ID, round(length(unique(agg$Customer.ID))*0.25))
  agg_val <- agg[Customer.ID %in% customer_validation]
  agg_train <- agg[!(Customer.ID %in% customer_validation)]
  
  X_train <- agg_train[, -c("Customer.ID", "VAR_REP")]
  y_train <- as.factor(agg_train$VAR_REP)
  X_val <- agg_val[, -c("Customer.ID", "VAR_REP")]
  y_val <- as.factor(agg_val$VAR_REP)
  train_control <- caret::trainControl(method="cv", number=4)
  # tune_grid <- expand.grid(nrounds=c(100,200,300,400),
  #                          max_depth = c(3:7),
  #                          eta = c(0.05, 1),
  #                          gamma = c(0.01),
  #                          colsample_bytree = c(0.75),
  #                          subsample = c(0.50),
  #                          min_child_weight = c(0))
  tune_grid <- expand.grid(nrounds=c(200),
                           max_depth = c(3),
                           eta = c(0.05),
                           gamma = c(0.01),
                           colsample_bytree = c(0.75),
                           subsample = c(0.50),
                           min_child_weight = c(0))
  
  xgboost <- train(x=X_train, y=y_train, 
                   method = "xgbTree",
                   trControl=train_control,
                   tuneGrid = tune_grid,
                   tuneLength = 10)
  
  pred_validation <- predict(xgboost, X_val, type="prob")[, 2]
  metric_auc <- auc(actual = agg_val$VAR_REP, pred = pred_validation)
  return(list(model=xgboost, auc=metric_auc, pred=pred_validation))
}
