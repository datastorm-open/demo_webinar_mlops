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
train_rf <- function(agg){
  set.seed(28)
  customer_validation <- sample(agg$Customer.ID, round(length(unique(agg$Customer.ID))*0.25))
  agg_val <- agg[Customer.ID %in% customer_validation]
  agg_train <- agg[!(Customer.ID %in% customer_validation)]
  
  X_train <- agg_train[, -c("Customer.ID", "VAR_REP")]
  y_train <- as.factor(agg_train$VAR_REP)
  X_val <- agg_val[, -c("Customer.ID", "VAR_REP")]
  y_val <- as.factor(agg_val$VAR_REP)
  
  train_control <- caret::trainControl(method="cv", number=4)
  rf <- caret::train(x=X_train, 
                     y=y_train, 
                     trControl=train_control, method="rf", ntree=250)
  print(rf)
  pred_validation <- predict(rf, X_val, type="prob")[, 2]
  metric_auc <- auc(actual = agg_val$VAR_REP, pred = pred_validation)
  return(list(xgboost=rf, auc=metric_auc, pred=pred_validation))
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
  tune_grid <- expand.grid(nrounds=c(100,200,300,400),
                           max_depth = c(3:7),
                           eta = c(0.05, 1),
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
