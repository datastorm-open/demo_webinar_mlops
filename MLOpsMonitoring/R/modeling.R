#' Train a RandomForest model. Compute AUC.
#'
#' @param agg : \code{data.table}. Features and target.
#' @param print_rf : print random forest summary
#' @param seed : seed number to control randomness
#' @param rep_factor : TODO
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
#' agg_4trim[,.N/nrow(agg_4trim),VAR_REP]
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
    metric_auc <- auc(actual = y_val, pred = pred_validation)
    metric_accuracy <- accuracy(actual = y_val, pred = pred_validation, best=TRUE)
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

#' Train a XGBoost model. Compute AUC.
#'
#' @param agg : \code{data.table}. Features and target.
#' 
#' 
#' @return a list of a XGBoost Object, AUC, predictions
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

#' accuracy
#'
#' @param actual : actual value of targets
#' @param pred : prediction made by a model
#' @param best : wthr. or not the function looks for the threshold that gives the best accuracy
#' @param threshold : fixed value of threshold (needed if best==FALSE)
#' 
#' @return a list of a RF Object, AUC, predictions
#'
#' @export
#'
#'
#' @examples 
#' \dontrun{
#' TODO
#' }
#'
accuracy <- function(actual, pred,  best=FALSE, threshold=0.5){
  pred_ROCR <- ROCR::prediction(pred, actual)
  acc <- ROCR::performance(pred_ROCR, measure = "acc")
  # Si on veut renvoyer la meilleure accuracy avec le meilleur seuil:
  if(best){
    best_accuracy <- max(acc@y.values[[1]])
    best_threshold <- acc@x.values[[1]][which(acc@y.values[[1]] == best_accuracy)]
    if(length(best_threshold) > 1){
      best_threshold <- min(best_threshold)
    }
    return(list(acc=best_accuracy, threshold=best_threshold))
  }
  else{
    return(acc@y.values[[1]][which(acc@x.values[[1]]==threshold)])
  }
}


#' A wrapper that run model for each testing period, computes metrics (auc, accuracy, kappa, drift_auc, ...)
#'
#' @param data : TODO
#' @param X_train : TODO
#' @param start : TODO
#' @param end : TODO
#' @param model : a model from caret (random forest, xgboost, ...)
#' @param kind_target : default factor
#' @param kind_agregates : default cumulative
#' @param depth_agregats : default M-6
#' @param delay_update : default month
#' @param date_format : default \%Y-\%m-\%d
#'
#' @return a list of a data.table with scores, and a list of data.table corresponding to each features for each months
#'
#' @import data.table
#' @export
#'
#' @examples 
#' \dontrun{
#' TODO
#' }
#'
monitoring_main <- function(data,
                            X_train,
                            start,
                            end,
                            model,
                            kind_target= "factor", 
                            kind_agregates="cumulative",
                            depth_agregats=c("M-6"),
                            delay_update = "month",
                            date_format = "%Y-%m-%d"){
  
  scores = data.table()
  features_batch = list()
  drift_imp = data.table()
  predictions = data.table()
  start = as.Date(start, origin="1970-01-01")
  end = as.Date(end, origin="1970-01-01") 
  for(TARGET_start in seq.Date(from = start, to=end, by=delay_update)){
    TARGET_start = as.Date(TARGET_start, origin="1970-01-01")
    TARGET_end = TARGET_start + base::months(1)
    scores_period = data.table(START = TARGET_start, END = TARGET_end)
    drift_imp_period = data.table(START = c(TARGET_start,TARGET_start),
                                  END = c(TARGET_end,TARGET_end))
    predictions_period = data.table(START = c(TARGET_start,TARGET_start),
                                  END = c(TARGET_end,TARGET_end))
    
    agg_period_raw <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, depth_agregats, kind_agregates)
    features_batch[[as.character(TARGET_start)]] = agg_period_raw
    
    # Predict
    y_pred <- agg_period_raw$VAR_REP
    X_pred <- agg_period_raw[, -c("Customer.ID", "VAR_REP", "YEAR")]
    if(kind_target == "factor"){
      pred_test <- pred_validation <- predict(model, X_pred, type="prob")[, 2]
      
      scores_period$AUC_GLOBAL = auc(actual = y_pred, pred = pred_validation)
      scores_period$ACC_GLOBAL = accuracy(actual = y_pred, pred = pred_validation, best=TRUE)$acc
      scores_period$Kappa = cohen.kappa(data.table("pred"=as.double(pred_test>.5), "real"=y_pred))$kappa
      scores_period$LogLoss =  LogLoss(pred_test, y_pred)
      
      for(size in c(100,200)){
        scores_period[[paste0("TauxAchat-TOP-",size)]] = mean(as.logical(y_pred[order(pred_test,decreasing = TRUE)[1:size]]))
      }
      
      for(size in c(5700,5800)){
        scores_period[[paste0("TauxAchat-DOWN-",size)]] = mean(as.logical(y_pred[order(pred_test)[1:size]]))
      }
      
    }
    else if(kind_target == "amount"){
      pred_validation <- predict(model, X_pred)
      scores_period$RMSE_GLOBAL = Metrics::rmse(actual = y_pred, predicted = pred_validation)
      scores_period$MAE_GLOBAL = Metrics::mae(actual = y_pred, predicted = pred_validation)
    }
    else{
      stop("kind_target soit factor soit amount")
    }
    
    predictions_period = cbind(predictions_period, data.table(PRED = pred_validation, ACTUAL=y_pred))
    
    res_drift <- drift_score(X_train[,-c("MONTH")], X_pred[,-c("MONTH")])
    scores_period$DRIFT_AUC = res_drift$auc
    scores_period$DRIFT_MATTHEWWS = res_drift$matthews
    df_imp <- res_drift$importance$importance
    df_imp <- data.table(VARIABLES = row.names(df_imp), IMPORTANCE = df_imp$Overall)
    drift_imp_period <- cbind(drift_imp_period, df_imp)
    
    scores = rbind(scores, scores_period)
    drift_imp = rbind(drift_imp, drift_imp_period)
    predictions = rbind(predictions, predictions_period)
  }
  return(list(scores=scores, features_batch=features_batch, drift_imp=drift_imp, predictions=predictions))
}


#' add_selected_but_no_bought
#'
#' @param data : a dataset in which we will add fake lines
#' @param from : same as from in ?seq.Date
#' @param to : same as to in ?seq.Date
#' @param by : same as by in ?seq.Date
#' @param min_customer : min number of customers the can be altered each month
#' @param max_customer : max number of customers the can be altered each month
#' @param odd_new_customer : inverse of probability to add a new customer rather than altering an existing one
#' @param mu_sku : number of SKU by customer to be added follows rnorm(mu_sku, sigma_sku)
#' @param sigma_sku : number of SKU by customer to be added follows a rnom(mu_sku, sigma_sku)
#' @param mu_qtty : quantity added for each of SKU added follows rnorm(mu_qtty, sigma_qtty)
#' @param sigma_qtty : quantity added for each of SKU added follows rnorm(mu_qtty, sigma_qtty)
#'
#' @return a new dataset
#'
#' @export
#'
#' @examples 
#' \dontrun{
#' #TODO
#' }
#'
add_selected_but_no_bought <- function(data, from, to, by, min_customer=500, max_customer=1000, odd_new_customer=5, mu_sku=30, sigma_sku=20, mu_qtty=30, sigma_qtty=20){
  months_list = seq.Date(from, to, by)
  data_with_fake = as.data.table(copy(data)) ## data.table that will be output
  list_of_sku = unique(data[,c("StockCode", "Description", "Price")])
  
  # List of customers that has not bought after months_list[1]
  # Create new customers
  real_customers_no_bough = setdiff(unique(data$Customer.ID), data[InvoiceDate>as.Date(months_list[1], origin="1970-01-01"), unique(Customer.ID)])
  new_customers = 20000:round(20000+odd_new_customer*length(real_customers_no_bough))
  customers = c(real_customers_no_bough, new_customers)
  
  # Select a few customers for each month
  n_by_month = round(runif(length(months_list), min_customer, max_customer))
  sample_customers_by_month = lapply(n_by_month, function(size,x){sample(x, size)}, x=customers)
  
  for(idx in 1:length(months_list)){
    customers = data.table("id" = sample_customers_by_month[[idx]],
                           "n" = abs(round(rnorm(n_by_month[idx], mu_sku, sigma_sku)))+1)
    month_start = months_list[idx]
    
    new_data = do.call(rbind, apply(customers, 1 , function(x, sku, start_month){
      id=x[[1]]
      n=x[[2]]
      # pick a random number of baskets and choose ids for baskets
      nb_cmd = round(runif(1,1,10))
      ids_cmd = paste0("PANIER", round(runif(nb_cmd, 50000,60000)))
      # pick a date for each basket
      date_cmd = start_month + round(runif(nb_cmd,1,30))
      names(date_cmd) = ids_cmd
      # select randomly products
      out=sku[runif(n,1,nrow(sku)-1),]
      # add metadata
      out[["Customer.ID"]]=id
      out[["Quantity"]] = rnorm(n, mu_qtty, sigma_qtty)
      out[["Country"]] = "NEWDATA"
      out[["Invoice"]] = sample(ids_cmd, n, replace = T)
      out[["InvoiceDate"]] = date_cmd[out[["Invoice"]]]
      return(out)
    }, sku=list_of_sku, start_month=month_start))
    
    data_with_fake = rbind(data_with_fake, new_data)
  }
  return(data_with_fake)
}