source("MLOpsMonitoring/R/dataset.R")
source("MLOpsMonitoring/R/drift_score.R")

library(caret)
library(Metrics)


# Modèle target sur 3 mois
train_rf <- function(agg, print_rf=TRUE, seed=28, rep_factor=TRUE){
  agg <- apply(agg, 2,  function(x) replace(x, is.infinite(x), NA))
  agg[is.na(agg)] <- 0
  agg <- as.data.table(agg)
  
  set.seed(seed)
  customer_validation <- sample(agg$Customer.ID, round(length(unique(agg$Customer.ID))*0.25))
  agg_val <- agg[Customer.ID %in% customer_validation]
  agg_train <- agg[!(Customer.ID %in% customer_validation)]
  
  X_train <- agg_train[, -c("Customer.ID", "VAR_REP")]
  X_val <- agg_val[, -c("Customer.ID", "VAR_REP")]
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

# Accuracy
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

res3mois <- train_rf(agg_target3)
res3mois$auc


res_mai <- train_rf(agg_target1_windows3)
res_mai$auc
plot(varImp(res_mai$rf), top=10)

res_mai12 <- train_rf(agg_target1_windows12)
res_mai12$auc
plot(varImp(res_mai$rf), top=10)


# Comparaison pour de la profondeur d'historique necessaire
# Fixer une date de target : SEPT 2011 et tester sur OCT, NOV, DEC 2011
list_months_windows <- c(1, 2, 3, 6, 12)
agg_sept11 <- create_features_on_period(data, "2011-09-01", "2011-09-30", list_months_windows)
agg_sept11[, .N/nrow(agg_sept11), VAR_REP]
dim(agg_sept11)

res_models <- list()
for(i in 1:length(list_months_windows)){
  prev_month <- list_months_windows[1:i]
  sub_col <- lapply(prev_month, function(x) colnames(agg_sept11)[grepl(paste0("_", x ,"M"), colnames(agg_sept11))])
  sub_col <- unlist(sub_col)
  sub_col <- c("Customer.ID", "VAR_REP", sub_col)
  model <- train_rf(agg_sept11[, sub_col, with=FALSE])
  res_models[[i]] <- list(months_agg = prev_month, 
                        auc = model$auc)
}

additionnal_months <- c(3, 6, 12)
for(j in 1:length(additionnal_months)){
  sub_col <- colnames(agg_sept11)[grepl(paste0("_", additionnal_months[j] ,"M"), colnames(agg_sept11))]
  print(sub_col)
  sub_col <- c("Customer.ID", "VAR_REP", sub_col)
  model <- train_rf(agg_sept11[, sub_col, with=FALSE])
  res_models[[i+j]] <- list(months_agg = additionnal_months[j], 
                          auc = model$auc)
}

# dt_res_models <- as.data.table(t(as.data.table(res_models)))
# colnames(dt_res_models) <- c("Mois_agg", "AUC_RF")
dt_res_models <- data.table("MOIS_AGG" = unlist(lapply(res_models, function(x) paste0(x$months_agg, collapse = "_"))), 
           "AUC" = unlist(lapply(res_models, function(x) x$auc)))
# Meilleur modèle avec agrégats sur les 6 derniers mois
# Avec ce même modèle comparer les prevs à M+1, M+2, etc

sub_col_6M <- colnames(agg_sept11)[grepl("_6M", colnames(agg_sept11))]
sub_col_6M <- c("Customer.ID", "VAR_REP", sub_col_6M)
model_agg6M <- train_rf(agg_sept11[, sub_col_6M, with=FALSE])
model_agg6M$auc
model_agg6M$pred
model_agg6M$id_val

#Compute accuracy
pred_val <- model_agg6M$pred
actual_val <- agg_sept11[Customer.ID %in% model_agg6M$id_val, VAR_REP]
auc(actual = actual_val, pred = pred_val)
acc <- accuracy(actual = actual_val, pred = pred_val, best=TRUE)

res_perf_predictions <- list()
cpt_index <- 1
for(end_date in c("2011-10-31", "2011-11-30", "2011-12-31")){
  end_date <- as.Date(end_date)
  start_date <- end_date
  lubridate::mday(start_date) <- 1
  agg_predictions <- create_features_on_period(data, start_date, end_date, c(6))
  X_val <- agg_predictions[, -c("Customer.ID", "VAR_REP")]
  y_val <- as.factor(agg_predictions$VAR_REP)
  pred_validation <- predict(model_agg6M$rf, X_val, type="prob")[, 2]
  metric_auc <- auc(actual = y_val, pred = pred_validation)
  metric_accuracy <- accuracy(actual = y_val, pred = pred_validation, best=TRUE)
  res_perf_predictions[[cpt_index]] <- list(start_date_reponse = start_date,
                                            auc=metric_auc,
                                            accuracy=metric_accuracy$acc,
                                            threshold=metric_accuracy$threshold)
  cpt_index <- cpt_index + 1
}






