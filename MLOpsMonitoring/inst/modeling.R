library(MLOpsMonitoring)
library(TTR)
library(caret)
library(psych)
library(ggplot2)

# Import dataset and train rf
data  <- MLOpsMonitoring::import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")

# Train
train <- MLOpsMonitoring::create_features(data, 
                                          from=as.Date("2010-03-01"), to=as.Date("2011-01-01"), by="month", 
                                          windows=c("M-1", "M-2", "M-3"), kind="lag")
xgb = MLOpsMonitoring::train_xgboost(train)
xgb$auc
rf = MLOpsMonitoring::train_rf(train)
rf$auc


train_6M <- MLOpsMonitoring::create_features_on_period(data, 
                                          start_rep=as.Date("2010-07-01"), end_rep=as.Date("2010-07-31"),
                                          windows=c("M-6"), kind="cumulative")
rf_6M = train_rf(train_6M, rep_factor = TRUE)
rf_6M$auc



price_related_col <- c("BASKET_PRICE_MEAN_M-1", "BASKET_PRICE_MIN_M-1", "BASKET_PRICE_MAX_M-1",
                       "BASKET_PRICE_MEAN_M-2", "BASKET_PRICE_MIN_M-2", "BASKET_PRICE_MAX_M-2",
                       "BASKET_PRICE_MEAN_M-3", "BASKET_PRICE_MIN_M-3", "BASKET_PRICE_MAX_M-3",
                       "EXPENSES_M-1", "EXPENSES_M-2", "EXPENSES_M-3", 
                       "EXPENSES_CANCELLED_M-1", "EXPENSES_CANCELLED_M-2", "EXPENSES_CANCELLED_M-3",
                       "EXPENSES_FREQ_M-1","EXPENSES_FREQ_M-2","EXPENSES_FREQ_M-3")

## MONITORING : Suivi des perfs. mois par mois
## Grosse fonction main produisant le monitoring 
## TODO : Semaine après semaine
params_monitoring = list(kind_target= "factor", 
                         kind_agregates="cumulative",
                         depth_agregats=c("M-6"),
                         compute_drift=TRUE, 
                         compute_perf_globale=TRUE,
                         model_monitor=rf_6M$rf, 
                         X_train=train_6M[, -c("Customer.ID", "VAR_REP", "YEAR")])
delay_update = "month" # ou weeks(1)
wdw = months(1) # = 4 semaines
out = data.table()
start <- "2010-09-01"
end <- "2010-11-30"
date_format = "%Y-%m-%d"
for(suivi_start in seq.Date(from = as.Date(start, format=date_format), to = as.Date(end, format=date_format), by="month")){
  suivi_start <- as.Date(suivi_start, origin = "1970-01-01")
  suivi_fin = suivi_start + wdw
  suivi_fin <- as.Date(suivi_fin, origin = "1970-01-01")
  print(c(suivi_start, suivi_fin))
  
  out_period = data.table()
  out_period[, SUIVI_START := suivi_start]
  out_period[, SUIVI_END := suivi_fin]
  
  # Duplication le dataset de nouvelles données
  # Ajouter des pertubations (avec un paramètre d'intensité)
  
  # Calcul des features en utilisant (create_features_on_period) sur le jeu sain et celui perturbé
  agg_period_raw <- create_features_on_period(data, suivi_start, suivi_fin,
                                          params_monitoring$depth_agregats,
                                          kind=params_monitoring$kind_agregates,
                                          target = params_monitoring$kind_target)
  # Predict
  X_pred <- agg_period_raw[, -c("Customer.ID", "VAR_REP", "YEAR")]
  y_pred <- agg_period_raw$VAR_REP
  if(params_monitoring$kind_target == "factor"){
    y_pred <- as.factor(y_pred)
    pred_validation <- predict(params_monitoring$model_monitor, 
                               X_pred, 
                               type="prob")[, 2]
    metric_auc <- auc(actual = y_pred, pred = pred_validation)
    metric_accuracy <- accuracy(actual = y_pred, pred = pred_validation, best=TRUE)
    out_period[, AUC_GLOBAL := metric_auc]
    out_period[, ACC_GLOBAL := metric_accuracy$acc]
  }
  else if(params_monitoring$kind_target == "amount"){
    pred_validation <- predict(params_monitoring$model_monitor, 
                               X_pred)
    metric_rmse <- Metrics::rmse(actual = y_pred, predicted = pred_validation)
    metric_mae <- Metrics::mae(actual = y_pred, predicted = pred_validation)
    out_period[, RMSE_GLOBAL := metric_rmse]
    out_period[, MAE_GLOBAL := metric_mae]
  }
  else{
    stop("params_monitoring$kind_target soit factor soit amount")
  }
  # Drift Score / Kappa / TOP / Bottom
  res_drift <- drift_score(params_monitoring$X_train, X_pred)
  out_period[, DRIFT_AUC := res_drift$auc]
  out_period[, DRIFT_MATTHEWWS := res_drift$matthews]

  out <- rbind(out, out_period)
}

# Nos graphs mois par mois à partir de out



