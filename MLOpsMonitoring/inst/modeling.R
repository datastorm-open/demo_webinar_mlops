library(MLOpsMonitoring)
library(TTR)
library(caret)
library(psych)
library(ggplot2)

# Import dataset and train rf
data  <- MLOpsMonitoring::import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")

# Train
train <- MLOpsMonitoring::create_features(data, 
                                          from=as.Date("2010-03-01", origin="1970-01-01"), 
                                          to=as.Date("2011-01-01", origin="1970-01-01"), by="month", 
                                          windows=c("M-6"), kind="cumulative")
xgb = MLOpsMonitoring::train_xgboost(train)
xgb$auc
rf = MLOpsMonitoring::train_rf(train)
rf$auc

train_6M <- MLOpsMonitoring::create_features_on_period(data, 
                                                       start_rep=as.Date("2010-07-01"), end_rep=as.Date("2010-07-31"),
                                                       windows=c("M-6"), kind="cumulative")
rf_6M = train_rf(train_6M, rep_factor = TRUE)
rf_6M$auc


price_related_col <- c("BASKET_PRICE_MEAN_M-6", "BASKET_PRICE_MIN_M-6", "BASKET_PRICE_MAX_M-6",
                       "EXPENSES_M-6", "EXPENSES_CANCELLED_M-6", "EXPENSES_FREQ_M-6")

## MONITORING : Suivi des perfs. mois par mois
## TODO : Semaine après semaine
monitoring_main <- function(data,
                            X_train,
                            start,
                            end,
                            model,
                            kind_target= "factor", 
                            kind_agregates="cumulative",
                            depth_agregats=c("M-6"),
                            compute_drift = TRUE,
                            compute_perf_globale=TRUE,
                            delay_update = "month",
                            date_format = "%Y-%m-%d"){
  
  out = data.table()
  for(TARGET_start in seq.Date(from=as.Date(start, origin="1970-01-01"), to=as.Date(end, origin="1970-01-01"), by=delay_update)){
    TARGET_start = as.Date(TARGET_start, origin="1970-01-01")
    TARGET_end = TARGET_start + base::months(1)
    out_period = data.table(START = c(TARGET_start,TARGET_start),
                            END = c(TARGET_end,TARGET_end),
                            STATUS = c("témoin", "pertubé"))
    
    # Duplication le dataset de nouvelles données
    # Ajouter des pertubations (avec un paramètre d'intensité)
    agg_period_raw <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, depth_agregats, kind_agregates)
    
    # Calcul des features en utilisant (create_features_on_period) sur le jeu sain et celui perturbé
    agg_period_drifted <- copy(agg_period_raw)
    agg_period_drifted[, price_related_col] = abs(rnorm(nrow(agg_period_raw), .5, 1))*agg_period_drifted[, ..price_related_col]
    
    # Predict
    y_pred <- agg_period_raw$VAR_REP
    X_preds <- list(agg_period_raw[, -c("Customer.ID", "VAR_REP", "YEAR")], 
                    agg_period_drifted[, -c("Customer.ID", "VAR_REP", "YEAR")])
    perf_1 <- numeric(2)
    perf_2 <- numeric(2)
    l_drift_auc <- numeric(2)
    l_dirft_matt <- numeric(2)
    perf_1_name <- ""
    perf_2_name <- ""
    for(idx in 1:2){
      X_pred = X_preds[[idx]]
      if(kind_target == "factor"){
        y_pred <- as.factor(y_pred)
        pred_validation <- predict(model, X_pred, type="prob")[, 2]
        metric_auc <- auc(actual = y_pred, pred = pred_validation)
        metric_accuracy <- accuracy(actual = y_pred, pred = pred_validation, best=TRUE)
        perf_1[idx] = metric_auc
        perf_1_name = "AUC_GLOBAL"
        perf_2[idx] = metric_accuracy$acc
        perf_2_name = "ACC_GLOBAL"
      }
      else if(kind_target == "amount"){
        pred_validation <- predict(model, X_pred)
        metric_rmse <- Metrics::rmse(actual = y_pred, predicted = pred_validation)
        metric_mae <- Metrics::mae(actual = y_pred, predicted = pred_validation)
        perf_1[idx] = metric_rmse
        perf_1_name = "RMSE_GLOBAL"
        perf_2[idx] = metric_mae
        perf_2_name = "MAE_GLOBAL"
      }
      else{
        stop("kind_target soit factor soit amount")
      }
      
      res_drift <- drift_score(X_train[,-c("MONTH")], X_pred[,-c("MONTH")])
      l_drift_auc[idx] = res_drift$auc
      l_dirft_matt[idx] = res_drift$matthews
    }
    browser()
    out_period[[perf_1_name]] = perf_1
    out_period[[perf_2_name]] = perf_2
    out_period[["DRIFT_AUC"]] = l_drift_auc
    out_period[["DRIFT_MATTHEWWS"]] = l_dirft_matt
    
    
    if(kind_target == "factor"){
      pred_test <- predict(model, agg_period_raw[, -c("Customer.ID", "VAR_REP", "YEAR")], type="prob")[,2]
      pred_drifted <- predict(model, agg_period_drifted[, -c("Customer.ID", "VAR_REP", "YEAR")], type="prob")[,2]
      
      out_period$AUC = c(round(MLOpsMonitoring:::auc(actual = y_pred, pred = pred_test),5),
                         round(MLOpsMonitoring:::auc(actual = y_pred, pred = pred_drifted), 5))
      
      out_period$Accuracy = c(round(mean((pred_test>.5) == y_pred),3),
                              round(mean((pred_drifted>.5) == y_pred),3))
      
      out_period$Kappa = c(cohen.kappa(data.table("pred"=as.double(pred_test>.5), "real"=y_pred))$kappa,
                           cohen.kappa(data.table("pred"=as.double(pred_drifted>.5), "real"=y_pred))$kappa)
      
      for(size in c(100,200)){
        out_period[[paste0("TauxAchat-TOP-",size)]] = c(mean(as.logical(y_pred[order(pred_test,decreasing = TRUE)[1:size]])),
                                                        mean(as.logical(y_pred[order(pred_drifted,decreasing = TRUE)[1:size]])))
      }
      
      for(size in c(5700,5800)){
        out_period[[paste0("TauxAchat-DOWN-",size)]] = c(mean(as.logical(y_pred[order(pred_test)[1:size]])),
                                                         mean(as.logical(y_pred[order(pred_drifted)[1:size]])))
      }
    }
    
    out = rbind(out, out_period)
  }
  return(out)
}

scores = monitoring_main(data, train_6M[, -c("Customer.ID", "VAR_REP", "YEAR")], "2011-01-01", "2011-12-31", rf_6M$rf)

# Nos graphs mois par mois à partir de out



