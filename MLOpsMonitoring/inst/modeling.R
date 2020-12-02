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
                                          windows=c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6"), kind="cumulative")
xgb = MLOpsMonitoring::train_xgboost(train)
xgb$auc
rf = MLOpsMonitoring::train_rf(train)
rf$auc


price_related_col <- c("BASKET_PRICE_MEAN_M-1", "BASKET_PRICE_MIN_M-1", "BASKET_PRICE_MAX_M-1",
                       "BASKET_PRICE_MEAN_M-2", "BASKET_PRICE_MIN_M-2", "BASKET_PRICE_MAX_M-2",
                       "BASKET_PRICE_MEAN_M-3", "BASKET_PRICE_MIN_M-3", "BASKET_PRICE_MAX_M-3",
                       "EXPENSES_M-1", "EXPENSES_M-2", "EXPENSES_M-3", 
                       "EXPENSES_CANCELLED_M-1", "EXPENSES_CANCELLED_M-2", "EXPENSES_CANCELLED_M-3",
                       "EXPENSES_FREQ_M-1","EXPENSES_FREQ_M-2","EXPENSES_FREQ_M-3")

## MONITORING : Suivi des perfs. mois par mois
## TODO : Semaine après semaine
monitoring_main <- function(data,
                            start,
                            end,
                            model,
                            kind_target = "factor",
                            compute_drift = TRUE,
                            delay_update = "month",
                            aggr_windows = c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6"),
                            aggr_kind = "cumulative"){
  
  out = data.table()
  for(TARGET_start in seq.Date(from=as.Date(start, origin="1970-01-01"), to=as.Date(end, origin="1970-01-01"), by=delay_update)){
    TARGET_end = TARGET_start + base::months(1)
    out_period = data.table(START = c(TARGET_start,TARGET_start),
                            END = c(TARGET_end,TARGET_end),
                            STATUS = c("témoin", "pertubé"))
    
    # Duplication le dataset de nouvelles données
    # Ajouter des pertubations (avec un paramètre d'intensité)
    test <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, aggr_windows, aggr_kind)
    print(colnames(test))
    
    # Calcul des features en utilisant (create_features_on_period) sur le jeu sain et celui perturbé
    test_drifted <- copy(test)
    test_drifted[, price_related_col] = abs(rnorm(nrow(test), .5, 1))*test_drifted[, ..price_related_col]
    
    # Predict
    pred_test <- predict(model, test[, -c("Customer.ID", "VAR_REP", "YEAR")], type="prob")[,2]
    pred_drifted <- predict(model, test_drifted[, -c("Customer.ID", "VAR_REP", "YEAR")], type="prob")[,2]
    truth <- test[, VAR_REP]
    
    # Drift Score / Kappa / TOP / Bottom
    out_period$AUC = c(round(MLOpsMonitoring:::auc(actual = truth, pred = pred_test),5),
                       round(MLOpsMonitoring:::auc(actual = truth, pred = pred_drifted), 5))
    
    out_period$Accuracy = c(round(mean((pred_test>.5) == truth),3),
                            round(mean((pred_drifted>.5) == truth),3))
    
    out_period$Kappa = c(cohen.kappa(data.table("pred"=as.double(pred_test>.5), "real"=truth))$kappa,
                         cohen.kappa(data.table("pred"=as.double(pred_drifted>.5), "real"=truth))$kappa)
    
    for(size in c(100,200)){
      out_period[paste0("TauxAchat-TOP-",size)] = c(mean(truth[order(pred_test,decreasing = TRUE)[1:size]]),
                                                    mean(truth[order(pred_drifted,decreasing = TRUE)[1:size]]))
    }
    
    for(size in c(5700,5800)){
      out_period[paste0("TauxAchat-DOWN-",size)] = c(mean(truth[order(pred_test)[1:size]]),
                                                     mean(truth[order(pred_drifted)[1:size]]))
    }
    
    out = rbind(out, out_period)
  }
  return(out)
}


scores = monitoring_main(data, "2011-01-01", "2011-12-31", xgb$model)

# Nos graphs mois par mois à partir de out



