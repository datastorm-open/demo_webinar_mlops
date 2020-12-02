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


price_related_col <- c("BASKET_PRICE_MEAN_M-1", "BASKET_PRICE_MIN_M-1", "BASKET_PRICE_MAX_M-1",
                       "BASKET_PRICE_MEAN_M-2", "BASKET_PRICE_MIN_M-2", "BASKET_PRICE_MAX_M-2",
                       "BASKET_PRICE_MEAN_M-3", "BASKET_PRICE_MIN_M-3", "BASKET_PRICE_MAX_M-3",
                       "EXPENSES_M-1", "EXPENSES_M-2", "EXPENSES_M-3", 
                       "EXPENSES_CANCELLED_M-1", "EXPENSES_CANCELLED_M-2", "EXPENSES_CANCELLED_M-3",
                       "EXPENSES_FREQ_M-1","EXPENSES_FREQ_M-2","EXPENSES_FREQ_M-3")

## MONITORING : Suivi des perfs. mois par mois
## Grosse fonction main produisant le monitoring 
## TODO : Semaine après semaine
params_monitoring = list(kind_target= "factor", compute_drift=TRUE)
delay_update = "month" # ou weeks(1)
wdw = months(1) # = 4 semaines
out = data.table()
for(suivi_start in seq.Date(from = as.Date(start), to = as.Date(end), by="month")){
  out_period = data.table()
  suiv_fin = suivi_start + wdw
  
  # Duplication le dataset de nouvelles données
  # Ajouter des pertubations (avec un paramètre d'intensité)
  
  # Calcul des features en utilisant (create_features_on_period) sur le jeu sain et celui perturbé
  
  # Predict
  
  # Drift Score / Kappa / TOP / Bottom

  out = rbind(out, out_period)
}

# Nos graphs mois par mois à partir de out



