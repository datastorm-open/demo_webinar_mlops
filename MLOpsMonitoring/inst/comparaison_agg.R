# Comparaison des perfs selon différentes condigs d'agregats

library(ggplot2)

TARGET_start <- "2010-12-01"
TARGET_end <- "2010-12-31"

agg_target1_windows1_lag <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, 
                                                                       c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6"), kind="lag")
agg_target1_windows1_cumm <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, 
                                                                        c("M-1", "M-2", "M-3", "M-6", "M-12"), kind="cumulative")

agg_target1_windows1_lag[, .N/nrow(agg_target1_windows1_lag), VAR_REP]
model_lag <- train_rf(agg_target1_windows1_lag)
model_lag$auc
model_lag$acc
model_lag$threshold

model_cumm <- train_rf(agg_target1_windows1_cumm)
model_cumm$auc
model_cumm$acc
model_cumm$threshold

# Quelle fenetre pour les agrégats cumulés ? 
nb_models <- 5
for(w in c("M-1", "M-2", "M-3", "M-6", "M-12")){
  sub_col <- colnames(agg_target1_windows1_cumm)[grepl(w, colnames(agg_target1_windows1_cumm))]
  sub_col <- c("VAR_REP", "Customer.ID", sub_col)
  
  list_auc <- c()
  list_acc <- c()
  set.seed(19)
  list_seeds <- sample(1:100, nb_models)
  for(seed in list_seeds){
    print(seed)
    model_temp_ <- train_rf(agg_target1_windows1_cumm[, sub_col, with=FALSE], print_rf = FALSE, seed=seed)
    list_auc <- c(list_auc, model_temp_$auc)
    list_acc <- c(list_acc, model_temp_$acc)
  }
  print(paste0("AGG : ", w, " - MOY AUC : ", mean(list_auc), " -  MOY ACC : ", mean(list_acc)))
  # model_temp_ <- train_rf(agg_target1_windows1_cumm[, sub_col, with=FALSE], print_rf = FALSE, seed=28)
  # print(paste0("AGG : ", w, " - AUC : ", model_temp_$auc, " - ACC : ", model_temp_$acc, " - THRESHOLD ", model_temp_$threshold))
  # model_temp_ <- train_rf(agg_target1_windows1_cumm[, sub_col, with=FALSE], print_rf = FALSE, seed=88)
  # print(paste0("AGG : ", w, " - AUC : ", model_temp_$auc, " - ACC : ", model_temp_$acc, " - THRESHOLD ", model_temp_$threshold))
  print("----")
}
# Choix entre M-1 et M-6 


end_of_month <- function(date){
  date <- as.Date(date, origin = "1970-01-01")
  data_end <- date %m+% months(1) %m-% days(1)
  return(data_end)
}

# Comment le modèle évolue sur l'année 2011 
# Entre le modèle LAG 
model_stabilite_futur <- function(model, start="2011-01-01", end="2011-12-31", windows, kind, 
                                  return_pred=FALSE, rep_factor="TRUE"){
  list_months <- seq.Date(from = as.Date(start), to = as.Date(end), by="month")
  print(list_months)
  all_auc <- c()
  all_acc <- c()
  if(return_pred){
    dt_pred <- data.table(PRED = numeric(), MONTH_START=character())
  }
  for(month in list_months){
    month <- as.Date(month, origin = "1970-01-01")
    target_start <- month
    target_end <- end_of_month(target_start)
    print(c(target_start, target_end))
    if(rep_factor){
      agg_ <- create_features_on_period(data, target_start, target_end, windows, kind=kind,target="factor")
    }
    else{
      agg_ <- create_features_on_period(data, target_start, target_end, windows, kind=kind,target="amount")
    }
    

    X_val <- agg_[, -c("Customer.ID", "VAR_REP")]
    y_val <- agg_$VAR_REP
    if(rep_factor){
      y_val <- as.factor(y_val)
      pred_validation <- predict(model$rf, X_val, type="prob")[, 2]
    }
    else{
      pred_validation <- predict(model$rf, X_val)
    }
    
    if(return_pred){
      dt_pred_ <- data.table(PRED = pred_validation, MONTH_START=rep(as.character(target_start), length(pred_validation)))
      dt_pred <- rbind(dt_pred, dt_pred_)
    }
    
    if(rep_factor){
      metric_auc <- auc(actual = y_val, pred = pred_validation)
      metric_accuracy <- accuracy(actual = y_val, pred = pred_validation, threshold = model$threshold)
      all_auc <- c(all_auc, metric_auc)
      all_acc <- c(all_acc, metric_accuracy)
    }
    else{
      metric_rmse <- Metrics::rmse(actual = y_val, predicted = pred_validation)
      metric_mae <- Metrics::mae(actual = y_val, predicted = pred_validation)
      all_auc <- c(all_auc, metric_rmse)
      all_acc <- c(all_acc, metric_mae)
    }
  }
  if(return_pred){
    if(rep_factor){
      return(list(perf=data.table(MONTH = list_months, AUC = all_auc, ACC = all_acc), ALL_PRED=dt_pred))
    }
    else{
      return(list(perf=data.table(MONTH = list_months, RMSE = all_auc, MAE = all_acc), ALL_PRED=dt_pred))
    }
    
  }
  else{
    if(rep_factor){
      return(data.table(MONTH = list_months, AUC = all_auc, ACC = all_acc))
    }
    else{
      return(data.table(MONTH = list_months, RMSE = all_auc, MAE = all_acc))
    }
    
  }
  
}


# sub_col_M1 <- c("VAR_REP", "Customer.ID", colnames(agg_target1_windows1_cumm)[grepl("M-1", colnames(agg_target1_windows1_cumm))])
TARGET_start <- "2010-12-01"
TARGET_end <- "2010-12-31"
agg_cumm_M1 <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, 
                                                                        c("M-1"), kind="cumulative")
model_cumm_M1 <- train_rf(agg_cumm_M1, print_rf = FALSE, seed=28)
dt_stabilite_cumm_M1 <- model_stabilite_futur(model_cumm_M1, windows = c("M-1"), kind="cumulative")

agg_cumm_M6 <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, 
                                                          c("M-6"), kind="cumulative")
model_cumm_M6 <- train_rf(agg_cumm_M6, print_rf = FALSE, seed=28)
dt_stabilite_cumm_M6 <- model_stabilite_futur(model_cumm_M6, windows = c("M-6"), kind="cumulative")


ggplot()+
  geom_point(data=dt_stabilite_cumm_M1, aes(x=MONTH, y=AUC, color="red"))+
  geom_line(data=dt_stabilite_cumm_M1, aes(x=MONTH, y=AUC, color="red"), )+
  geom_point(data=dt_stabilite_cumm_M6, aes(x=MONTH, y=AUC, color="blue"))+
  geom_line(data=dt_stabilite_cumm_M6, aes(x=MONTH, y=AUC, color="blue"))+
  theme_minimal()+
  ggtitle("Evolution des perfs d'un modèle entrainé sur DEC 2010 mois par mois sur 2011")+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Agregats", 
                      values = c("red"= "red", "blue"="blue"), 
                      labels=c("6M cumm", "1M cumm"))

ggplot()+
  geom_point(data=dt_stabilite_cumm_M1, aes(x=MONTH, y=ACC, color="red"))+
  geom_line(data=dt_stabilite_cumm_M1, aes(x=MONTH, y=ACC, color="red"))+
  geom_point(data=dt_stabilite_cumm_M6, aes(x=MONTH, y=ACC, color="blue"))+
  geom_line(data=dt_stabilite_cumm_M6, aes(x=MONTH, y=ACC, color="blue"))+
  theme_minimal()+
  ggtitle("Evolution des perfs d'un modèle entrainé sur DEC 2010 mois par mois sur 2011")+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Agregats", 
                      values = c("red"= "red", "blue"="blue"), 
                      labels=c("6M cumm", "1M cumm"))
model_cumm_M1$auc
model_cumm_M6$auc


ggplot()+
  geom_point(data=dt_stabilite_cumm_M1, aes(x=MONTH, y=1-ACC, color="red"))+
  geom_line(data=dt_stabilite_cumm_M1, aes(x=MONTH, y=1-ACC, color="red"))+
  geom_point(data=dt_stabilite_cumm_M6, aes(x=MONTH, y=1-ACC, color="blue"))+
  geom_line(data=dt_stabilite_cumm_M6, aes(x=MONTH, y=1-ACC, color="blue"))+
  theme_minimal()+
  ggtitle("Evolution des perfs d'un modèle entrainé sur DEC 2010 mois par mois sur 2011 - Probabilité de se tromper = 1 - ACC")+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Agregats", 
                      values = c("red"= "red", "blue"="blue"), 
                      labels=c("6M cumm", "1M cumm"))


# Drift score entre 2 jeux de données sur 2 périodes de temps
X1 <- agg_cumm_M1[, -c("Customer.ID", "VAR_REP", "MONTH", "YEAR")]
X2 <- agg_cumm_M1[, -c("Customer.ID", "VAR_REP", "MONTH", "YEAR")]
res_drift_score <- drift_score(X1, X2)

# Comparaison de la distrib des scores dans le temps: 
# garder les preds mois par mois et comparer...
stabilite_with_pred_1Mcumm <- model_stabilite_futur(model_cumm_M1, windows = c("M-1"), kind="cumulative", return_pred = TRUE)
stabilite_with_pred_6Mcumm <- model_stabilite_futur(model_cumm_M6, windows = c("M-6"), kind="cumulative", return_pred = TRUE)

ggplot(stabilite_with_pred_1Mcumm$ALL_PRED, 
       aes(x=PRED)) +
  geom_density(aes(colour=as.factor(MONTH_START)))+
  ggtitle("Comparaison des prédictions")+
  theme_minimal()

ggplot(stabilite_with_pred_6Mcumm$ALL_PRED, 
       aes(x=PRED)) +
  geom_density(aes(colour=as.factor(MONTH_START)))+
  ggtitle("Comparaison des prédictions")+
  theme_minimal()

# Comparaison des prevs 2 à 2 avec le modèle de base... 
length(unique(stabilite_with_pred_1Mcumm$ALL_PRED$MONTH_START))
for(month in unique(stabilite_with_pred_1Mcumm$ALL_PRED$MONTH_START)){
  dt_density_pred <- data.table(PRED = model_cumm_M1$pred, MONTH=rep("VALIDATION", 
                                                                     length(model_cumm_M1$pred)))
  dt_density_pred <- rbind(dt_density_pred, data.table(PRED = stabilite_with_pred_1Mcumm$ALL_PRED[MONTH_START == month, PRED], 
                                                       MONTH=rep(as.character(month), length(stabilite_with_pred_1Mcumm$ALL_PRED[MONTH_START == month, PRED]))))
  p <- ggplot(dt_density_pred, 
         aes(x=PRED)) +
    geom_density(aes(colour=as.factor(MONTH)))+
    ggtitle("Comparaison des prédictions")
  theme_minimal()
  p
}

## BOF comme viz, regarder QQPLOT entre 2 samples différents 

# Drift score
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


# Drift a partir agg 6M
res_drift_futur <- drift_futur(agg_cumm_M6, windows = c("M-6"), kind="cumulative")
res_drift_futur_6M <- drift_futur(agg_cumm_M6, start="2011-01-01", end="2011-12-31", windows = c("M-6"), kind="cumulative")
ggplot()+
  geom_point(data=res_drift_futur, aes(x=MONTH, y=AUC, color="red"))+
  geom_line(data=res_drift_futur, aes(x=MONTH, y=AUC, color="red"))+
  geom_point(data=res_drift_futur, aes(x=MONTH, y=MATTHEWS, color="blue"))+
  geom_line(data=res_drift_futur, aes(x=MONTH, y=MATTHEWS, color="blue"))+
  theme_minimal()+
  ggtitle("Evolution des drift score par rapport à DEC 2010")+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Métriques", 
                      values = c("red"= "red", "blue"="blue"), 
                      labels=c("MATTHEWS", "AUC"))

graph_stabilite_perf <- function(data){
  p <- ggplot()+
    geom_point(data=data, aes(x=MONTH, y=AUC, color="red"))+
    geom_line(data=data, aes(x=MONTH, y=AUC, color="red"))+
    geom_point(data=data, aes(x=MONTH, y=ACC, color="blue"))+
    geom_line(data=data, aes(x=MONTH, y=ACC, color="blue"))+
    theme_minimal()+
    ggtitle("Evolution des perfs")+
    theme(legend.position = "bottom")+
    scale_colour_manual(name = "Métriques", 
                        values = c("red"= "red", "blue"="blue"), 
                        labels=c("ACCURACY", "AUC"))
  return(p)
}

graph_stabilite_drift <- function(data){
  p <- ggplot()+
    geom_point(data=data, aes(x=MONTH, y=AUC, color="red"))+
    geom_line(data=data, aes(x=MONTH, y=AUC, color="red"))+
    geom_point(data=data, aes(x=MONTH, y=MATTHEWS, color="blue"))+
    geom_line(data=data, aes(x=MONTH, y=MATTHEWS, color="blue"))+
    theme_minimal()+
    ggtitle("Evolution des drift score par rapport à DEC 2010")+
    theme(legend.position = "bottom")+
    scale_colour_manual(name = "Métriques", 
                        values = c("red"= "red", "blue"="blue"), 
                        labels=c("MATTHEWS", "AUC"))
  return(p)
}

# Comparaison perf, pred et drift score sur 6 mois
TARGET_start <- "2010-09-01"
TARGET_end <- "2010-09-30"
stabilite_start="2010-10-01"
stabilite_end="2011-10-31"
agg_cumm_M6 <- MLOpsMonitoring::create_features_on_period(data, TARGET_start, TARGET_end, 
                                                          c("M-6"), kind="cumulative")
model_cumm_M6 <- train_rf(agg_cumm_M6, print_rf = FALSE, seed=28)
dt_stabilite_cumm_M6 <- model_stabilite_futur(model_cumm_M6,stabilite_start, stabilite_end, 
                                              windows = c("M-6"), kind="cumulative")

model_cumm_M6$auc
dt_stabilite_cumm_M6 <- rbind(data.table(MONTH = as.Date(TARGET_start), 
                                         AUC = model_cumm_M6$auc, 
                                         ACC = model_cumm_M6$acc), 
                              dt_stabilite_cumm_M6)
graph_stabilite_perf(dt_stabilite_cumm_M6)


res_drift_futur_6M <- drift_futur(agg_cumm_M6, start=stabilite_start, end=stabilite_end, 
                                  windows = c("M-6"), kind="cumulative")
graph_stabilite_drift(res_drift_futur_6M)
# qqplot(x, y, xlim=rg, ylim=rg, ...)

# MODELE avec Y = montant
agg_cumm_M6_montant <- create_features_on_period(data, TARGET_start, TARGET_end, 
                                                          c("M-6"), kind="cumulative", 
                                                          target="amount")
model_cumm_M6_montant <- train_rf(agg_cumm_M6_montant, print_rf = FALSE, seed=28, rep_factor = FALSE)
dt_stabilite_cumm_M6_montant <- model_stabilite_futur(model_cumm_M6_montant, stabilite_start, stabilite_end, 
                                                      windows = c("M-6"), 
                                                      kind="cumulative",rep_factor = FALSE)

# par(mfrow=c(1, 2))
require(gridExtra)
p1 <- ggplot()+
  geom_point(data=dt_stabilite_cumm_M6_montant, aes(x=MONTH, y=RMSE, color="red"))+
  geom_line(data=dt_stabilite_cumm_M6_montant, aes(x=MONTH, y=RMSE, color="red"))+
  theme_minimal()+
  ggtitle("Evolution des perfs - RMSE")+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Métriques", 
                      values = c("red"= "red"), 
                      labels=c("RMSE"))
p2 <- ggplot()+
  geom_point(data=dt_stabilite_cumm_M6_montant, aes(x=MONTH, y=MAE, color="blue"))+
  geom_line(data=dt_stabilite_cumm_M6_montant, aes(x=MONTH, y=MAE, color="blue"))+
  theme_minimal()+
  ggtitle("Evolution des perfs - MAE")+
  theme(legend.position = "bottom")+
  scale_colour_manual(name = "Métriques", 
                      values = c("blue"="blue"), 
                      labels=c("MAE"))
grid.arrange(p1, p2, ncol=2)
model_cumm_M6_montant$rmse
model_cumm_M6_montant$mae

volume_ventes <- data[, .N, .(year(InvoiceDate), month(InvoiceDate))]
volume_ventes[, date := as.Date(paste0(month, "-01-", year), format="%m-%d-%Y")]

