library(MLOpsMonitoring)
library(TTR)
library(caret)
library(psych)
library(ggplot2)

# Import dataset and train rf
data  <- MLOpsMonitoring::import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")
train <- MLOpsMonitoring::create_features(data, 
                                          from=as.Date("2010-03-01"), to=as.Date("2011-01-01"), by="month", 
                                          windows=c("M-1", "M-2", "M-3"), kind="lag")
xgb = MLOpsMonitoring::train_xgboost(train)
xgb$auc
rf = MLOpsMonitoring::train_rf(train)
rf$auc

# Create test dataset, apply drift
test <- MLOpsMonitoring::create_features(data, 
                                         from=as.Date("2011-01-01"),  to=as.Date("2012-01-01"), by = "month", 
                                         windows=c("M-1", "M-2", "M-3"), kind="lag")


price_related_col <- c("BASKET_PRICE_MEAN_M-1", "BASKET_PRICE_MIN_M-1", "BASKET_PRICE_MAX_M-1",
                       "BASKET_PRICE_MEAN_M-2", "BASKET_PRICE_MIN_M-2", "BASKET_PRICE_MAX_M-2",
                       "BASKET_PRICE_MEAN_M-3", "BASKET_PRICE_MIN_M-3", "BASKET_PRICE_MAX_M-3",
                       "EXPENSES_M-1", "EXPENSES_M-2", "EXPENSES_M-3", 
                       "EXPENSES_CANCELLED_M-1", "EXPENSES_CANCELLED_M-2", "EXPENSES_CANCELLED_M-3",
                       "EXPENSES_FREQ_M-1","EXPENSES_FREQ_M-2","EXPENSES_FREQ_M-3")

test_drifted <- copy(test)
test_drifted[YEAR>2010&MONTH>5, price_related_col] = abs(rnorm(77246, .5, 1))*test_drifted[YEAR>2010&MONTH>5, ..price_related_col]
dim(test_drifted)
colnames(test_drifted)

# apply_drift <- function(df){}
# test_drifted[,apply_drift(.SD)$ITEM_PRICE_MAX_M-1=.SD$ITEM_PRICE_MAX_M-1, by=.(YEAR, MONTH)]

scores = data.table()
for(year in 2011:2011){
  for(month in 1:12){
    pred_drifted <- predict(xgb$model, test_drifted[(YEAR==year)&(MONTH==month), -c("Customer.ID", "VAR_REP")], type="prob")[,2]
    pred_test <- predict(xgb$model, test[YEAR==year&MONTH==month, -c("Customer.ID", "VAR_REP")], type="prob")[,2]
    truth <- test[YEAR==2011&MONTH==month, VAR_REP]
    
    ## AUC
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="temoin", SCORE="AUC", 
                                    VALUE=round(MLOpsMonitoring:::auc(actual = truth, pred = pred_test),5)))
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="drifted", SCORE="AUC", 
                                    VALUE=round(MLOpsMonitoring:::auc(actual = truth, pred = pred_drifted), 5)))
    ## ACCURACY
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="temoin", SCORE="Accuracy", 
                                    VALUE=round(mean((pred_test>.5) == truth),3)))
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="drifted", SCORE="Accuracy", 
                                    VALUE=round(mean((pred_drifted>.5) == truth),3)))
    
    ## KAPPA
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="temoin", SCORE="Kappa", 
                                    VALUE=cohen.kappa(data.table("pred"=as.double(pred_test>.5), "real"=truth))$kappa))
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="drifted", SCORE="Kappa", 
                                    VALUE=cohen.kappa(data.table("pred"=as.double(pred_drifted>.5), "real"=truth))$kappa))

    ## TOP (800clients/mois)
    for(size in c(100,200)){
      scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="temoin", SCORE=paste0("TauxAchat-TOP-",size), 
                                      VALUE=mean(truth[order(pred_test,decreasing = TRUE)[1:size]])))
      scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="drifted", SCORE=paste0("TauxAchat-TOP-",size), 
                                      VALUE=mean(truth[order(pred_drifted,decreasing = TRUE)[1:size]])))
    }
    
    ## DOWN (800clients/mois)
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="temoin", SCORE="TauxAchat-DOWN", 
                                    VALUE=mean(truth[order(pred_test)[1:5800]])))
    scores=rbind(scores, data.frame(YEAR=year, MONTH=month, STATUS="drifted", SCORE="TauxAchat-DOWN", 
                                    VALUE=mean(truth[order(pred_drifted)[1:5800]])))
  }     
}

monitor_score <- function(df, score_name, ylab, ylim, leg_pos="bottomleft"){
  score_ok=df[SCORE==score_name&STATUS=="temoin", VALUE]
  score_ko=df[SCORE==score_name&STATUS=="drifted", VALUE]
  plot(score_ok, type='l', ylim=ylim, col="blue", main='Dérive', ylab=ylab)
  lines(score_ko)
  legend(leg_pos,legend=c('Témoin', 'Avec dérive'), col=c('blue', 'black'), lty=1,lwd=2)
}

monitor_score(scores, "AUC", "AUC", c(.5,1))
monitor_score(scores, "Accuracy", "Accuracy", c(.5,1))
monitor_score(scores, "Kappa", "Kappa", c(0,.5))
monitor_score(scores, "TauxAchat-TOP-100", "TauxAchat-TOP-100", c(.5,1))
monitor_score(scores, "TauxAchat-TOP-200", "TauxAchat-TOP-200", c(.5,1))
monitor_score(scores, "TauxAchat-DOWN", "TauxAchat-DOWN", c(0,.5))
