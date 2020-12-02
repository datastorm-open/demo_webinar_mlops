library(MLOpsMonitoring)
library(TTR)
library(caret)
library(psych)
library(ggplot2)

keywords = c("TEA", "CANDLE", "LUNCH BAG", "CARD", "DECORATION", "T-LIGHT", "HOT WATTER BOTTLE")
more_keyword = c("CHRISTMAS", "UMBRELLA", "MIROR", "WALL ART", "TISSUES", "BOWL", "BRACELET",
                 "DRESS", "T-SHIRT",  "FRAME", "COVER")
more_more_keyword = c("MUG", "BOTTLE",
                      "DOILY", "DOILIES", "MIROR", "WALL ART", "LIGHT", "BRACELET",
                      "BOX", "GLASS", "RUBBER", "PENCIL", "PEN", "DRESS", "T-SHIRT", "MAGNET", "FRAME",
                      "ORGANISER", "ERASER", "NECKL", "CLOCK", "LAMP", "STICKER", "COVER")

data <- MLOpsMonitoring::import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")
cnt <-  MLOpsMonitoring::count_keywords(unique(data$Description), unique(c(keywords, more_keyword, more_more_keyword)))

# train model
agg <- MLOpsMonitoring::create_features(data, from = as.Date("2010-03-01"), to = as.Date("2011-04-01"), by = "month", windows=c("M-1", "M-2", "M-3"), kind="lag")
xgboost = MLOpsMonitoring::train_xgboost(agg)
xgboost$auc

# add drift
start = "2011-05-01"
end =  "2012-01-01"
data_wth_drift = copy(data)
for (categ in unique(c(keywords, more_keyword, more_more_keyword))){
  cond = as.logical(cnt$dt[data$Description, categ]) & data$InvoiceDate>start & data$InvoiceDate<end & data$Quantity>0
  
  ## Les produits disparaissent, les clients se rabattent sur d'autres produits
  # data_wth_drift[cond, c("StockCode", "Description")] = data[sample(1:nrow(data), sum(cond)), c("StockCode", "Description")]
  
  ## Les produits sont vendus par lots, avec une promotion sur le prix
  data_wth_drift[as.logical(cnt$dt[data_wth_drift$Description, categ]) & InvoiceDate>start & InvoiceDate<end & Quantity>0, Price:=0.5*Price*Quantity]
  data_wth_drift[as.logical(cnt$dt[data_wth_drift$Description, categ]) & InvoiceDate>start & InvoiceDate<end & Quantity>0, Quantity:=(1)]
  
  ## Les prix ont baissé donc la quantité achetée a augmenté
  # data_wth_drift[as.logical(cnt$dt[data_wth_drift$Description, categ]) & InvoiceDate>start & InvoiceDate<end & Quantity>0, Price:=0.25*Price]
  # data_wth_drift[as.logical(cnt$dt[data_wth_drift$Description, categ]) & InvoiceDate>start & InvoiceDate<end & Quantity>0, Quantity:=round(Quantity*1.5)]
  
  ## La quantité acheté est divisée par 2
  # data_wth_drift[cond, Price:=Price/2]
  # data_wth_drift[cond, Quantity:=(Quantity%/%2+1)]

  ## Les quantités et prix les plus rares deviennent les plus fréquents
  # pricies = data[cond, Price]
  # qtties  = data[cond, Quantity]
  # data_wth_drift[cond,]$Price = as.double(lapply(pricies, function(x, m){return(round(runif(1, x, m)))}, max(pricies)))
  # data_wth_drift[cond,]$Quantity = as.double(lapply(qtties, function(x, m){return(round(runif(1, x, m)))}, max(qtties)))
}

# plot distrib par mois par categ
plot_distrib_categ <- function(start = "2011-05-01", categ = "CARD"){
  end =  as.Date(start)+months(1)
  p <- ggplot(data_wth_drift[as.logical(cnt$dt[data_wth_drift$Description, categ]) & InvoiceDate>start & InvoiceDate<end,], aes(x=Quantity)) + 
    geom_density()
  p
}
plot_distrib_categ("2011-02-01", "LUNCH BAG")
plot_distrib_categ("2011-03-01", "LUNCH BAG")
plot_distrib_categ("2011-04-01", "LUNCH BAG")
plot_distrib_categ("2011-05-01", "LUNCH BAG")
plot_distrib_categ("2011-06-01", "LUNCH BAG")


# test on drifted dataset
test <- MLOpsMonitoring::create_features(data, 
                                         from = as.Date("2011-04-01"), 
                                         to = as.Date("2012-01-01"), 
                                         by = "month", 
                                         windows=c("M-1", "M-2", "M-3"), kind="lag")
test_wth_drift <- MLOpsMonitoring::create_features(data_wth_drift, 
                                                   from = as.Date("2011-04-01"), 
                                                   to = as.Date("2012-01-01"), 
                                                   by = "month", 
                                                   windows=c("M-1", "M-2", "M-3"), kind="lag")

acc_ok = c()
auc_ok = c()
acc_ko = c()
auc_ko = c()
kap_ok = c()
kap_ko = c()
for(month in 4:12){
  pred_test_wth_drift <- predict(xgboost$model, test_wth_drift[MONTH==month, -c("Customer.ID", "VAR_REP")], type="prob")[, 2]
  pred_test <- predict(xgboost$model, test[MONTH==month, -c("Customer.ID", "VAR_REP")], type="prob")[, 2]
  drift_auc <- round(MLOpsMonitoring:::auc(actual = test_wth_drift[MONTH==month, VAR_REP], pred = pred_test_wth_drift), 5)
  ref_auc <- round(MLOpsMonitoring:::auc(actual = test[MONTH==month, VAR_REP], pred = pred_test), 5)

  real = test[MONTH==month, VAR_REP]
  kap_ok = c(kap_ok, cohen.kappa(data.table("pred"=as.double(pred_test>.5), "real"=real))$kappa)
  acc_ok = c(acc_ok, round(mean((pred_test>.5) == real),3))
  auc_ok = c(auc_ok, ref_auc)
  kap_ko = c(kap_ko, cohen.kappa(data.table("pred"=as.double(pred_test_wth_drift>.5), "real"=real))$kappa)
  acc_ko = c(acc_ko, round(mean((pred_test_wth_drift>.5) == real),3))
  auc_ko = c(auc_ko, drift_auc)
}

monitor_score <- function(score_ok, score_ko, ylab){
  plot(auc_ok, type='l', ylim=c(.5,1), col="blue", main='Dérive', ylab=ylab)
  lines(auc_ko)
  legend("bottomleft",legend=c('Témoin', 'Avec dérive'), col=c('blue', 'black'), lty=1,lwd=2)
}

plot_score(auc_ok, auc_ko, "AUC")
plot_score(kap_ok, kap_ko, "Kappa")
plot_score(acc_ok, acc_ko, "Accuracy")

#####################
### Distrib. categ  #
#####################

boxplot(lapply(keywords, function(x){data[as.logical(cnt$dt[data$Description,x]) & (Quantity>0),Price]}), 
        ylab="Distribution prix", names=keywords)

boxplot(lapply(keywords, function(x){data[as.logical(cnt$dt[data$Description,x]) & (Quantity>0),Quantity]}), 
        ylab="Distribution quantité", names=keywords)