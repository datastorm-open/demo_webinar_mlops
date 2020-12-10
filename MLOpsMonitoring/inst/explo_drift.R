#### CHANGE POINT ON SCORES


#ind = "AUC_GLOBAL"
#ind = "ACC_GLOBAL"
#ind = "Kappa"
#ind = "TauxAchat.TOP.100"
colnames(scores)

for(k in 4:nrow(scores)){
  print(changepoint::cpt.mean(scores[1:k,][[ind]])@cpts)
}

for(k in 4:nrow(scores)){
  print(changepoint::cpt.meanvar(scores[1:k,][[ind]])@cpts)
}

for(k in 4:nrow(scores)){
  print(changepoint::cpt.var(scores[1:k,][[ind]])@cpts)
}




#### CHANGE POINT ON "HAVING WRONG PROBA"

predictions[,"HAVING_WRONG"] = (predictions$PRED>=.5)*(1-predictions$ACTUAL) + predictions$ACTUAL*(1-(predictions$PRED>=.5))
plot(rollmean(predictions[,"HAVING_WRONG"], 150))

for(lim in seq.Date(as.Date("2011-03-01", origin="1970-01-01"), 
                    as.Date("2011-12-01", origin="1970-01-01"), "month")){
  lim = as.Date(lim, origin="1970-01-01")
  print(paste(lim, predictions[changepoint::cpt.var(predictions[START<lim]$HAVING_WRONG)@cpts[1], START]))
}





predictions=as.data.table(read.csv("/home/mmasson/data/mlops-wbr/save_output_predictions_1210.csv"))
predictions=as.data.table(read.csv("/home/ngirard/Webinaire_MLOPS/data/save_output_predictions_1209.csv"))

for(lim in seq.Date(as.Date("2011-03-01", origin="1970-01-01"), 
                    as.Date("2011-12-01", origin="1970-01-01"), "month")){
  lim = as.Date(lim, origin="1970-01-01")
  print(paste(lim, mnt$predictions[changepoint::cpt.var(mnt$predictions[START<lim]$PRED)@cpts[1], START]))
}


plot(rollmean(mnt$predictions$PRED, 150))

