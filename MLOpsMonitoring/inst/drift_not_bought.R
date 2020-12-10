library(MLOpsMonitoring)

data  <- MLOpsMonitoring::import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")

# write.csv(data, file="/home/ngirard/Webinaire_MLOPS/data/data_1209.csv")

train_6M <- MLOpsMonitoring::create_features_on_period(data, 
                                                       start_rep=as.Date("2010-07-01"), 
                                                       end_rep=as.Date("2010-07-31"),
                                                       windows=c("M-6"), kind="cumulative")
rf_6M = train_rf(train_6M, rep_factor = TRUE)
#write.csv(train_6M, file=paste0("/home/mmasson/data/mlops-wbr/save_features_train.csv"))


data_drifted <- MLOpsMonitoring::add_selected_but_no_bought(data, 
                                                            as.Date("2011-03-01"), as.Date("2011-06-01"), "month",
                                                            min_customer=666, max_customer=1024, 
                                                            odd_new_customer=0.5, 
                                                            mu_sku=100, sigma_sku=50, 
                                                            mu_qtty=50, sigma_qtty=40)

mnt <- MLOpsMonitoring::monitoring_main(data_drifted, 
                                        train_6M[, -c("Customer.ID", "VAR_REP", "YEAR")], 
                                        "2010-08-01", "2011-12-31", rf_6M$rf)


write.csv(mnt$scores, file="/home/mmasson/data/mlops-wbr/save_output_1210.csv")
# scores = read.csv("/home/mmasson/data/mlops-wbr/save_output_1210.csv")

write.csv(mnt$scores, file="/home/mmasson/data/mlops-wbr/save_output_scores_1210.csv", row.names = FALSE)
write.csv(mnt$drift_imp, file="/home/mmasson/data/mlops-wbr/save_output_drift_imp_1210.csv", row.names = FALSE)
write.csv(mnt$predictions, file="/home/mmasson/data/mlops-wbr/save_output_predictions_1210.csv", row.names = FALSE)

# Statistiques descriptives:
# train_6M[, me]


for(a in names(mnt$features_batch)){
  write.csv(mnt$features_batch[[a]],
            file=paste0("/home/mmasson/data/mlops-wbr/save_features_1210_",a,".csv"))
}


######## PLOTS

scores = mnt$scores
dates = as.Date(names(mnt$features_batch))

plot(dates, scores$LogLoss, type="l")
plot(dates, scores$AUC_GLOBAL, type="l")
plot(dates, scores$ACC_GLOBAL, type="l")
plot(dates, scores$`TauxAchat-TOP-100`, type="l")
plot(dates, scores$`TauxAchat-TOP-200`, type="l")
