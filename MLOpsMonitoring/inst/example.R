library(lubridate)
library(MLOpsMonitoring)

source('MLOpsMonitoring/R/dataset.R')

data <- import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")

dim(data)


# START_REP <- "2011-12-01"
# END_REP <- "2011-12-31"
# WINDOWS_MONTH <- c(3, 6, 12)
# 
# agg <- create_var_reponse(data, START_REP, END_REP)
# 
# for(windows in WINDOWS_MONTH){
#   all_customers = data[, .(Customer.ID = unique(Customer.ID))]
#   sub_data_agg <- create_subset_data(data, START_REP, windows)
#   
#   agg <- create_features(sub_data_agg, all_customers, windows)
# }
# 
# dim(agg)
# colnames(agg)


# Target à 3 mois
create_complete_dataset <- function(data, start_rep, end_rep, windows_vect){
  agg <- create_var_reponse(data, start_rep, end_rep)

  for(windows in windows_vect){
    all_customers <- data[, .(Customer.ID = unique(Customer.ID))]
    sub_data_agg <- create_subset_data(data, start_rep, windows)
    
    agg <- merge(agg, create_features(sub_data_agg, all_customers, windows), by="Customer.ID")
  }
  return(agg)
}


# agg_target3 <- create_complete_dataset(data, "2011-10-01", "2011-12-31", c(3, 6, 12))



agg_target1_windows3 <- create_complete_dataset(data, "2011-05-01", "2011-05-30", c(1))
agg_target1_windows3 <- create_complete_dataset(data, "2011-05-01", "2011-05-30", c(3))
agg_target1_windows12 <- create_complete_dataset(data, "2011-05-01", "2011-05-30", c(12))



