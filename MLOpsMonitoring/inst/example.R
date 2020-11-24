library(lubridate)
library(MLOpsMonitoring)

data <- import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")

dim(data)


START_REP <- "2011-12-01"
END_REP <- "2011-12-31"
WINDOWS_MONTH <- c(3, 6, 12)

agg <- create_var_reponse(data, START_REP, END_REP)

for(windows in WINDOWS_MONTH){
  all_customers = data[, .(Customer.ID = unique(Customer.ID))]
  sub_data_agg <- create_subset_data(data, START_REP, windows)
  
  agg <- create_features(sub_data_agg, all_customers, windows)
}

dim(agg)
colnames(agg)
