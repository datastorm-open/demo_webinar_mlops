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
  
  agg_pt1 = create_agg_prix_qty(sub_data_agg, all_customers)
  colnames(agg_pt1)[-1] <- paste0(colnames(agg_pt1)[-1], "_", windows, "M")
  agg <- merge(agg, agg_pt1, by = "Customer.ID")
  
  agg_pt2 = sub_data_agg[, create_agg_freq_cncl(.SD), by="Customer.ID"]
  colnames(agg_pt2)[-1] <- paste0(colnames(agg_pt2)[-1], "_", windows, "M")
  
  agg <- merge(agg, agg_pt2, by = "Customer.ID", all.x=TRUE)
  agg[is.na(agg)] <- 0
}

dim(agg)
colnames(agg)
