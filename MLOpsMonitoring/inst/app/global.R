rm(list=ls())
require(shiny)
require(shinyjs)
require(shinydashboard)
require(shinyWidgets)
require(shinymanager)
require(shinycssloaders)
require(openxlsx)
require(rAmCharts)
require(MLmetrics)
require(changepoint)

dev = TRUE

scores = as.data.table(read.csv("/home/mmasson/data/mlops-wbr/save_output_1208.csv"))
scores$cheatcode = c(rep(5, nrow(scores)/2), rep(9, 1+nrow(scores)/2))
scores$START = as.POSIXct.Date(as.Date(scores$START))
scores$END = as.POSIXct.Date(as.Date(scores$END))

drift_imp <- as.data.table(read.csv("/home/ngirard/Webinaire_MLOPS/data/save_output_importance_1208.csv"))

features_train = as.data.table(read.csv(paste0("/home/mmasson/data/mlops-wbr/save_features_train.csv")))
features_batch = list()
for(TARGET_start in seq.Date(from=as.Date("2010-08-01", origin="1970-01-01"), to=as.Date("2011-12-31", origin="1970-01-01"), by="month")){
  TARGET_start = as.Date(TARGET_start, origin="1970-01-01")
  TARGET_end = TARGET_start + base::months(1)
  features_batch[[as.character(TARGET_end)]] = read.csv(paste0("/home/mmasson/data/mlops-wbr/save_features_1208_",TARGET_start,".csv"))
}

features = setdiff(colnames(features_train), c("X", "Customer.ID", "VAR_REP", "MONTH", "YEAR"))
# is_similar = distrib_comparison(features_train, features_batch, features, verbose=F)

