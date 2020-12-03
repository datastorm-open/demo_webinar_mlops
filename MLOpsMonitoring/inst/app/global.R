rm(list=ls())
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinymanager)
require(shinycssloaders)
require(openxlsx)
require(rAmCharts)

dev = TRUE

scores = as.data.table(read.csv("/home/mmasson/data/mlops-wbr/save_output_1203.csv"))
scores$START = as.POSIXct.Date(as.Date(scores$START))
scores$END = as.POSIXct.Date(as.Date(scores$END))

features_train = read.csv(paste0("/home/mmasson/data/mlops-wbr/save_features_train.csv"))
features_batch = list()
for(TARGET_start in seq.Date(from=as.Date("2011-01-01", origin="1970-01-01"), to=as.Date("2011-12-31", origin="1970-01-01"), by="month")){
  TARGET_start = as.Date(TARGET_start, origin="1970-01-01")
  TARGET_end = TARGET_start + base::months(1)
  features_batch[[as.character(TARGET_end)]] = read.csv(paste0("/home/mmasson/data/mlops-wbr/save_features_",TARGET_start,".csv"))
}
