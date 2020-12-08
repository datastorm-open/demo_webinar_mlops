rm(list=ls())
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinymanager)
require(shinycssloaders)
require(openxlsx)
require(rAmCharts)
require(MLmetrics)
require(changepoint)

dev = TRUE

scores = as.data.table(read.csv("/home/mmasson/data/mlops-wbr/save_output_1207.csv"))
scores$cheatcode = c(rep(5, nrow(scores)/2), rep(9, nrow(scores)/2))
scores$START = as.POSIXct.Date(as.Date(scores$START))
scores$END = as.POSIXct.Date(as.Date(scores$END))

drift_imp <- as.data.table(read.csv("/home/ngirard/Webinaire_MLOPS/data/save_output_importance_1208.csv"))

features_train = as.data.table(read.csv(paste0("/home/mmasson/data/mlops-wbr/save_features_train.csv")))
features_batch = list()
for(TARGET_start in seq.Date(from=as.Date("2011-01-01", origin="1970-01-01"), to=as.Date("2011-12-31", origin="1970-01-01"), by="month")){
  TARGET_start = as.Date(TARGET_start, origin="1970-01-01")
  TARGET_end = TARGET_start + base::months(1)
  features_batch[[as.character(TARGET_end)]] = read.csv(paste0("/home/mmasson/data/mlops-wbr/save_features_",TARGET_start,".csv"))
}

features = setdiff(colnames(features_train), c("X", "Customer.ID", "VAR_REP", "MONTH", "YEAR"))
distrib_comparison <- function(train, list_of_datasets, features, threshold=0.05, verbose=T){
  options(warn=ifelse(verbose,0,-1))
  kolma_test=data.table()
  for(var in features){
    tmp = c()
    for(elem in names(list_of_datasets)){
      tmp = c(tmp, ks.test(train[[var]], list_of_datasets[[elem]][[var]])$p.value)   
    }
    tmp=data.table(tmp)
    names(tmp)=var
    kolma_test=cbind(kolma_test, tmp)
  }
  rownames(kolma_test) = names(list_of_datasets)
  options(warn=0)
  return(kolma_test>threshold)
}
# is_similar = distrib_comparison(features_train, features_batch, features, verbose=F)



