rm(list=ls())
require(shiny)
require(shinyjs)
require(shinydashboard)
#require(shinymanager)
require(shinycssloaders)
require(openxlsx)
require(data.table)
require(rAmCharts)
require(MLmetrics)
require(changepoint)
require(DT)
require(lubridate)
require(Metrics)

dev = TRUE

threshold = list("AUC" = .6,
              "ACC" = .6,
              "Kappa" = .2,
              "TauxAch" = .7,
              "LogLoss" = 3.5, 
              "Drift_AUC" = .7,
              "Drift_Matt" = .5)


print("Importing outputs...")
scores = as.data.table(read.csv("../data/save_output_1210.csv"))
scores$cheatcode = c(rep(5, nrow(scores)/2), rep(9, 1+nrow(scores)/2))
scores$START = as.POSIXct.Date(as.Date(scores$START))
scores$END = as.POSIXct.Date(as.Date(scores$END))

# Importance des variables (drift score)
drift_imp <- as.data.table(read.csv("../data/save_output_drift_imp_1210.csv"))


# Predictions
predictions <- as.data.table(read.csv("../data/save_output_predictions_1210.csv"))
predictions[, START := as.Date(START)]
predictions[, END := as.Date(END)]
predictions[,"HAVING_WRONG"] = (predictions$PRED>=.5)*(1-predictions$ACTUAL) + predictions$ACTUAL*(1-(predictions$PRED>=.5))

change_point = c()
for(sep in as.Date(scores$END)){
  change_point = c(change_point, predictions[changepoint::cpt.var(predictions[START<as.Date(sep)]$HAVING_WRONG)@cpts[1], START])
}
scores[["RUPT_EST"]] = change_point


features_train = as.data.table(read.csv(paste0("../data/save_features_train.csv")))
features_batch = list()
for(TARGET_start in seq.Date(from=as.Date("2010-08-01", origin="1970-01-01"), to=as.Date("2011-12-31", origin="1970-01-01"), by="month")){
  TARGET_start = as.Date(TARGET_start, origin="1970-01-01")
  TARGET_end = TARGET_start + base::months(1)
  features_batch[[as.character(TARGET_end)]] = read.csv(paste0("../data/save_features_1210_",TARGET_start,".csv"))
}

features = setdiff(colnames(features_train), c("X", "Customer.ID", "VAR_REP", "MONTH", "YEAR"))
# is_similar = distrib_comparison(features_train, features_batch, features, verbose=F)

# Data raw
print("Importing complete dataset...")
data_raw <- MLOpsMonitoring::import_dataset("../data/uk-retailer-ii.xlsx")

# Dates slider:
min_date = as.Date("2010-09-01","%Y-%m-%d")
max_date = as.Date("2012-01-01","%Y-%m-%d")
list_dates <- seq.Date(min_date, max_date, by="month")
list_dates_format <- unlist(lapply(list_dates, function(x) format(x, "%b %Y")))

print("Starting..")