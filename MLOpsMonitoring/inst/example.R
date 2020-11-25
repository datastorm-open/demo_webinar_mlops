library(lubridate)
library(MLOpsMonitoring)

source("MLOpsMonitoring/R/dataset.R")

data <- import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")
dim(data)

### FOR ONE SPECIFIC TARGET PERIOD
START_REP <- "2011-12-01"
END_REP <- "2011-12-31"
WINDOWS_MONTH <- c(3, 6, 12)
agg_period <- create_features_on_period(data, START_REP, END_REP, WINDOWS_MONTH)

### CONCATENATE SEVERAL TARGET PERIODS
agg <- create_features(from = as.Date("2010/03/01"), to = as.Date("2011/12/01"), by = "month")
write.csv(agg, "/home/mmasson/data/mlops-wbr/features_several_y_period.csv", na = "", row.names = FALSE)
agg <- read.csv("/home/mmasson/data/mlops-wbr/features_several_y_period.csv", na = "", row.names = FALSE)
