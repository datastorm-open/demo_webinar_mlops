library(lubridate)
library(MLOpsMonitoring)

source("MLOpsMonitoring/R/dataset.R")

data <- import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")
dim(data)

### FOR ONE SPECIFIC TARGET PERIOD
# START_REP <- "2011-12-01"
# END_REP <- "2011-12-31"
# WINDOWS_MONTH <- c(3, 6, 12)
# agg_period <- create_features_on_period(data, START_REP, END_REP, WINDOWS_MONTH)
# dim(agg)
# colnames(agg)

### SEPARATED WINDOWS
# agg_target3 <- create_features_on_period(data, "2011-10-01", "2011-12-31", c(3, 6, 12))
agg_target1_windows3 <- create_features_on_period(data, "2011-05-01", "2011-05-30", c(1))
agg_target1_windows3 <- create_features_on_period(data, "2011-05-01", "2011-05-30", c(3))
agg_target1_windows12 <- create_features_on_period(data, "2011-05-01", "2011-05-30", c(12))


### CONCATENATE SEVERAL TARGET PERIODS
agg <- create_features(from = as.Date("2010/03/01"), to = as.Date("2011/12/01"), by = "month")
write.csv(agg, "/home/mmasson/data/mlops-wbr/features_several_y_period.csv", na = "", row.names = FALSE)
agg <- read.csv("/home/mmasson/data/mlops-wbr/features_several_y_period.csv", na = "", row.names = FALSE)

