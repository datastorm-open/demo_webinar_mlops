library(MLOpsMonitoring)

data <- MLOpsMonitoring::import_dataset("/home/ngirard/Webinaire_MLOPS/data/uk-retailer-ii.xlsx")
dim(data)

### FOR ONE SPECIFIC TARGET PERIOD
# START_REP <- "2011-12-01"
# END_REP <- "2011-12-31"
# WINDOWS_MONTH <- c(3, 6, 12)
# agg_period <- MLOpsMonitoring::create_features_on_period(data, START_REP, END_REP, WINDOWS_MONTH)
# dim(agg)
# colnames(agg)

### SEPARATED WINDOWS
agg_target1_windows1_lapse <- MLOpsMonitoring::create_features_on_period(data, "2011-05-01", "2011-05-30", c("M-1", "M-2", "M-3"), kind="lag")
agg_target1_windows1_cumm <- MLOpsMonitoring::create_features_on_period(data, "2011-05-01", "2011-05-30", c("M-1", "M-2", "M-3"), kind="cumulative")

agg_target1_windows3_cumm <- MLOpsMonitoring::create_features_on_period(data, "2011-05-01", "2011-05-30", c("M-3", "M-6", "M-12"), kind="cumulative")
# equivalent to
agg_target1_windows3_cumm <- MLOpsMonitoring::create_features_on_period(data, "2011-05-01", "2011-05-30", c("T-1", "T-2", "T-3"), kind="cumulative") 

agg_target1_windows3_lapse <- MLOpsMonitoring::create_features_on_period(data, "2011-05-01", "2011-05-30", c("T-1", "T-2", "T-3"), kind="lag")
agg_target1_windows12 <- MLOpsMonitoring::create_features_on_period(data, "2011-05-01", "2011-05-30", c("Y-1"))


### CONCATENATE SEVERAL TARGET PERIODS
# agg <- MLOpsMonitoring::create_features(from = as.Date("2010-03-01"), to = as.Date("2011-12-01"), by = "month", windows=c("M-1", "M-2", "M-3"), kind="lag")
agg <- MLOpsMonitoring::create_features(from = as.Date("2010-03-01"), to = as.Date("2011-04-01"), by = "month", windows=c("M-1", "M-2", "M-3"), kind="lag")
# write.csv(agg, "/home/mmasson/data/mlops-wbr/features_several_y_period.csv", na = "", row.names = FALSE)
# agg <- read.csv("/home/mmasson/data/mlops-wbr/features_several_y_period.csv")

