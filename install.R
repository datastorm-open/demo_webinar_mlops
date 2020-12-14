options(repos = structure(c(CRAN = 'https://cloud.r-project.org')))
install.packages("remotes")
install.packages(c("DT", "MLmetrics", "caret", "changepoint", "data.table", "lubridate", "openxlsx", "rAmCharts", "shiny", "shinycssloaders", "shinydashboard", "shinyjs", "stringr", "xgboost"))
remotes::install_local("./MLOpsMonitoring_0.1.0.tar.gz", dependencies = T)
