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