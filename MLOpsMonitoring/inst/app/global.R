rm(list=ls())
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(shinymanager)
require(shinycssloaders)
require(openxlsx)
require(rAmCharts)

dev = TRUE

scores = read.csv("/home/mmasson/data/mlops-wbr/save_output_1203.csv")