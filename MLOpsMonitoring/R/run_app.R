#' @title Run App
#'
#' @description Run App
#'
#' @import shiny
#' @import shinyjs
#' @import shinydashboard
#' @import shinycssloaders
#' @import data.table
#' @import openxlsx
#' @import rAmCharts
#' @import MLmetrics
#' @import changepoint
#' @import DT
#'
#' @export 
run_app <- function(host = "0.0.0.0", port = 3838) {
  shiny::shinyAppDir(appDir = system.file(file.path("app"),
                                          package = "MLOpsMonitoring"),
                     options = list(host = host, port = port))
  
}