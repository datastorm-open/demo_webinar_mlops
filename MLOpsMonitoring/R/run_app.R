#' @title Run App
#'
#' @description Run App
#'
#' @import shiny
#'
#' @export 
run_app <- function(host = "0.0.0.0", port = 3838) {
  shiny::shinyAppDir(appDir = system.file(file.path("app"),
                                          package = "MLOpsMonitoring"),
                     options = list(host = host, port = port))
  
}

#' @title Run App for Developpement
#'
#' @description Run App for Developpement
#'
#' @import shiny
#'
#' @export 
run_app_dev <- function(host = "0.0.0.0", port = 3838) {
  shiny::shinyAppDir(appDir = system.file(file.path("app"),
                                          package = "MLOpsMonitoring"),
                     options = list(launch.browser=TRUE))
  
}