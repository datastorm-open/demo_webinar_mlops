#' @title Run App
#'
#' @description Run App
#'
#' @param host \code{character}. See \code{\link{shinyAppDir}} (options).
#' @param port \code{integer}. See \code{\link{shinyAppDir}} (options)..
#'
#' @import shiny
#'
#' @export 
run_app <- function(host = "0.0.0.0", port = 3838) {
  shiny::shinyAppDir(appDir = system.file(file.path("app"),
                                          package = "MLOpsMonitoring"),
                     options = list(host = host, port = port))
  
}