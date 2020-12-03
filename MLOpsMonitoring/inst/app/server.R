shinyServer(function(input, output, session) {
  
  if(!dev){
    res_auth <- secure_server(check_credentials = check_credentials(data.frame(
      user = c("mlops"), # mandatory
      password = c("webinar"), # mandatory
      start = c("2019-04-15"), # optinal (all others)
      expire = c(NA),
      admin = c(TRUE),
      stringsAsFactors = FALSE
    ))) 
  }
  
  scores_at_t <- reactive({scores[STATUS=="pertubé"&END<=input$t,]})
  
  makeMonitoringCharts <- function(dt, score, main, threshold){
    dt$alerting_threshold = threshold
    charts = amTimeSeries(dt, col_date="END", col_series=c(score, "alerting_threshold"), 
                          groupToPeriods = "MM", main=main, legend = FALSE, col=c("blue", "#AA1010"))
    charts@panels[[1]]$stockGraphs[[2]]$fillToAxis = "xAxis"
    charts@panels[[1]]$stockGraphs[[2]]$fillAlphas = .5
    charts@panels[[1]]$stockGraphs[[2]]$balloonText = "Zone d'alerte"
    return(charts)
  }
  
  
  # data = import_dataset()
  # fct = density(data$Quantity)
  # amPlot(fct$x, fct$y)
  
  
  source("src/server/modelperf_server.R", local = T)
  source("src/server/driftscore_server.R", local = T)
})