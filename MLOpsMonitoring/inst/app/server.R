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
  
  last_batch_name <- reactive({min(names(features_batch)[names(features_batch)>=input$t])})
  scores_at_t <- reactive({scores[END<=input$t,]})
  
  makeMonitoringCharts <- function(dt, score, main, threshold){
    
    chpts = changepoint::cpt.mean(dt[[score]])@cpts
    
    dt$alerting_threshold = threshold
    charts = amTimeSeries(dt, col_date="END", col_series=c(score, "alerting_threshold"), 
                          groupToPeriods = "MM", main=main, legend = FALSE, col=c("blue", "#AA1010"))
    charts@panels[[1]]$stockGraphs[[2]]$fillToAxis = "xAxis"
    charts@panels[[1]]$stockGraphs[[2]]$fillAlphas = .5
    charts@panels[[1]]$stockGraphs[[2]]$balloonText = "Zone d'alerte"
    
    charts@panels[[1]]$categoryAxis = rAmCharts::categoryAxis()
    if(length(chpts)>1){
      for(rupt in chpts[1:(length(chpts)-1)]){
        charts@panels[[1]]$categoryAxis = addGuide(charts@panels[[1]]$categoryAxis, 
                                                   rAmCharts::guide(date = dt$END[rupt+1],
                                                                    lineColor = "#CC0000",
                                                                    lineAlpha = 1,
                                                                    dashLength = 2,
                                                                    inside = T,
                                                                    labelRotation = 90,
                                                                    label = "Possible rupture"))
      }
    }

    return(charts)
  }
  
  source("src/server/features_server.R", local = T)
  source("src/server/modelperf_server.R", local = T)
  source("src/server/driftscore_server.R", local = T)
  source("src/server/scoresdistrib_server.R", local = T)
})