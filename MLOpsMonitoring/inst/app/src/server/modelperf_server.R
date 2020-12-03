makeMonitoringCharts <- function(dt, score, main, threshold){
  dt$alerting_threshold = threshold
  charts = amTimeSeries(dt, col_date="END", col_series=c(score, "alerting_threshold"), 
                        groupToPeriods = "MM", main=main, legend = FALSE, col=c("blue", "#AA1010"))
  charts@panels[[1]]$stockGraphs[[2]]$fillToAxis = "xAxis"
  charts@panels[[1]]$stockGraphs[[2]]$fillAlphas = .5
  charts@panels[[1]]$stockGraphs[[2]]$balloonText = "Zone d'alerte"
  return(charts)
}

output$auc_global <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "AUC_GLOBAL", "AUC Global", .6)})