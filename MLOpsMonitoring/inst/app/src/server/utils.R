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

checkUp <- function(scores){
  km_features = sum(!is_similar()[last_batch_name(),])
  if(km_features>0){
    alerts$km_features = notificationItem(
      text = "Les données en entrée ne correspondent pas totalement à celles de la période d'apprentissage",
      icon("users"),
      status = "warning"
    )
  }else{
    alerts$km_features = NULL
  }
  
  row=scores[nrow(scores)]
  if(row$Kappa<.3 | row$AUC_GLOBAL<.6 | row$ACC_GLOBAL<.3 | row$TauxAchat.TOP.100<.7 | row$LogLoss>3){
    alerts$model_perf = notificationItem(
      text = "Les performances du modèle sont mauvaises",
      icon("users"),
      status = "danger"
    )    
  }else{
    alerts$model_perf = NULL
  }
}

distrib_comparison <- function(train, list_of_datasets, features, threshold=0.05, verbose=T){
  options(warn=ifelse(verbose,0,-1))
  kolma_test=data.table()
  for(var in features){
    tmp = c()
    for(elem in names(list_of_datasets)){
      tmp = c(tmp, ks.test(train[[var]], list_of_datasets[[elem]][[var]])$p.value)   
    }
    tmp=data.table(tmp)
    names(tmp)=var
    kolma_test=cbind(kolma_test, tmp)
  }
  rownames(kolma_test) = names(list_of_datasets)
  options(warn=0)
  return(kolma_test>threshold)
}