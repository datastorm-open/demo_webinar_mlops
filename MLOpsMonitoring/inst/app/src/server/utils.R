makeMonitoringCharts <- function(dt, score, main, threshold, date_drift=NULL, top=F){
  
  if(score=="ACC_GLOBAL"){
    #browser()    
  }
  
  if(!is.null(date_drift)){
    chpts = date_drift
  }else if(length(dt[[score]])>4){
    chpts = changepoint::cpt.meanvar(dt[[score]])@cpts
    chpts = dt$END[chpts+1]
    chpts = chpts[!is.na(chpts)]
  }else{
    chpts = c()
  }
  
  dt$alerting_threshold = threshold
  if(top && max(dt[[score]])>=threshold){
    dt$max=max(dt[[score]])
    to_plot = c(score, "alerting_threshold", "max")
  } else {
    to_plot = c(score, "alerting_threshold")
  }
  
  charts = amTimeSeries(dt, col_date="END", col_series=to_plot, groupToPeriods = "MM", 
                        main=main, legend = FALSE, col=c("blue", "#AA1010", "#AA1010"))
  
  if(top){
    if(max(dt[[score]])>=threshold){
      charts@panels[[1]]$stockGraphs[[2]]$fillToGraph = "max"   
      charts@panels[[1]]$stockGraphs[[2]]$fillAlphas = .5
      charts@panels[[1]]$stockGraphs[[3]]$balloonText = "Maximum atteint"
    }
  } else {
    charts@panels[[1]]$stockGraphs[[2]]$fillToAxis = "xAxis"    
    charts@panels[[1]]$stockGraphs[[2]]$fillAlphas = .5
  }
  charts@panels[[1]]$stockGraphs[[2]]$balloonText = "Zone d'alerte"
  

  charts@panels[[1]]$categoryAxis = rAmCharts::categoryAxis()
  if(length(chpts)>0){
    for(rupt in chpts){
      charts@panels[[1]]$categoryAxis = addGuide(charts@panels[[1]]$categoryAxis, 
                                                 rAmCharts::guide(date = rupt,
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

checkUp <- function(scores, threshold){
  km_features = sum(!is_similar()[last_batch_name(),])
  if(km_features>0){
    alerts$km_features = notificationItem(
      text = "Les données en entrée ne correspondent pas totalement à celles de la période d'apprentissage",
      icon("users"),
      status = "primary"
    )
  }else{
    alerts$km_features = NULL
  }
  
  row=scores[nrow(scores)]
  if(row$Kappa<threshold$Kappa | row$AUC_GLOBAL<threshold$AUC | row$ACC_GLOBAL<threshold$ACC | row$TauxAchat.TOP.100<threshold$TauxAch | row$LogLoss>threshold$LogLoss){
    alerts$model_perf = notificationItem(
      text = "Les performances du modèle sont mauvaises",
      icon("car-crash"),
      status = "danger"
    )    
  }else{
    alerts$model_perf = NULL
  }
  
  if(row$DRIFT_AUC > threshold$Drift_AUC | row$DRIFT_MATTHEWWS > threshold$Drift_Matt){
    alerts$datadrift = notificationItem(
      text = "Un data drift a été détecté",
      icon("chart-line"),
      status = "warning"
    )    
  }else{
    alerts$datadrift = NULL
  }
}

# Retourne les variables dont les distrib sont SIMILAIRES ! 
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