output$drift_auc <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "DRIFT_AUC", "DRIFT AUC", threshold$Drift_AUC, date_drift=est_rupt_date(), top=T)})

output$drift_matth <- renderAmCharts({makeMonitoringCharts(copy(scores_at_t()), "DRIFT_MATTHEWWS", "DRIFT MATTHEWS", threshold$Drift_Matt, date_drift=est_rupt_date(), top=T)})

output$drift_imp <- renderAmCharts({
  start_month_input <- input$t
  day(start_month_input) <- 1
  end_month_input <- input$t
  day(end_month_input) <- 1
  end_month_input <- end_month_input + base::months(1) 
  subset_drift_imp <- drift_imp[START== start_month_input & END == end_month_input]
  subset_drift_imp <- subset_drift_imp[order(IMPORTANCE, decreasing = TRUE)]
  subset_drift_imp[, IMPORTANCE := round(IMPORTANCE, 2)]
  amBarplot(x = "VARIABLES", y = "IMPORTANCE", data = subset_drift_imp,
            # main=paste0("TOP 15/", nrow(subset_drift_imp) ," IMPORTANCE VARIABLES MODELE DRIFT SCORE"), 
            main="IMPORTANCE VARIABLES MODELE DRIFT SCORE", 
            horiz=TRUE) 
  
})