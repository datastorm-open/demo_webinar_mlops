scores_predictions_before <- reactive({
  date_input <- input$t
  lubridate::day(date_input) <- 1
  pred_before <- predictions[START < date_input, PRED]
  pred_before
})


scores_predictions_batch <- reactive({
  date_input <- input$t
  lubridate::day(date_input) <- 1
  pred_batch <- predictions[START == date_input, PRED]
  pred_batch
})

output$density_scores <- renderAmCharts({
  # date_input <- input$t
  # day(date_input) <- 1
  # pred_before <- predictions[START < date_input, PRED]
  # pred_batch <- predictions[START == date_input, PRED]
  dist_aft = density(scores_predictions_batch(), from=0, to=1) 
  max_plot <- max(dist_aft$y)
  if(length(scores_predictions_before()) > 0){
    dist_bef = density(scores_predictions_before(), from=0, to=1)
    max_plot <- max(max(dist_aft$y), max(dist_bef$y))
  }
  chart=amPlot(dist_aft$x, round(dist_aft$y, 2), type="l", title="Prédictions du dernier batch",
               xlab="Score", ylab="Densité", zoom=T, legend=T, ylim=c(0,max_plot*1.1))
  amLines(chart,y=round(dist_bef$y, 2), type="l", col="black", title="Précédentes prédictions") 

})

output$rmse_diag <- renderInfoBox({
  # pvalue <- ks.test(scores_predictions_before(), scores_predictions_batch())$p.value
  qtl = 1:99*0.01
  dist_bef = quantile(scores_predictions_before(), qtl) 
  dist_aft = quantile(scores_predictions_batch(), qtl)
  res_rmse <- round(Metrics::rmse(dist_bef, dist_aft), 3)
  infoBox("RMSE à la diagonale", res_rmse , color = ifelse(res_rmse < 0.05, "green", "orange"),
          width = 12, icon=icon("not-equal"))
})


output$qqplot_scores <- renderAmCharts({
  qtl = 1:99*0.01
  dist_bef = quantile(scores_predictions_before(), qtl) 
  dist_aft = quantile(scores_predictions_batch(), qtl)
  lim=c(min(min(dist_bef), min(dist_aft)), max(max(dist_bef), max(dist_aft)))
  chart=amPlot(dist_bef, dist_aft, 
               xlim=lim, ylim=lim, 
               xlab="Quantiles précédentes prédictions", ylab="Quantiles du dernier batch",
               main = "QQ-Plot")
  amLines(chart,y=unname(dist_bef), type="l", col="black") 
})

output$log_rupt_date <- renderDT({
  out = scores[END<as.Date(input$t), .("Date d'estimation"=END, "Date de rupture"=RUPT_EST)]
  setorder(out, -"Date d'estimation")
  out
})