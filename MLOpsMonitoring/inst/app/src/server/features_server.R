latest_batch <- reactive({
  periods = as.Date(names(features_batch), origin="1970-01-01")
  features_batch[[as.character(max(periods[periods<=input$t]))]]
})

output$qqplot <- renderAmCharts({
  qtl = 1:99*0.01
  dist_bef = quantile(latest_batch()[,c(input$feature)], qtl) 
  dist_aft = quantile(features_train[,c(input$feature)], qtl)
  lim=c(min(min(dist_bef), min(dist_aft)), max(max(dist_bef), max(dist_aft)))
  chart=amPlot(dist_bef, dist_aft, 
               xlim=lim, ylim=lim, 
               xlab="Quantiles du jeu d'entraînement", ylab="Quantiles du dernier batch")
  amLines(chart,y=unname(dist_bef), type="l", col="black")
})

