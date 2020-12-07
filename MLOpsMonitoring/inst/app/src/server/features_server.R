latest_batch <- reactive({
  periods = as.Date(names(features_batch), origin="1970-01-01")
  features_batch[[as.character(max(periods[periods<=input$t]))]]
})

output$display_density <- reactive({any(!is_similar[last_batch_name(),])})
outputOptions(output, "display_density", suspendWhenHidden = FALSE)

output$kolmo_features <- renderInfoBox({
  alert = any(!is_similar[last_batch_name(),])
  infoBox(title = ifelse(alert,"Incohérence(s) entre nouvelles données et jeu d'entraînement", "Cohérence entre nouvelles données et jeu d'entraînement"), 
          value = ifelse(alert, paste(sum(!is_similar[last_batch_name(),]), "variable(s) différente(s)"), "Tout va bien"), 
          color = ifelse(alert, 'orange', 'green'), 
          width = 12)
})

observe({updateSelectInput(session, "feature", choices=colnames(is_similar)[which(!is_similar[last_batch_name(),])])})

output$qqplot <- renderAmCharts({
  qtl = 1:99*0.01
  dist_bef = quantile(latest_batch()[,c(input$feature)], qtl) 
  dist_aft = quantile(features_train[[input$feature]], qtl)
  lim=c(min(min(dist_bef), min(dist_aft)), max(max(dist_bef), max(dist_aft)))
  chart=amPlot(dist_bef, dist_aft, 
               xlim=lim, ylim=lim, 
               xlab="Quantiles du jeu d'entraînement", ylab="Quantiles du dernier batch")
  amLines(chart,y=unname(dist_bef), type="l", col="black")
})

output$density <- renderAmCharts({
  dist_aft = density(latest_batch()[,c(input$feature)]) 
  dist_bef = density(features_train[[input$feature]])
  #dt=merge(data.table(x=dist_bef$x, y_1=dist_bef$y), data.table(x=dist_aft$x, y_2=dist_aft$y), on="x", all=T)
  #chart=amPlot(dt$x, dt$y_1, type="l")
  #amLines(chart,y=dt$y_2, type="l", col="black")
  chart=amPlot(dist_aft$x, dist_aft$y, type="l")
  chart
})
