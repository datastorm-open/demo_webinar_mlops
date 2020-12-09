output$density_scores <- renderAmCharts({
  # mini = min(features_train[[input$feature]])-0.1*abs(min(features_train[[input$feature]]))
  # maxi = max(features_train[[input$feature]])+0.1*abs(max(features_train[[input$feature]]))
  dist_aft = density(latest_batch()[,c(input$feature)], from=mini, to=maxi) 
  dist_bef = density(features_train[[input$feature]], from=mini, to=maxi)
  chart=amPlot(dist_aft$x, dist_aft$y, type="l", title="Dernier batch",
               xlab=input$feature, ylab="Densité", zoom=T, legend=T)
  amLines(chart,y=dist_bef$y, type="l", col="black", title="Données d'entraînement") 
  
})

