output$density_scores <- renderAmCharts({
  # mini = min(features_train[[input$feature]])-0.1*abs(min(features_train[[input$feature]]))
  # maxi = max(features_train[[input$feature]])+0.1*abs(max(features_train[[input$feature]]))
  date_input <- input$t
  day(date_input) <- 1
  pred_before <- predictions[START < date_input, PRED]
  pred_batch <- predictions[START == date_input, PRED]
  dist_aft = density(pred_batch, from=0, to=1) 
  max_plot <- max(dist_aft$y)
  if(length(pred_before) > 0){
    dist_bef = density(pred_before, from=0, to=1)
    max_plot <- max(max(dist_aft$y), max(dist_bef$y))
  }
  chart=amPlot(dist_aft$x, round(dist_aft$y, 2), type="l", title="Prédictions du dernier batch",
               xlab="Score", ylab="Densité", zoom=T, legend=T, ylim=c(0,max_plot*1.1))
  amLines(chart,y=round(dist_bef$y, 2), type="l", col="black", title="Précédentes prédictions") 

})

