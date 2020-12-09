latest_batch <- reactive({
  periods = as.Date(names(features_batch), origin="1970-01-01")
  features_batch[[as.character(max(periods[periods<=input$t]))]]
})

display_density <- reactive({any(!is_similar()[last_batch_name(),])})
output$display_density <- display_density
outputOptions(output, "display_density", suspendWhenHidden = FALSE)

is_similar <- reactive({
  is_similar <- distrib_comparison(features_train, features_batch, features, verbose=F, 
                     threshold = input$treshold_kolmo)
  # browser()
  is_similar
})


output$kolmo_features <- renderInfoBox({
  alert = display_density()
  infoBox(title = ifelse(alert,"Incohérence(s) entre nouvelles données et jeu d'entraînement", "Cohérence entre nouvelles données et jeu d'entraînement"), 
          value = ifelse(alert, paste("Sur", sum(!is_similar()[last_batch_name(),]), "variable(s)"), "Tout va bien"), 
          color = ifelse(alert, 'orange', 'green'), 
          width = 12)
})

observe({
  if(display_density()){
    updateSelectInput(session, "feature", choices=colnames(is_similar())[which(!is_similar()[last_batch_name(),])])    
  } else {
    updateSelectInput(session, "feature", choices=NULL, selected = NULL)
  }
})

output$qqplot <- renderAmCharts({
  if(display_density() && !is.null(input$feature) && input$feature!=""){
    qtl = 1:99*0.01
    dist_bef = quantile(latest_batch()[,c(input$feature)], qtl) 
    dist_aft = quantile(features_train[[input$feature]], qtl)
    lim=c(min(min(dist_bef), min(dist_aft)), max(max(dist_bef), max(dist_aft)))
    chart=amPlot(dist_bef, dist_aft, 
                 xlim=lim, ylim=lim, 
                 xlab="Quantiles du jeu d'entraînement", ylab="Quantiles du dernier batch")
    amLines(chart,y=unname(dist_bef), type="l", col="black") 
  }
})

output$density <- renderAmCharts({
  if(display_density() && !is.null(input$feature) && input$feature!=""){
    mini = min(features_train[[input$feature]])-0.1*abs(min(features_train[[input$feature]]))
    maxi = max(features_train[[input$feature]])+0.1*abs(max(features_train[[input$feature]]))
    dist_aft = density(latest_batch()[,c(input$feature)], from=mini, to=maxi) 
    dist_bef = density(features_train[[input$feature]], from=mini, to=maxi)
    chart=amPlot(dist_aft$x, dist_aft$y, type="l", title="Dernier batch",
                 xlab=input$feature, ylab="Densité", zoom=T, legend=T)
    amLines(chart,y=dist_bef$y, type="l", col="black", title="Données d'entraînement") 
  }
})

output$stats <- renderDataTable({
  if(display_density() && !is.null(input$feature) && input$feature!=""){
    test <- latest_batch()[,c(input$feature)]
    train <- features_train[[input$feature]]
    stats_train <- c(min(train), quantile(train, 0.05), 
                     quantile(train, 0.25), median(train), mean(train), 
                     quantile(train, 0.75), quantile(train, 0.95), max(train))
    stats_test <- c(min(test), quantile(test, 0.05), 
                     quantile(test, 0.25), median(test), mean(test), 
                     quantile(test, 0.75), quantile(test, 0.95), max(test))
    dt_stats <- data.table(TRAIN = round(stats_train, 2), LAST_BATCH = round(stats_test, 2))
    DT::datatable(dt_stats, 
                  options = list(dom = 't', autoWidth = FALSE), rownames = c("min", "Quantile 0.05", "Quantile 0.25", 
                                                          "médiane", "moyenne", 
                                                          "Quantile 0.75", "Quantile 0.95", "max"), 
                  colnames = c("Données d'entrainement", "Dernier batch"), 
                  caption = input$feature, 
                  width = 50)
  }
})
