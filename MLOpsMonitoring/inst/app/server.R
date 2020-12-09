shinyServer(function(input, output, session) {
  
  if(!dev){
    res_auth <- secure_server(check_credentials = check_credentials(data.frame(
      user = c("mlops"), # mandatory
      password = c("webinar"), # mandatory
      start = c("2019-04-15"), # optinal (all others)
      expire = c(NA),
      admin = c(TRUE),
      stringsAsFactors = FALSE
    ))) 
  }
  
  # UTILS FUNCTION
  source("src/server/utils.R", local = T)
  
  # DATA
  last_batch_name <- reactive({min(names(features_batch)[names(features_batch)>=input$t])})
  scores_at_t <- reactive({scores[END<=input$t,]})
  
  # ALERTING SYSTEM
  observe({
    checkUp(scores_at_t())
  })
  alerts <- reactiveValues(datadrift=NULL, model_perf=NULL, km_features=NULL)
  output$alerts <- renderMenu({
    ntfs = reactiveValuesToList(alerts)
    print(lapply(ntfs, is.null))
    if(length(ntfs)>0){
      dropdownMenu(type = "notifications", .list = ntfs[!as.logical(lapply(ntfs, is.null))])
    }else{
      NULL
    }
  })
  
  source("src/server/features_server.R", local = T)
  source("src/server/modelperf_server.R", local = T)
  source("src/server/driftscore_server.R", local = T)
})