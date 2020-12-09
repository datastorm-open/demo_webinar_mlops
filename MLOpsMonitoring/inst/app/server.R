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
  alerts <- reactiveValues(datadrift=NULL, model_perf=NULL, km_features=NULL)
  observe({
    checkUp(scores_at_t(), threshold)
    if(any(!as.logical(lapply(reactiveValuesToList(alerts), is.null)))){
      output$alerts <- renderMenu({
        ntfs = reactiveValuesToList(alerts)
        dropdownMenu(type = "notifications", .list = ntfs[!as.logical(lapply(ntfs, is.null))])
      })
    }
    runjs(paste0('document.getElementsByClassName("dropdown notifications-menu")[0].style.display=',
                 ifelse(any(!as.logical(lapply(reactiveValuesToList(alerts), is.null))),'"block"','"none"')))
  })

  
  source("src/server/features_server.R", local = T)
  source("src/server/modelperf_server.R", local = T)
  source("src/server/driftscore_server.R", local = T)
  
})