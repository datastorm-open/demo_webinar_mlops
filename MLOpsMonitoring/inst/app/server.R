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
  
  est_rupt_date <- reactive({
    #predictions[changepoint::cpt.var(predictions[START<input$t]$PRED)@cpts[1], START]
    predictions[changepoint::cpt.var(predictions[START<input$t]$HAVING_WRONG)@cpts[1], START]
  })

  # ADAPT HEADER TO DATE
  observe({
    if(input$t < as.Date("2011-03-01")){
      runjs('document.getElementsByClassName("js-irs-0")[0].children[2].style.background="#AAAAAA"')
      runjs('document.getElementsByClassName("js-irs-0")[0].children[3].style.background="#AAAAAA"')
    }else{
      runjs('document.getElementsByClassName("js-irs-0")[0].children[2].style.background="#420d09"')
      runjs('document.getElementsByClassName("js-irs-0")[0].children[3].style.background="#420d09"')
      runjs('document.getElementsByClassName("js-irs-0")[0].children[0].children[5].firstChild.data += " (Drift)"')
    }
  })
  
  # ALERTING SYSTEM
  alerts <- reactiveValues(datadrift=NULL, model_perf=NULL, km_features=NULL)
  observe({
    checkUp(scores_at_t(), threshold)
    if(any(!as.logical(lapply(reactiveValuesToList(alerts), is.null)))){
      output$alerts <- renderMenu({
        ntfs = reactiveValuesToList(alerts)
        dropdownMenu(type = "notifications", 
                     icon = icon(ifelse(is.null(alerts$model_perf), "bell","exclamation-triangle")),
                     headerText = "L'APP VOUS A ENVOYE LES MAILS SUIVANTS", 
                     .list = ntfs[!as.logical(lapply(ntfs, is.null))])
      })
    }
    runjs(paste0('document.getElementsByClassName("dropdown notifications-menu")[0].style.display=',
                 ifelse(any(!as.logical(lapply(reactiveValuesToList(alerts), is.null))),'"block"','"none"')))
  })

  
  source("src/server/features_server.R", local = T)
  source("src/server/modelperf_server.R", local = T)
  source("src/server/driftscore_server.R", local = T)
  source("src/server/scoresdistrib_server.R", local = T)

})