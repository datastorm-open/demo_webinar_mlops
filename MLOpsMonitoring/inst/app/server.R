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
})