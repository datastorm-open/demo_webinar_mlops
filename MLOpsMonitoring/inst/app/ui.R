sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Suivi des ventes", tabName = "customers", icon = icon("cart-arrow-down"))
  )
)

header <- dashboardHeader(title = "Webinaire MLOps")

body <- dashboardBody(
  
  tags$link(rel="stylesheet", type="text/css", href="css/style.css"),
  tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"),
  
  img(src="img/logoDS.png", class="ribbon", style="margin-right: 1.3cm;cm;margin-top: 0.2cm;margin-bottom: 0.1cm;height: 38px"),
  
  div(style="position:relative; top:-20px;background-color:#fff;border-bottom: 3px solid #e0e0e0;padding:20px;", 
      sliderInput("time",
              "",
              min = as.Date("2009-01-01","%Y-%m-%d"),
              max = as.Date("2012-01-01","%Y-%m-%d"),
              value=as.Date("2010-12-01"),
              timeFormat="%Y-%m-%d", width="100%")
      ),
  
  div(style="padding:15px;",
    tabItems(
      tabItem(tabName = "customers", p("bla"))
      #tabItem(tabName = "analysis", source("src/ui/analysis_ui.R", local = T)$value)
    )
  )
)


if(!dev){
  secure_app(
    dashboardPage(
      header,
      sidebar,
      body), 
    tags_top = tags$img(src = "img/logoDS.png", width = 200),
    tags_bottom = tags$div(tags$p("MLops Webinar - 2020/12/15"), align = "center"),
    language = "fr",
    background = "linear-gradient(to top, #d5d4d0 0%, #d5d4d0 1%, #eeeeec 31%, #efeeec 75%, #e9e9e7 100%);",
    enable_admin = TRUE)  
} else {
  dashboardPage(
    header,
    sidebar,
    body)
}