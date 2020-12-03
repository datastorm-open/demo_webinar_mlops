sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Suivi des ventes", tabName = "customers", icon = icon("cart-arrow-down")),
    menuItem("Performance du modèle", tabName = "modelperf", icon = icon("chart-line"))
  )
)

header <- dashboardHeader(title = "Webinaire MLOps")

body <- dashboardBody(
  
  tags$link(rel="stylesheet", type="text/css", href="css/style.css"),
  tags$script(src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"),
  
  img(src="img/logoDS.png", class="ribbon", style="margin-right: 1.3cm;cm;margin-top: 0.2cm;margin-bottom: 0.1cm;height: 38px"),
  
  div(style="position:relative; top:-20px;background-color:#fff;border-bottom: 3px solid #e0e0e0;padding:20px;", 
      sliderInput("t",
                  "Nous sommes le :",
                  min = as.Date("2011-01-01","%Y-%m-%d"),
                  max = as.Date("2012-02-01","%Y-%m-%d"),
                  value=as.Date("2011-03-01"),
                  timeFormat="%Y-%m-%d", width="100%")
      ),
  
  div(style="padding-left:15px;padding-right:15px;overflow-y: scroll;",
    tabItems(
      tabItem(tabName = "customers", p("Stats sur les ventes (indicateurs). Distribution des features du modèle.")),
      tabItem(tabName = "modelperf", source("src/ui/modelperf_ui.R", local = T)$value)
      # tabItem(tabName = "modelperf", )
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