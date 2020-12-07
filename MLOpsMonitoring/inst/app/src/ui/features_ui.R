fluidPage(
  fluidRow(
    infoBoxOutput("kolmo_features", width = 6),
    conditionalPanel(condition = "output.display_density !== undefined && output.display_density",
                     box(width=6,selectInput("feature", "Choisir une feature", choices=NULL, selected = NULL), height = "90px")    
    )
  ),
  conditionalPanel(condition = "output.display_density !== undefined && output.display_density",
                   fluidRow(
                     box(shinycssloaders::withSpinner(amChartsOutput("density")), title = "Densités"), 
                     box(shinycssloaders::withSpinner(amChartsOutput("qqplot")), title = "QQPlot")
                   )
  )
)
