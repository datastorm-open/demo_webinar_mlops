fluidPage(
  fluidRow(
    infoBoxOutput("kolmo_features", width = 6),
    conditionalPanel(condition = "output.display_density !== undefined && output.display_density",
                     box(width=6,selectInput("feature", "Choisir une feature", choices=c("")), height = "90px")    
    )
  ),
  conditionalPanel(condition = "output.display_density !== undefined && output.display_density",
                   fluidRow(
                     box(shinycssloaders::withSpinner(amChartsOutput("density")), title = "Densités"), 
                     box(shinycssloaders::withSpinner(amChartsOutput("qqplot")), title = "QQPlot")
                   )
  ),
  p("Sélection d'une feature du modèle"),
  p("Ajouter des tests avec une réponses binaire : ok/ko"),
  p("Stats sur les ventes (indicateurs). Distribution des features du modèle."),
  p("QQPlots : features train vs. features actuelles")
)
