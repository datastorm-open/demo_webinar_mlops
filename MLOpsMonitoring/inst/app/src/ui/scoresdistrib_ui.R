fluidPage(
  fluidRow(
    box(shinycssloaders::withSpinner(amChartsOutput("density_scores")), 
        title = "Comparaison des densités des scores"), 
    box(title="Test statistique", 
        fluidRow(
          infoBoxOutput("kolmo_scores"),
          amChartsOutput("qqplot_scores")
        ))
  )
)
