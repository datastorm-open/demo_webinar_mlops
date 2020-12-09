fluidPage(
  fluidRow(
    box(shinycssloaders::withSpinner(amChartsOutput("density_scores")), title = "Densités des scores")
  )
)
