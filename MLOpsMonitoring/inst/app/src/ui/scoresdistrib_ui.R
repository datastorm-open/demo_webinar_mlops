fluidPage(
  fluidRow(
    box(shinycssloaders::withSpinner(amChartsOutput("density_scores")), 
        title = "Comparaison des densités des scores"), 
    box(title="Comparaison statistique", 
        fluidRow(
          infoBoxOutput("rmse_diag", width = 12),
          fluidRow(amChartsOutput("qqplot_scores", height = "300px", width="400px"))
        ))
  )
)
