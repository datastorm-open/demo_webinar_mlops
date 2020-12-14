fluidPage(
  fluidRow(
    box(dataTableOutput("log_rupt_date")), 
    # box(title="Comparaison statistique", 
    #     fluidRow(
    #       column(12, 
    #              infoBoxOutput("rmse_diag", width = 12),
    #              fluidRow(amChartsOutput("qqplot_scores", height = "300px", width="450px"))
    #       )
    #     ), height = 475)
    box(title="Comparaison statistique", 
        column(12,
               infoBoxOutput("rmse_diag", width = 12),
               amChartsOutput("qqplot_scores", height = "300px", width="500px"), 
               align="center"),
        , height = 475),
    box(shinycssloaders::withSpinner(amChartsOutput("density_scores")), 
        title = "Comparaison des densités des scores", height = 475, width=12)
  )
)
