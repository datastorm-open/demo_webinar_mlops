fluidPage(
  fluidRow( 
    box(shinycssloaders::withSpinner(amChartsOutput("drift_auc"))),
    box(shinycssloaders::withSpinner(amChartsOutput("drift_matth")))
  ), 
  fluidRow(
    box(shinycssloaders::withSpinner(amChartsOutput("drift_imp", height = "500px")), width = 12)
  )
)