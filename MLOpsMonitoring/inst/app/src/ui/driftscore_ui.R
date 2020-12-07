fluidPage(
  fluidRow( 
    box(shinycssloaders::withSpinner(amChartsOutput("drift_auc"))),
    box(shinycssloaders::withSpinner(amChartsOutput("drift_matth")))
  )
)