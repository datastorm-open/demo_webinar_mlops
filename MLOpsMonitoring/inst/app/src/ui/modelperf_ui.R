fluidPage(
  p("Suivi des différents scores sur les derniers mois"),
  fluidRow( 
      box(amChartsOutput("auc_global"))
    )
)