fluidPage(
  h2("Indicateurs de performance"),
  fluidRow( 
      box(shinycssloaders::withSpinner(amChartsOutput("cheatcode"))),
      box(shinycssloaders::withSpinner(amChartsOutput("logloss"))),
      box(shinycssloaders::withSpinner(amChartsOutput("auc_global"))),
      box(shinycssloaders::withSpinner(amChartsOutput("acc_global"))),
      box(shinycssloaders::withSpinner(amChartsOutput("kappa"))),
      box(shinycssloaders::withSpinner(amChartsOutput("taux-achat-top-100")))
    ),
  h2("Recherche d'une rupture dans les prédictions")
)