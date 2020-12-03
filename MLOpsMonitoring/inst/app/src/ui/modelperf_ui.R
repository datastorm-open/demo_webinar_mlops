fluidPage(
  h2("Indicateurs de performance"),
  fluidRow( 
      box(amChartsOutput("auc_global")),
      box(amChartsOutput("acc_global")),
      box(amChartsOutput("kappa")),
      box(amChartsOutput("taux-achat-top-100"))
    ),
  h2("Recherche d'une rupture dans les prédictions")
)