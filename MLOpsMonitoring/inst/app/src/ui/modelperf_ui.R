fluidPage(
  fluidRow( 
      box(amChartsOutput("auc_global")),
      box(amChartsOutput("acc_global")),
      box(amChartsOutput("kappa")),
      box(amChartsOutput("taux-achat-top-100"))
    )
)