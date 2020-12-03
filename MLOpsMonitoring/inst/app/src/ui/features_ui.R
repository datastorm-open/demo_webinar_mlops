fluidPage(
  selectInput("feature", "Choisissez une feature", choices=setdiff(colnames(features_train),c("X", "MONTH", "Customer.ID", "VAR_REP", "YEAR"))), 
  fluidRow(
    box(amChartsOutput("qqplot"), title = "QQPlot")
    ),
  p("Sélection d'une feature du modèle"),
  p("Ajouter des tests avec une réponses binaire : ok/ko"),
  p("Stats sur les ventes (indicateurs). Distribution des features du modèle."),
  p("QQPlots : features train vs. features actuelles")
)
