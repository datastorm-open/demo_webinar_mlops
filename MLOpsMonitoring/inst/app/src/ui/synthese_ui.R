fluidPage(
  fluidRow(
    infoBox(title = "Modèle", subtitle = "Période création agrégats d'entrainement", value = "DEC"),
    infoBox(title = "Modèle", subtitle = "Période cible", value = "DEC"),
    infoBox(title = "Modèle", subtitle = "Nb variables ", value = "22"),
    box(title = "Données brutes", DTOutput("dt_data_raw"), collapsible = TRUE, 
        width = 12)
    # box(title = "Données d'entrainement", DTOutput("dt_data_agreg"), collapsible = TRUE, 
    #     width = 12)
  )
)