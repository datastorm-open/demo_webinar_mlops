fluidPage(
  fluidRow(
    infoBox(title = "Modèle", subtitle = "Période cible entrainement", value = "Juillet 2010", icon=icon("cogs")),
    infoBox(title = "Modèle", subtitle = "Période création agrégats d'entrainement", value = "Janvier - Juin 2010",
            icon=icon("cogs")),
    infoBox(title = "Modèle", subtitle = "Nb variables ", value = "22", icon=icon("cogs")),
    box(title = "Données brutes", DTOutput("dt_data_raw"), collapsible = TRUE, 
        width = 12)
    # box(title = "Données d'entrainement", DTOutput("dt_data_agreg"), collapsible = TRUE, 
    #     width = 12)
  )
)