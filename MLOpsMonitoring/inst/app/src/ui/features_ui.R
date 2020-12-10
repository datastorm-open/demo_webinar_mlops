fluidPage(
  fluidRow(box(sliderInput("treshold_kolmo", "Choix du seuil du test de Kolmogorov-Smirnoff:", min=0.005, max=0.2, step=0.005, 
                           value=0.05), collapsible = TRUE, collapsed = TRUE,
               title="Option avancée du test", 
               width = 12)),
           fluidRow(
             infoBoxOutput("kolmo_features", width = 6),
           conditionalPanel(condition = "output.display_density !== undefined && output.display_density",
                            box(width=6,selectInput("feature", "Choisir une feature", choices=NULL, selected = NULL), 
                                height = "90px")    
           )),
  conditionalPanel(condition = "output.display_density !== undefined && output.display_density",
                   fluidRow(
                     box(shinycssloaders::withSpinner(amChartsOutput("density")), title = "Densités"), 
                     box(shinycssloaders::withSpinner(amChartsOutput("qqplot")), title = "QQPlot")
                   ), 
                   fluidRow(
                     box(shinycssloaders::withSpinner(DTOutput("stats")), title = "Quelques statistiques", width = 12)
                   )
  )
)
