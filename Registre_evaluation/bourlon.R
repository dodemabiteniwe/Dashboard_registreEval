titlePanel("Dashboard Cadastre – OTR Togo"),
sidebarLayout(
  sidebarPanel(
    selectInput("commune", "Choisir une commune :", choices = c("Toutes", unique(donnees_cadastre$Commune))),
    sliderInput("impot", "Estimation impôt :", 
                min = 0, max = max(donnees_cadastre$Estimation_impot, na.rm = TRUE), 
                value = c(0, max(donnees_cadastre$Estimation_impot, na.rm = TRUE))),
    downloadButton("telecharger", "Télécharger rapport CSV")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Résumé", verbatimTextOutput("resume")),
      tabPanel("Carte", leafletOutput("carte", height = 500)),
      tabPanel("Données", DTOutput("tableau")),
      tabPanel("Graphiques",
               plotOutput("histogramme"),
               plotOutput("barres_commune"),
               plotOutput("barres_affectation"),
               plotOutput("exoneration"),
               plotOutput("scatterplot"))
    )
  )
)