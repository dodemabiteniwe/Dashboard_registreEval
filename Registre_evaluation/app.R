library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)

# Charger les données nettoyées
donnees_cadastre <- readRDS("../data/mad_cad_cleaned.rds")

# UI
ui <- fluidPage(
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
)

# Serveur
server <- function(input, output) {
  
  donnees_filtrees <- reactive({
    df <- donnees_cadastre
    if (input$commune != "Toutes") {
      df <- df %>% filter(Commune == input$commune)
    }
    df %>% filter(Estimation_impot >= input$impot[1], Estimation_impot <= input$impot[2])
  })
  
  output$resume <- renderPrint({
    df <- donnees_filtrees()
    valeur_totale <- sum(df$Valeur_Cadastrale, na.rm = TRUE)
    valeur_exoneree <- sum(df$Valeur_Cadastrale[df$Estimation_impot == 0], na.rm = TRUE)
    prop_exoneree <- ifelse(valeur_totale > 0, round(valeur_exoneree / valeur_totale * 100, 2), 0)
    
    list(
      Parcelles = nrow(df),
      Surface_totale_m2 = sum(df$Surperficiem2, na.rm = TRUE),
      Valeur_totale = valeur_totale,
      Impot_total = sum(df$Estimation_impot, na.rm = TRUE),
      Valeur_exoneree = valeur_exoneree,
      Pourcentage_exonere = paste0(prop_exoneree, " %")
    )
  })
  
  output$carte <- renderLeaflet({
    df <- donnees_filtrees()
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~latitude, lng = ~longitude,
        label = ~paste0("<b>Cadastre:</b> ", Num_Cad, "<br>",
                        "<b>Impôt:</b> ", ifelse(Estimation_impot == 0, "Exonération", paste0(Estimation_impot, " FCFA")), "<br>",
                        "<b>Valeur cadastrale:</b> ", Valeur_Cadastrale, "<br>",
                        "<b>NIF:</b> ", Nif,
                        "<b>PlusCode:</b> ", AdressePlusCode),
        labelOptions = labelOptions(direction = "auto"),
        popupOptions = popupOptions(closeButton = FALSE),
        radius = 6, color = "navy", fillOpacity = 0.8
      )
  })
  
  output$tableau <- renderDT({
    datatable(donnees_filtrees(), options = list(pageLength = 10), filter = "top")
  })
  
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("cadastre_filtré_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(donnees_filtrees(), file, row.names = FALSE)
    }
  )
  
  output$histogramme <- renderPlot({
    df <- donnees_filtrees()
    ggplot(df, aes(x = Estimation_impot)) +
      geom_histogram(binwidth = 5000, fill = "steelblue", color = "white") +
      labs(title = "Répartition des impôts estimés", x = "Impôt estimé (FCFA)", y = "Nombre de parcelles") +
      theme_minimal()
  })
  
  output$barres_commune <- renderPlot({
    df <- donnees_filtrees()
    df %>%
      group_by(Commune) %>%
      summarise(Impôt_total = sum(Estimation_impot, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Commune, Impôt_total), y = Impôt_total)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(title = "Total impôt par commune", x = "Commune", y = "Total impôt") +
      theme_minimal()
  })
  
  output$barres_affectation <- renderPlot({
    df <- donnees_filtrees()
    df %>%
      group_by(Affectation) %>%
      summarise(Valeur_totale = sum(Valeur_Cadastrale, na.rm = TRUE)) %>%
      ggplot(aes(x = reorder(Affectation, Valeur_totale), y = Valeur_totale)) +
      geom_col(fill = "orange") +
      coord_flip() +
      labs(title = "Valeur cadastrale par affectation", x = "Affectation", y = "Valeur") +
      theme_minimal()
  })
  
  output$exoneration <- renderPlot({
    df <- donnees_filtrees()
    df %>%
      mutate(Statut = ifelse(Estimation_impot == 0, "Exonérée", "Imposée")) %>%
      count(Statut) %>%
      ggplot(aes(x = Statut, y = n, fill = Statut)) +
      geom_bar(stat = "identity") +
      labs(title = "Répartition des parcelles exonérées vs imposées", x = "", y = "Nombre") +
      theme_minimal()
  })
  
  output$scatterplot <- renderPlot({
    df <- donnees_filtrees()
    ggplot(df, aes(x = Surperficiem2., y = Estimation_impot)) +
      geom_point(color = "purple", alpha = 0.7) +
      labs(title = "Impôt estimé en fonction de la superficie", x = "Superficie (m²)", y = "Impôt estimé (FCFA)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
