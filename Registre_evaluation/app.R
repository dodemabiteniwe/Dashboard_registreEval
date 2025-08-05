library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(gridExtra)
library(reactablefmtr)
library(reactable)
library(scales)
library(openxlsx2)

if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# Charger les données nettoyées
donnees_cadastre <- readRDS("../data/mad_cad_cleaned.rds")

# Fonctions de visualisation externes
graph_boxplot <- function(df, borne_inf, borne_sup) {
  df_filtre <- df %>% filter(Estimation_impot >= borne_inf, Estimation_impot <= borne_sup)
  ordre <- df_filtre %>% group_by(Nature) %>% summarise(med = median(Estimation_impot)) %>% arrange(med) %>% pull(Nature)
  df_filtre <- df_filtre %>% mutate(Nature = factor(Nature, levels = ordre))
  counts <- df_filtre %>% group_by(Nature) %>% summarise(n = n(), y_pos = max(Estimation_impot)*1.05)
  ggplot(df_filtre, aes(x = Nature, y = Estimation_impot, fill = Nature)) +
    geom_boxplot(outlier.shape = NA) +
    geom_text(data = counts, aes(x = Nature, y = y_pos, label = paste0("n = ", n)), inherit.aes = FALSE, size = 3) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() + labs(title = "Boxplot sans valeurs extrêmes") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}

graph_density <- function(df, borne_inf, borne_sup) {
  df_density <- df %>%
    mutate(Type_Bien = ifelse(Nature == "Terrain nu", "Terrain nu", "Terrain construit")) %>%
    filter(Estimation_impot >= borne_inf, Estimation_impot <= borne_sup)
  ggplot(df_density, aes(x = Estimation_impot, fill = Type_Bien)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("Terrain nu" = "#66c2a5", "Terrain construit" = "#fc8d62")) +
    theme_minimal() + labs(title = "Densité - Terrain nu vs Terrain construit")
}

graph_jitter_extremes <- function(df, borne_inf, borne_sup) {
  df_ext <- df %>% filter(Estimation_impot < borne_inf | Estimation_impot > borne_sup)
  ggplot(df_ext, aes(x = Nature, y = Estimation_impot, color = Nature)) +
    geom_jitter(width = 0.2, alpha = 0.7) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() + labs(title = "Valeurs extrêmes par nature") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}

graph_bar_cumul <- function(df, borne_inf, borne_sup) {
  df_labeled <- df %>%
    mutate(Groupe = ifelse(Estimation_impot > borne_sup | Estimation_impot < borne_inf, "Valeurs extrêmes", "Valeurs normales"))
  parts <- df_labeled %>%
    group_by(Groupe) %>% summarise(Total_impot = sum(Estimation_impot), Nb_parcelles = n(), .groups = "drop") %>%
    mutate(Pourcentage = 100 * Total_impot / sum(Total_impot), Etiquette = paste0(round(Pourcentage, 1), "%\n(n = ", Nb_parcelles, ")"))
  ggplot(parts, aes(x = Groupe, y = Pourcentage, fill = Groupe)) +
    geom_bar(stat = "identity", width = 0.6) +
    geom_text(aes(label = Etiquette), vjust = -0.1) +
    scale_fill_manual(values = c("Valeurs extrêmes" = "#E41A1C", "Valeurs normales" = "#377EB8")) +
    ylim(0, 100) + theme_minimal() + labs(title = "Part de l'impôt total") +
    theme(legend.position = "none")
}

# UI
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Dashboard Cadastre – OTR Togo</a>'), id="nav",
             windowTitle = "Dashboard Cadastre – OTR Togo",
             tabPanel("Carte",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("carte", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 350, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        h3(textOutput("parcel"), align = "right"),
                                        h4(textOutput("superficieT"), align = "right"),
                                        h4(textOutput("valeurcadastral"), align = "right"),
                                        h4(textOutput("impot"), align = "right"),
                                        h4(textOutput("exoprop"), align = "right"),
                                        selectInput("commune", label= h5("Choisir une commune :"), choices = c("Toutes", unique(donnees_cadastre$Commune))),
                                        sliderInput("impot", label = h5("Estimation impôt :"), 
                                                    min = 0, max = max(donnees_cadastre$Estimation_impot, na.rm = TRUE), 
                                                    value = c(0, max(donnees_cadastre$Estimation_impot, na.rm = TRUE))))
                                        
                          )
             ),
             tabPanel("Données",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("commune", "Choisir une commune :", choices = c("Toutes", unique(donnees_cadastre$Commune))),
                          sliderInput("impot", "Estimation impôt :", 
                                      min = 0, max = max(donnees_cadastre$Estimation_impot, na.rm = TRUE), 
                                      value = c(0, max(donnees_cadastre$Estimation_impot, na.rm = TRUE))),
                          downloadButton("telecharger", "Télécharger rapport XLSX")
                        ),
                        mainPanel(DTOutput("tableau")))
                      ),
             tabPanel("Graphiques",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("commune", "Choisir une commune :", choices = c("Toutes", unique(donnees_cadastre$Commune))),
                          sliderInput("impot", "Estimation impôt :", 
                                      min = 0, max = max(donnees_cadastre$Estimation_impot, na.rm = TRUE), 
                                      value = c(0, max(donnees_cadastre$Estimation_impot, na.rm = TRUE))),
                          plotOutput("plot_donut_pref")
                        ),
                        mainPanel(
                          tabsetPanel(
                          tabPanel("histogramme",
                                    plotlyOutput("plot_boxplot"),
                                     plotlyOutput("plot_extremes")
                                   ),
                          tabPanel("barres_commune",
                                   plotlyOutput("barres_commune"),
                                   h4("Résumé par quartier (localité)"),
                                   span(tags$i(h6("La valeur cadastrale (V.C) est en millions de FCFA et l'impôt estimé (Impôt E.) en milliers de FCFA")), style="color:#045a8d"),
                                   reactableOutput("table_quartiers")),
                          tabPanel("barres_affectation", plotlyOutput("barres_affectation")),
                          tabPanel("scatterplot",
                                    plotlyOutput("plot_density"),
                                    plotlyOutput("plot_cumul")
                                   )
                        )))
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
  
  output$parcel <- renderText({
    paste0(prettyNum(nrow(donnees_filtrees()), big.mark=","), " parcelles")
  })
  
  output$superficieT <- renderText({
    paste0(prettyNum(sum(donnees_filtrees()$Surperficiem2, na.rm = TRUE), big.mark=","), " m2 de superficie totale")
  })
  
  output$valeurcadastral <- renderText({
    paste0(prettyNum(sum(donnees_filtrees()$Valeur_Cadastrale, na.rm = TRUE), big.mark=","), " FCFA de valeur cadastrale")
  })
  
  output$impot <- renderText({
    paste0(prettyNum(sum(donnees_filtrees()$Estimation_impot, na.rm = TRUE), big.mark=","), " FCFA d'impôt estimé")
  })
  
  output$exoprop <- renderText({
    df <- donnees_filtrees()
    valeur_totale <- sum(df$Valeur_Cadastrale, na.rm = TRUE)
    valeur_exoneree <- sum(df$Valeur_Cadastrale[df$Estimation_impot == 0], na.rm = TRUE)
    prop_exoneree <- ifelse(valeur_totale > 0, round(valeur_exoneree / valeur_totale * 100, 2), 0)
    paste0(prop_exoneree, "% d'exonnération du total de la valeur cadastrale")
  })
  
  output$carte <- renderLeaflet({
    df <- donnees_filtrees()
    leaflet(df) %>%
      addTiles() %>%
      setView(lng = 1.218, lat = 6.138, zoom = 12)%>%
      #addProviderTiles(providers$Esri.WorldImagery, group = "Satellite (Esri)") %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste0("<b>Cadastre:</b> ", Num_Cad, "<br>",
                        "<b>Impôt:</b> ", ifelse(Estimation_impot == 0, "Exonération", paste0(Estimation_impot, " FCFA")), "<br>",
                        "<b>Valeur cadastrale:</b> ", Valeur_Cadastrale, "<br>",
                        "<b>NIF:</b> ", Nif, "<br>",
                        "<b>PlusCode:</b> ", AdressePlusCode),
        clusterOptions = markerClusterOptions()
      )
  })
  
  output$tableau <- renderDT({
    datatable(donnees_filtrees(), options = list(pageLength = 10), filter = "top")
  })
  
  output$telecharger <- downloadHandler(
    filename = function() {
      paste0("cadastre_filtré_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(donnees_filtrees(), file, row_names = FALSE)
    }
  )
  
  output$plot_boxplot <- renderPlotly({
    df <- donnees_filtrees()
    Q1 <- quantile(df$Estimation_impot, 0.25)
    Q3 <- quantile(df$Estimation_impot, 0.75)
    IQR <- Q3 - Q1
    borne_inf <- Q1 - 1.5 * IQR
    borne_sup <- Q3 + 1.5 * IQR
    ggplotly(graph_boxplot(df, borne_inf, borne_sup))
  })
  
  output$plot_density <- renderPlotly({
    df <- donnees_filtrees()
    Q1 <- quantile(df$Estimation_impot, 0.25)
    Q3 <- quantile(df$Estimation_impot, 0.75)
    IQR <- Q3 - Q1
    borne_inf <- Q1 - 1.5 * IQR
    borne_sup <- Q3 + 1.5 * IQR
    ggplotly(graph_density(df, borne_inf, borne_sup))
  })
  
  output$plot_extremes <- renderPlotly({
    df <- donnees_filtrees()
    Q1 <- quantile(df$Estimation_impot, 0.25)
    Q3 <- quantile(df$Estimation_impot, 0.75)
    IQR <- Q3 - Q1
    borne_inf <- Q1 - 1.5 * IQR
    borne_sup <- Q3 + 1.5 * IQR
    ggplotly(graph_jitter_extremes(df, borne_inf, borne_sup))
  })
  
  output$plot_cumul <- renderPlotly({
    df <- donnees_filtrees()
    Q1 <- quantile(df$Estimation_impot, 0.25)
    Q3 <- quantile(df$Estimation_impot, 0.75)
    IQR <- Q3 - Q1
    borne_inf <- Q1 - 1.5 * IQR
    borne_sup <- Q3 + 1.5 * IQR
    ggplotly(graph_bar_cumul(df, borne_inf, borne_sup))
  })
  
  output$barres_commune <- renderPlotly({
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
  
  output$barres_affectation <- renderPlotly({
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
  
  output$table_quartiers <- renderReactable({
    df <- donnees_filtrees()
    
    tableau <- df %>%
      group_by(Localite, Nature) %>%
      summarise(
        Valeur_Cadastrale = sum(Valeur_Cadastrale, na.rm = TRUE),
        Estimation_Impot = sum(Estimation_impot, na.rm = TRUE),
        Nb_Immeubles = n(),
        .groups = "drop"
      ) %>%
      group_by(Localite) %>%
      mutate(
        Total_Impot = sum(Estimation_Impot),
        Total_Valeur = sum(Valeur_Cadastrale)
      ) %>%
      ungroup() %>%
      mutate(
        Total_Impot_k = round(Total_Impot / 1000),
        Total_Valeur_M = round(Total_Valeur / 1e6),
        Pourcent_Impot = round(100 * Total_Impot / Total_Valeur, 2)
      ) %>%
      select(Localite, Nature, Nb_Immeubles, Total_Impot_k, Total_Valeur_M) %>%
      tidyr::pivot_wider(names_from = Nature, values_from = Nb_Immeubles, values_fill = 0)
    
    natures_cols <- setdiff(colnames(tableau), c("Localite", "Total_Impot_k", "Total_Valeur_M"))
    pal_scale <- c("#F4FFFD", "#E9DAEC","#A295E9", "#A270E5", "#43009A")
    
    reactable(
      tableau,
      theme = reactableTheme(
        style = list(fontFamily = "sans-serif"),
        borderColor = "#DADADA"
      ),
      defaultColDef = colDef(
        vAlign = "center",
        align = "center",
        headerVAlign = "center",
        style = color_scales(tableau, span = 4:8, colors = pal_scale),
        headerStyle = list(fontFamily = "sans-serif")
      ),
      defaultSorted = list(Total_Impot_k = "desc"),
      columnGroups = list(
        colGroup(name = "", columns = c("Localite", "Total_Impot_k", "Total_Valeur_M")),
        colGroup(name = "Nature des immeubles", columns = natures_cols)
      ),
      columns = list(
        Localite = colDef(name = "Quartier", minWidth = 150),
        Total_Valeur_M = colDef(name = "V.C", align = "right",
                                cell = data_bars(tableau, fill_color = "#4575b4", text_position = "outside-end")),
        Total_Impot_k = colDef(name = "Impôt E.", align = "right",
                               cell = data_bars(tableau, fill_color = "#d73027", text_position = "outside-end"))
      ),
      compact = TRUE,
      searchable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      wrap = FALSE
    )
  })
  
  output$plot_donut_pref <- renderPlot({
    df <- donnees_filtrees() %>% 
      group_by(Prefecture) %>%
      summarise(Nb = n(), .groups = "drop") %>%
      mutate(Pourcent = round(Nb / sum(Nb) * 100, 1),
             label = paste0(Prefecture, ": ", Pourcent, "%"),
             ymax = cumsum(Pourcent),
             ymin = c(0, head(ymax, n=-1)),
             labelPosition =  (ymax + ymin) / 2)
    
    ggplot(df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = Prefecture)) +
      geom_rect()+
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void()+ theme(legend.position = "none") +
      labs(title = "Répartition des parcelles par préfecture")
  })
  
}

shinyApp(ui = ui, server = server)
