# scripts/prepare_data.R

library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
Registr_Fonc <- read_excel("./data/Registr_FoncierFiscal30_06_2025(01-06_2025)_V5b.xlsx", 
                                                             col_types = c("numeric", "text", "text", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "numeric", "text", "text", "numeric", 
                                                                           "numeric", "numeric", "date", "numeric", 
                                                                           "numeric", "numeric", "text"))

Registr_Fonc<- Registr_Fonc %>% 
  mutate(
    AdressePlusCode = if_else(AdressePlusCode == "Données manquantes", NA_character_, AdressePlusCode),
    Nature = ifelse(Nature == "Hangard,Baraque,conteneur, en construction", "Hangard et assimilés", Nature),
    Prefecture = ifelse(Prefecture == "LOME COMMUNE", "GOLFE", Prefecture))

# Nettoyage et transformation
df_clean <- Registr_Fonc %>%
  rename_with(~str_replace_all(., " ", "_")) %>%
  rename_with(~str_replace_all(., "\\(m²\\)", "m2")) %>%
  mutate(
    Commune = str_trim(Commune),
    Prefecture = str_trim(Prefecture),
    Affectation = str_to_sentence(Affectation),
    Nature = str_to_sentence(Nature),
    Date_evaluation = as.Date(Date_evaluation),
    Estimation_impot = ifelse(is.na(Estimation_impot), 0, Estimation_impot),
    Exoneration = ifelse(Estimation_impot == 0, TRUE, FALSE)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>% # Ne garder que les localisations valides
  filter(!is.na(Nature), !is.na(Estimation_impot),!is.na(Prefecture))
   # Ne garder que les localisations valides

# Créer le dossier 'data' s’il n’existe pas
#if (!dir.exists("data")) dir.create("data")

# Enregistrer en .rds pour un chargement rapide
saveRDS(df_clean, "data/mad_cad_cleaned.rds")
