library(dplyr)
library(labelled)
glimpse(Registr_Fonc)
look_for(Registr_Fonc)
Registr_Fonc<- Registr_Fonc %>% 
  mutate(AdressePlusCode = if_else(AdressePlusCode == "Données manquantes", NA_character_, AdressePlusCode))
summarise(Registr_Fonc)
summary(Registr_Fonc)
look_for(df_clean)
