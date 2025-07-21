# ---------------------------------------------------------------------------
# Libraries ----

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(writexl)

# ---------------------------------------------------------------------------
# Read raw data from {quadrige.explorer} ----

data_physico_chem <- esteem.overview::data_physico_chem

# ---------------------------------------------------------------------------
# Cleaning variables

data_physchem <- data_physico_chem |>
  mutate(ESTUARY = case_when(
    ZONE_MARINE_QUADRIGE == "085 - Estuaire de la Gironde" ~ "Gironde",
    ZONE_MARINE_QUADRIGE == "070 - Estuaire de la Loire" ~ "Loire",
    ZONE_MARINE_QUADRIGE == "011 - Estuaire de la Seine" ~ "Seine"
  )) |>
  select(-c(THEME, PARAMETRE_GROUPE, ZONE_MARINE_QUADRIGE,
            SOUS_REGION_MARINE_DCSMM, MASSE_EAU_DCE, LIEU_IDENTIFIANT,
            PARAMETRE_LIBELLE_COMPLET, PARAMETRE_CODE,
            GROUPE_TAXON_LIBELLE, TAXON_LIBELLE, NUMERO_INDIVIDU_OBSERVATION,
            PASSAGE_COORDONNEES, PRELEVEMENT_COORDONNEES,
            NIVEAU_QUALITE, QUALITE_DESCRIPTION))

# # PARAMETRE_GROUPE: Biologie/Phytoplancton
#       "Chlorophylle a", "Phéopigments", # > production primaire
#
#       # PARAMETRE_GROUPE: Mesures physiques
#       "Température de l'eau",
#
#       # PARAMETRE_GROUPE: Mesures physiques/Matériel particulaire
#       "Turbidité",
#       "Turbidité FNU",
#       "Matière en suspension",
#
#       # PARAMETRE_GROUPE: Nutriments/Nutriments Inorganiques
#       "Silicate", # RNOHYD (1975-2016) + REPHY (2007-2024)
#       "Ammonium", "Azote nitreux (nitrite)", "Azote nitrique (nitrate)",
#       "Nitrate + nitrite", "Phosphate",
#
#       # PARAMETRE_GROUPE: Physicochimie
#       "Oxygène dissous", "pH", "Salinité"

# ---------------------------------------------------------------------------
# Export cleaned dataset for physico-chemical parameters ----

usethis::use_data(data_physchem, overwrite = TRUE)
write_xlsx(data_physchem, "inst/results/data_physico_chemistry/data_physchem_cleaned.xlsx")
