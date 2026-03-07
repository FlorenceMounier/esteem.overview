# =====================================================
# Preparation script
# Datasets:
#  - data_physchem.rda
#  - GPS_box.rda
#  - id_colors.rda
# Author: FM
# Date: 2026-03-06
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(tidyverse, quietly = TRUE)

# =====================================================
# 01.Read raw data from {quadrige.explorer}
# =====================================================

data_physico_chem <- esteem.overview::data_physico_chem

# =====================================================
# 02. Clean variables
# =====================================================

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
            NIVEAU_QUALITE, QUALITE_DESCRIPTION)) |>
  rename(estuary = ESTUARY)

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

# =====================================================
# 03. Haline zones
# =====================================================

limits_gironde <- halin_table |> filter(estuary == "Gironde")
limits_seine <- halin_table |> filter(estuary == "Seine")
limits_loire <- halin_table |> filter(estuary == "Loire")

data_physchem <- data_physchem |>

  # ----- Filter estuaries GPS delimitations -----
mutate(
  estuary = case_when(
    estuary == "Gironde" &
      latitude > limits_gironde$estuary_limit_lat_min &
      latitude < limits_gironde$estuary_limit_lat_max &
      longitude > limits_gironde$estuary_limit_lon_min &
      longitude < limits_gironde$estuary_limit_lon_max ~ "Gironde",
    estuary == "Loire" &
      latitude > limits_loire$estuary_limit_lat_min &
      latitude < limits_loire$estuary_limit_lat_max &
      longitude > limits_loire$estuary_limit_lon_min &
      longitude < limits_loire$estuary_limit_lon_max ~ "Loire",
    estuary == "Seine" &
      latitude > limits_seine$estuary_limit_lat_min &
      latitude < limits_seine$estuary_limit_lat_max &
      longitude > limits_seine$estuary_limit_lon_min &
      longitude < limits_seine$estuary_limit_lon_max ~ "Seine",
    TRUE ~ NA
  )
)  |>

  # ----- Create haline_zone variable -----
mutate(
  haline_zone = case_when(
    estuary == "Gironde" & latitude >= limits_gironde$halin_limit_lat ~ "polyhalin",
    estuary == "Gironde" & latitude >= limits_gironde$estuary_limit_lat_min ~ "mesohalin",
    estuary == "Loire" & longitude <= limits_loire$halin_limit_lon ~ "polyhalin",
    estuary == "Loire" & longitude <= limits_loire$estuary_limit_lon_max ~ "mesohalin",
    estuary == "Seine" & longitude <= limits_seine$halin_limit_lon ~ "polyhalin",
    estuary == "Seine" & longitude <= limits_seine$estuary_limit_lon_max ~ "mesohalin",
    TRUE ~ NA
  )
)

# =====================================================
# 04. Extract years, months
# =====================================================

data_physchem <- data_physchem |>
  mutate(year = year(DATE),
         month = month(DATE))

# =====================================================
# 05. Round GPS positions
# =====================================================

data_physchem <- data_physchem |>
  mutate(latitude = round(latitude, digits = 2)) |>
  mutate(longitude = round(longitude, digits = 2))

# =====================================================
# 06. Save dataset for physico-chemical parameters
# =====================================================

usethis::use_data(data_physchem, overwrite = TRUE)

# =====================================================
# 07. Define the study areas for physical and chemical parameters
# =====================================================

GPS_box <- tribble(
  ~ estuary, ~ haline_zone, ~ min_lon, ~ max_lon, ~ min_lat, ~ max_lat,
  "Gironde", "mesohalin", -0.78, -0.65, 45.15, 45.3,
  "Loire", "mesohalin", -2.0, -1.88, 47.27, 47.30,
  "Seine", "polyhalin", 0.0, 0.2, 49.42, 49.49
)

usethis::use_data(GPS_box, overwrite = TRUE)

# =====================================================
# 07. Define the colors of the measurement points
# =====================================================

id_colors <- c("orange", "red", "purple","yellow", "pink","brown", "deeppink" )

usethis::use_data(id_colors, overwrite = TRUE)
