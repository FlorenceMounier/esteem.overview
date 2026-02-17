# =====================================================
# Dataset: base map files in inst/extdata
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(tidyverse)

# =====================================================
# 01. Halin limits by estuary
# =====================================================

halin_table <- tibble::tibble(
  estuary = c("Gironde", "Loire", "Seine"),
  halin_limit_lat = c(45.4, NA, NA),
  halin_limit_lon = c(NA, -2.0, 0.3)
)

usethis::use_data(halin_table, overwrite = TRUE)


# =====================================================
# 02. Base OpenStreetMaps by estuary
# =====================================================

fct_build_and_save_basemap(
  data = data_POMET |> filter(estuary == "Gironde"),
  estuary_name = "Gironde",
  villes_selection = c("Royan", "Pauillac", "Saint-Estèphe", "Blaye", "Saint-Christoly-Médoc")
)

fct_build_and_save_basemap(
  data = data_POMET |> filter(estuary == "Loire"),
  estuary_name = "Loire",
  villes_selection = c("Saint-Nazaire", "Cordemais")
)

fct_build_and_save_basemap(
  data = data_POMET |> filter(estuary == "Seine"),
  estuary_name = "Seine",
  villes_selection = c("Le Havre", "Honfleur", "Tancarville")
)

