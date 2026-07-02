# =====================================================
# Preparation script for GPS limits and basemaps of estuaries
# Datasets:
#   - GPS_limits_estuary.rda
# Base maps of type OpenStreetMaps:
#   - files in ~/inst/extdata/
# Author: FM
# Date: 2026-07-02
# =====================================================


# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse)


# =====================================================
# 01. GPS limits by estuary
# =====================================================

GPS_limits_estuary <- tibble::tibble(
  estuary = c("Gironde", "Loire", "Seine"),
  estuary_limit_lat_min = c(45.0, 47.15, 49.4),
  estuary_limit_lat_max = c(45.7, 47.34, 49.48),
  estuary_limit_lon_min = c(-1.1, -2.3, 0.0),
  estuary_limit_lon_max = c(-0.6, -1.8, 0.5)
)
usethis::use_data(GPS_limits_estuary, overwrite = TRUE)


# =====================================================
# 02. Base OpenStreetMaps by estuary
# =====================================================

# ---- Gironde ----

fct_build_and_save_basemap(
  GPS_limits_estuary = GPS_limits_estuary,
  estuary_name = "Gironde",
  villes_selection = c("Royan", "Pauillac", "Saint-Estèphe", "Blaye")
)

# ---- Loire ----

fct_build_and_save_basemap(
  GPS_limits_estuary = GPS_limits_estuary,
  estuary_name = "Loire",
  villes_selection = c("Saint-Nazaire", "Cordemais")
)

# ---- Seine ----

fct_build_and_save_basemap(
  GPS_limits_estuary = GPS_limits_estuary,
  estuary_name = "Seine",
  villes_selection = c("Le Havre", "Honfleur", "Tancarville")
)
