# =====================================================
# Preparation script for GPS limits of estuaries
# Datasets:
#   - GPS_limits_estuary.rda
# Author: FM
# Date: 2026-06-30
# =====================================================

library(tidyverse)

GPS_limits_estuary <- tibble::tibble(
  estuary = c("Gironde", "Loire", "Seine"),
  estuary_limit_lat_min = c(45.0, 47.15, 49.4),
  estuary_limit_lat_max = c(45.7, 47.34, 49.48),
  estuary_limit_lon_min = c(-1.1, -2.3, 0.0),
  estuary_limit_lon_max = c(-0.6, -1.8, 0.5)
)
usethis::use_data(GPS_limits_estuary, overwrite = TRUE)
