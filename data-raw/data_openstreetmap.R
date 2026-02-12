# ---------------------------------------------------------------------------
# Base OpenStreetMaps by estuary

library(esteem.overview)
library(tidyverse)
library(sf)
library(osmdata)
library(maptiles)

# Restart R before each code line to clean the cache and run the options function
# options(osmdata.overpass_url = "https://overpass-api.de/api/interpreter")

openstreet_base_map_gironde <- fct_openstreet_base_map(data = data_POMET |> filter(estuary == "Gironde"))
openstreet_base_map_loire <- fct_openstreet_base_map(data = data_POMET |> filter(estuary == "Loire"))
openstreet_base_map_seine <- fct_openstreet_base_map(data = data_POMET |> filter(estuary == "Seine"))

#------------------------------------------------------------------------------
# Save data.rda

usethis::use_data(openstreet_base_map_gironde, overwrite = TRUE)
usethis::use_data(openstreet_base_map_loire, overwrite = TRUE)
usethis::use_data(openstreet_base_map_seine, overwrite = TRUE)
