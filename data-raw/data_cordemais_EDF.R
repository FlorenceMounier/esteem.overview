# =====================================================
# Preparation script
# Datasets:
#  -
# Author: FM
# Date: 2026-06-30
# =====================================================

# =====================================================
# 00. Packages and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

# raw_data_EDF_cordemais_loire <- read_csv2("../BDD_Poisson_Ab_All_2013_AnneeN_AvecCrust.csv")
# usethis::use_data(raw_data_EDF_cordemais_loire)
data(raw_data_EDF_cordemais_loire)


# =====================================================
# 01. data_Cordemais_traits
# =====================================================

data_cordemais_traits <- raw_EDF_cordemais_loire |>

  # ---- Define estuary ----
  mutate(estuary = "Loire") |>

  # ---- Create GPS central position of traits ----
  mutate(latitude = round( (Latitude_WGS84_Debut_SEANEO + Latitude_WGS84_Fin_SEANEO) / 2, digits = 2 )) |>
  mutate(longitude = round( (Longitude_WGS84_Debut_SEANEO + Longitude_WGS84_Fin_SEANEO) / 2, digits = 2 )) |>

 # ---- Filter upstream (C) and get only one GPS positition ----
  filter(Station_Glob == "C") |>
  mutate(longitude = round( mean(longitude, na.rm = TRUE), digits = 2)) |>
  mutate(latitude = round( mean(latitude, na.rm = TRUE), digits = 2)) |>

# ----- Define estuary and haline_zone from GPS delimitations -----
get_haline_zone_from_gps_position(latitude = latitude, longitude = longitude) |>

# ----- Define information on year, month, season from date -----
get_info_from_dates(date_variable = Date)


  # # ----
  # group_by(latitude, longitude, Date) |>
  # summarise(
  #   Temperature = mean(Temp_eau_SEANEO, na.rm = TRUE),
  #   Salinity = mean(Salinite_SEANEO, na.rm = TRUE),
  #   `%O2` = mean(O2_dissous_pct_SEANEO, na.rm = TRUE),
  #   .groups = "drop"
  # )



# ---- Save data_Cordemais_traits.rda ----
usethis::use_data(data_cordemais_traits, overwrite = TRUE)


# group_by(latitude, longitude, Date, Espece, Temp_eau_SEANEO, Salinite_SEANEO, O2_dissous_pct_SEANEO) |>
#   summarise(density = sum(Ab, na.rm = TRUE), .groups = "drop")
