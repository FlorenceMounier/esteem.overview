# =====================================================
# Preparation script
# Datasets:
#  -
# Author: FM
# Date: 2026-06-12
# =====================================================

#------------------------------------------------------------------------------
# Packages

library(tidyverse, quietly = TRUE)

# Import raw sextant output files
EDF_cordemais_loire <- read_csv2("../BDD_Poisson_Ab_All_2013_AnneeN_AvecCrust.csv")


# =====================================================
# 01. data_Cordemais_traits
# =====================================================

data_Cordemais_traits <- EDF_cordemais_loire |>

  # ---- Define estuary ----
  mutate(estuary = "Loire") |>

  # ---- Create GPS central position of traits ----
  mutate(latitude = round( (Latitude_WGS84_Debut_SEANEO + Latitude_WGS84_Fin_SEANEO) / 2, digits = 2 )) |>
  mutate(longitude = round( (Longitude_WGS84_Debut_SEANEO + Longitude_WGS84_Fin_SEANEO) / 2, digits = 2 )) |>

 # ---- Filter upstream (C) and get only one GPS positition ----
  filter(Station_Glob == "C") |>
  mutate(longitude = round( mean(longitude, na.rm = TRUE), digits = 2)) |>
  mutate(latitude = round( mean(latitude, na.rm = TRUE), digits = 2))

# ----- Define estuary and haline_zone from GPS delimitations -----
data_Cordemais_traits <- get_haline_zone_from_gps_position(data = data_Cordemais_traits,
                                                    latitude = latitude,
                                                    longitude = longitude)

# ----- Define information on year, month, season from date -----
data_Cordemais_traits <- get_info_from_dates(data = data_Cordemais_traits, date_variable = Date)


  # ----
  group_by(latitude, longitude, Date) |>
  summarise(
    Temperature = mean(Temp_eau_SEANEO, na.rm = TRUE),
    Salinity = mean(Salinite_SEANEO, na.rm = TRUE),
    `%O2` = mean(O2_dissous_pct_SEANEO, na.rm = TRUE),
    .groups = "drop"
  )

data_Cordemais_traits |>  distinct(Date) |>  arrange(Date)


# ---- Save data_Cordemais_traits.rda ----
usethis::use_data(data_Cordemais_traits, overwrite = TRUE)







group_by(latitude, longitude, Date, Espece, Temp_eau_SEANEO, Salinite_SEANEO, O2_dissous_pct_SEANEO) |>
  summarise(density = sum(Ab, na.rm = TRUE), .groups = "drop")
