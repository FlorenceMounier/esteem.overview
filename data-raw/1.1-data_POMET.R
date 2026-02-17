# =====================================================
# Dataset: data_POMET.rda
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

`%!in%` = Negate(`%in%`)

# =====================================================
# 01. Import and join raw data
# =====================================================

data_POMET_biota <- esteem.overview::data_POMET_biota_ind
data_POMET_traits <- esteem.overview::data_POMET_traits

# names(data_POMET_biota)
# names(data_POMET_traits)
#
# intersect(names(data_POMET_biota), names(data_POMET_traits))

data_POMET <- left_join(data_POMET_biota, data_POMET_traits,
          by = c("trait_id", "masse_eau", "annee", "distance_chalutee",
                 "saison", "engin", "long_trait", "materiel_code"))


# =====================================================
# 02. Create GPS center position of traits
# =====================================================

data_POMET <- data_POMET |>
  mutate(latitude = (pos_deb_lat_dd + pos_fin_lat_dd) / 2) |>
  mutate(longitude = (pos_deb_long_dd + pos_fin_long_dd) / 2)


# =====================================================
# 03. Estuary GPS delimitations and halin zones + cleaning
# =====================================================


data_POMET <- data_POMET |>
  # ----- Create estuary variable -----
  mutate(estuary = case_when(
    masse_eau |> str_starts("Gironde") ~ "Gironde",
    masse_eau |> str_starts("Loire") ~ "Loire",
    masse_eau |> str_starts("Seine") ~ "Seine",
    TRUE ~ NA
  )) |>
  # ----- Filter estuaries GPS delimitations -----
  mutate(
    estuary = case_when(
      estuary == "Gironde" & latitude > 45.0 & latitude < 45.7 & longitude < -0.6 & longitude > -1.1 ~ "Gironde",
      estuary == "Loire" & latitude > 47.22 & latitude < 47.34 & longitude < -1.8 & longitude > -2.3 ~ "Loire",
      estuary == "Seine" &
        latitude > 49.4 & longitude < 0.5 ~ "Seine",
      TRUE ~ NA
    )
  )  |>
  # ----- Create haline_zone variable -----
  mutate(haline_zone = case_when(
    estuary == "Gironde" & latitude >= 45.4 ~ "polyhalin",
    estuary == "Gironde" & latitude >= 45.0 ~ "mesohalin",
    estuary == "Loire" & longitude <= -2.0 ~ "polyhalin",
    estuary == "Loire" & longitude <= -1.8 ~ "mesohalin",
    estuary == "Seine" & longitude <= 0.3 ~ "polyhalin",
    estuary == "Seine" & longitude <= 0.5 ~ "mesohalin",
    TRUE ~ NA
  )) |>
  # ----- Filter unrelevant observations -----
  filter(trait_id != 82) |> # fleuve Gironde
  filter(trait_id %!in% c(6185, 8494)) |>  # out of Loire
  filter(trait_id %!in% c(13477, 16087, 14362)) # filandres Seine

# =====================================================
# 04. Save data data_POMET.rda
# =====================================================

usethis::use_data(data_POMET, overwrite = TRUE)
