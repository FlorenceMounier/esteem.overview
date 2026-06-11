# =====================================================
# Import, join and formatting of POMET raw data
# Datasets:
#  - data_POMET_ALL_densities.rda
#  - data_POMET_densities.rda
#  - data_POMET_indiv.rda
# Preparation script
# Author: FM
# Date: 2026-02-20
# =====================================================

# =====================================================
# 00. Packages, functions and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

`%!in%` = Negate(`%in%`)

data(raw_data_POMET_densities)
data(raw_data_POMET_indiv)
data(raw_data_POMET_traits)

# =====================================================
# 01. data_POMET_traits
# =====================================================

data_POMET_traits <- raw_data_POMET_traits |>

  # ---- Create GPS central position of traits ----
mutate(latitude = (pos_deb_lat_dd + pos_fin_lat_dd) / 2) |>
mutate(longitude = (pos_deb_long_dd + pos_fin_long_dd) / 2) |>

  # ----- Filter unrelevant observations -----
filter(trait_id != 82) |> # fleuve Gironde
  filter(trait_id %!in% c(6185, 8494)) |>  # out of Loire
  filter(trait_id %!in% c(13477, 16087, 14362)) # filandres Seine

# ----- Define estuary and haline_zone from GPS delimitations -----
data_POMET_traits <- get_info_from_gps_position(data = data_POMET_traits,
                                                latitude = latitude,
                                                longitude = longitude)

# ---- Save data_POMET_traits.rda ----
usethis::use_data(data_POMET_traits, overwrite = TRUE)

# =====================================================
# 02. data_POMET_physico_chem
# =====================================================

data_POMET_physico_chem <- data_POMET_traits |>
  select(c(madate, estuary, latitude, longitude, haline_zone, oxygene, salinite, temperature))

# ----- Define information on year, month, season from date -----
data_POMET_physico_chem <- get_info_from_dates(data = data_POMET_physico_chem, date_variable = madate)

# ---- Save data_POMET_physico_chem.rda ----
usethis::use_data(data_POMET_physico_chem, overwrite = TRUE)


# =====================================================
# 03. Save data_POMET_*.rda related to biota
# =====================================================

# ---- Densities for all species ----

data_POMET_ALL_densities <- fct_formatting_raw_data_POMET(
  data_POMET = raw_data_POMET_densities,
  species = NULL) |>
  drop_na(estuary)

## Correction bug nt
# data_POMET_ALL_densities |>
#   group_by(trait_id, name) |>
#   summarise(n = n()) |>
#   filter (n != 1)

data_POMET_ALL_densities <- data_POMET_ALL_densities |>
  group_by(trait_id, annee, saison, name, oxygene, salinite,
           temperature, latitude, longitude, estuary, haline_zone) |>
  summarise(nt = sum(nt, na.rm = TRUE), Densite = sum(Densite, na.rm = TRUE), .groups = "drop") |>
  drop_na(nt, Densite)

usethis::use_data(data_POMET_ALL_densities, overwrite = TRUE)

# ---- Densities for studied species ----

data_POMET_densities <- fct_formatting_raw_data_POMET(raw_data_POMET_densities) |>
  drop_na(estuary)

## Correction bug nt
data_POMET_densities <- data_POMET_densities |>
  group_by(trait_id, annee, saison, name, oxygene, salinite,
           temperature, latitude, longitude, estuary, haline_zone) |>
  summarise(nt = sum(nt, na.rm = TRUE), Densite = sum(Densite, na.rm = TRUE), .groups = "drop") |>
  drop_na(nt, Densite)

usethis::use_data(data_POMET_densities, overwrite = TRUE)


# ---- Individual biometrics for studied species ----

data_POMET_indiv <- fct_formatting_raw_data_POMET(raw_data_POMET_indiv)
usethis::use_data(data_POMET_indiv, overwrite = TRUE)
