# =====================================================
# Datasets:
#  - data_POMET_ALL_densities.rda
#  - data_POMET_densities.rda
#  - data_POMET_indiv.rda
# Preparation script
# Author: FM
# Date: 2026-02-20
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

`%!in%` = Negate(`%in%`)

# =====================================================
# 01. Import, join and formatting of POMET raw data
# =====================================================

data(raw_data_POMET_densities)
data(raw_data_POMET_indiv)
data(raw_data_POMET_traits)

data_POMET_traits <- raw_data_POMET_traits |>
  select(trait_id, conductivite, duree, maree, oxygene, profondeur,
         salinite, temperature, pos_deb_lat_dd, pos_deb_long_dd, pos_fin_lat_dd, pos_fin_long_dd)

# =====================================================
# 02. Save data_POMET_*.rda
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
