# =====================================================
# Datasets:
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

data_POMET_densities <- fct_formatting_raw_data_POMET(raw_data_POMET_densities) |>
  drop_na(estuary)
usethis::use_data(data_POMET_densities, overwrite = TRUE)

data_POMET_indiv <- fct_formatting_raw_data_POMET(raw_data_POMET_indiv)
usethis::use_data(data_POMET_indiv, overwrite = TRUE)
