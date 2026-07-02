# =====================================================
# Preparation script
# Datasets:
#  - data_abiotic.rda
# Plots:
# Author: FM
# Date: 2026-07-02
# =====================================================


# =====================================================
# 00. Packages and data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
`%!in%` = Negate(`%in%`)




# =====================================================
# 01. Join with physico-chemical, river flows & surface area
# =====================================================

# ---- Variables combinations ----

variables_combinations <- data_physico_chem_summarised |>
  distinct(
    estuary, year, month, season,
    year_month, year_season, haline_zone
  )


# ---- Flow data completion ----

data_flow_completed <- variables_combinations |>
  left_join(
    data_flow |>
      group_by(estuary, year, month, year_month, season, year_season) |>
      summarise(
        RESULTAT = mean(RESULTAT, na.rm = TRUE),
        PROGRAMME = paste(sort(unique(na.omit(PROGRAMME))), collapse = " + "),
        PARAMETRE_LIBELLE = "Flow",
        .groups = "drop"
      ),
    by = join_by(estuary, year, month, year_month, season, year_season)
  )


# ---- Intertidal surface area completion ----

data_surface_area_completed <- variables_combinations |>
  left_join(
    data_intertidal_surface_interp |>
      select(estuary, year, haline_zone, PROGRAMME, PARAMETRE_LIBELLE, RESULTAT),
    by = join_by(estuary, year, haline_zone)
  ) |>
  arrange(estuary, year) |>
  group_by(estuary, haline_zone) |>
  tidyr::fill(PROGRAMME, PARAMETRE_LIBELLE, RESULTAT, .direction = "down") |>
  ungroup()


# ---- Join all datasets ----

data_abiotic <- bind_rows(
  data_physico_chem_summarised,
  data_flow_completed,
  data_surface_area_completed
)

data_abiotic |>
  count(PARAMETRE_LIBELLE)

usethis::use_data(data_abiotic, overwrite = TRUE)
