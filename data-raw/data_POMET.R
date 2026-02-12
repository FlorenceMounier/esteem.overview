# ---------------------------------------------------------------------------
# Libraries ----

library(esteem.overview)
library(tidyverse, quietly = TRUE)

`%!in%` = Negate(`%in%`)

# ---------------------------------------------------------------------------
# Read raw data from {esteem.overview} ----

data_POMET_biota <- esteem.overview::data_POMET_biota_ind
data_POMET_traits <- esteem.overview::data_POMET_traits

# ---------------------------------------------------------------------------
# Variables and join

# names(data_POMET_biota)
# names(data_POMET_traits)
#
# intersect(names(data_POMET_biota), names(data_POMET_traits))

data_POMET <- left_join(data_POMET_biota, data_POMET_traits,
          by = c("trait_id", "masse_eau", "annee", "distance_chalutee",
                 "saison", "engin", "long_trait", "materiel_code"))

# ---------------------------------------------------------------------------
# Create center position of traits

data_POMET <- data_POMET |>
  mutate(pos_lat_dd = (pos_deb_lat_dd + pos_fin_lat_dd) / 2) |>
  mutate(pos_long_dd = (pos_deb_long_dd + pos_fin_long_dd) / 2)

# ---------------------------------------------------------------------------
# Estuary GPS delimitations and halin zones + cleaning

data_POMET <- data_POMET |>
  mutate(estuary = case_when(
    masse_eau |> str_starts("Gironde") ~ "Gironde",
    masse_eau |> str_starts("Loire") ~ "Loire",
    masse_eau |> str_starts("Seine") ~ "Seine",
    TRUE ~ NA
  )) |>
  mutate(
    estuary = case_when(
      estuary == "Gironde" & pos_lat_dd > 45.0 & pos_lat_dd < 45.7 & pos_long_dd < -0.6 & pos_long_dd > -1.1 ~ "Gironde",
      estuary == "Loire" & pos_lat_dd > 47.22 & pos_lat_dd < 47.34 & pos_long_dd < -1.8 & pos_long_dd > -2.3 ~ "Loire",
      estuary == "Seine" &
        pos_lat_dd > 49.4 & pos_long_dd < 0.5 ~ "Seine",
      TRUE ~ NA
    )
  )  |>
  mutate(haline_zone = case_when(
    estuary == "Gironde" & pos_lat_dd >= 45.4 ~ "polyhalin",
    estuary == "Gironde" & pos_lat_dd >= 45.0 ~ "mesohalin",
    estuary == "Loire" & pos_long_dd <= -2.0 ~ "polyhalin",
    estuary == "Loire" & pos_long_dd <= -1.8 ~ "mesohalin",
    estuary == "Seine" & pos_long_dd <= 0.3 ~ "polyhalin",
    estuary == "Seine" & pos_long_dd <= 0.5 ~ "mesohalin",
    TRUE ~ NA
  )) |>
  filter(trait_id != 82) |> # fleuve Gironde
  filter(trait_id %!in% c(6185, 8494)) |>  # out of Loire
  filter(trait_id %!in% c(13477, 16087, 14362)) # filandres Seine

#------------------------------------------------------------------------------
# Save data data_POMET.rda

usethis::use_data(data_POMET, overwrite = TRUE)
