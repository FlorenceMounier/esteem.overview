---------------------------------------------------------------------------
# Libraries ----

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(writexl)
# Maps
library(sf)

`%!in%` = Negate(`%in%`)

# ---------------------------------------------------------------------------
# Read raw data from {quadrige.explorer} ----

data_ROCCHMV <- esteem.overview::data_contamination

# ---------------------------------------------------------------------------
# Cleaning variables

# data_ROCCHMV |>
#   count(CITATION)

data_ROCCHMV <- data_ROCCHMV |>
  mutate(YEAR = year(DATE)) |>
  mutate(ESTUARY = case_when(
    ZONE_MARINE_QUADRIGE == "085 - Estuaire de la Gironde" ~ "Gironde",
    ZONE_MARINE_QUADRIGE == "070 - Estuaire de la Loire" ~ "Loire",
    ZONE_MARINE_QUADRIGE == "011 - Estuaire de la Seine" ~ "Seine")) |>
  select(-c(THEME, ZONE_MARINE_QUADRIGE, SOUS_REGION_MARINE_DCSMM, MASSE_EAU_DCE,
            PROGRAMME, CAMPAGNE, GROUPE_TAXON_LIBELLE, TAXON_LIBELLE,
            LIEU_IDENTIFIANT, PARAMETRE_LIBELLE_COMPLET, PASSAGE_DESCRIPTION,
            PASSAGE_COORDONNEES, PRELEVEMENT_COORDONNEES, PARAMETRE_CODE,
            NIVEAU_QUALITE, QUALITE_DESCRIPTION))

# ---------------------------------------------------------------------------
# Geographical points ----

data_ROCCHMV |>
  select(ESTUARY, LIEU_MNEMONIQUE, latitude, longitude) |>
  distinct() |>
  group_by(ESTUARY) |>
  summarise(lat_stat = paste0(min(latitude), " - ", max(latitude)),
            lon_stat = paste0(min(longitude), " - ", max(longitude)))

ggmap_ROCCHMV_gironde <- gg_map_data_contamination_rocchmv(data = data_ROCCHMV, estuary = "Gironde", color = ggplot2_colors(3)[3])
ggmap_ROCCHMV_loire <- gg_map_data_contamination_rocchmv(data = data_ROCCHMV, estuary = "Loire", color = ggplot2_colors(3)[2])
ggmap_ROCCHMV_seine <- gg_map_data_contamination_rocchmv(data = data_ROCCHMV, estuary = "Seine", color = ggplot2_colors(3)[1])

ggsave(plot = ggmap_ROCCHMV_gironde, filename = "inst/results/data_contam/maps/ggmap_ROCCHMV_gironde.jpg")
ggsave(plot = ggmap_ROCCHMV_loire, filename = "inst/results/data_contam/maps/ggmap_ROCCHMV_loire.jpg")
ggsave(plot = ggmap_ROCCHMV_seine, filename = "inst/results/data_contam/maps/ggmap_ROCCHMV_seine.jpg")

# ---------------------------------------------------------------------------
# Campaign period and frequencies ----

## Campains are organized several times per year
# data_ROCCHMV |>
#   distinct(ESTUARY, year_month) |>
#   arrange(ESTUARY, year_month) |>
#   View()

## Summarise results per year
data_ROCCHMV_summarised <- data_ROCCHMV |>
  group_by(ESTUARY, YEAR, SUPPORT_NIVEAU_PRELEVEMENT, PARAMETRE_LIBELLE, UNITE) |>
  summarise(RESULTAT = median(RESULTAT), .groups = "drop")

# ---------------------------------------------------------------------------
# Sample type ----

# data_ROCCHMV_summarised |>
#   count(ESTUARY, SUPPORT_NIVEAU_PRELEVEMENT)
# - "Support : Poisson" in the Seine estuary

data_ROCCHMV_summarised <- data_ROCCHMV_summarised |>
  mutate(
    support = str_extract(SUPPORT_NIVEAU_PRELEVEMENT, "Support\\s*:\\s*([^-\n]+)") |>
      str_remove("Support\\s*:\\s*") %>%
      str_trim(),
    espece = str_extract(SUPPORT_NIVEAU_PRELEVEMENT, "-\\s*([^-\n]+)\\s*-")  |>
      str_remove_all("-") %>%
      str_trim(),
    niveau = str_extract(SUPPORT_NIVEAU_PRELEVEMENT, "Niveau\\s*:\\s*(.*)")  |>
      str_remove("Niveau\\s*:\\s*") %>%
      str_trim()
  )

# data_ROCCHMV_summarised |> filter(support == "Poisson") |> View()

# ---------------------------------------------------------------------------
# Export cleaned dataset for benthos ----

data_ROCCHMV_cleaned <- data_ROCCHMV_summarised
usethis::use_data(data_ROCCHMV_cleaned, overwrite = TRUE)
write_xlsx(data_ROCCHMV_cleaned, "inst/results/data_contam/data_ROCCHMV_cleaned.xlsx")

