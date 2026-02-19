# =====================================================
# Plots:
#  - ggplot_sediment_organic_matter.jpg
#  - ggplot_sediment_grain_size.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 0. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

# =====================================================
# 1. Dataset of sediment from samples studied for biota
# =====================================================

data <- esteem.overview::data_REBENT_sediment |> drop_na(tidal)

# =====================================================
# 2. Organic matter
# =====================================================

data_MO <- data |>
  filter(PARAMETRE_LIBELLE == "Matière organique") |>
  group_by(estuary, YEAR, tidal) |>
  summarise(median_MO = median(RESULTAT))

ggplot_sediment_organic_matter <- ggplot(data_MO) +
  aes(x = YEAR, y = median_MO, colour = estuary, shape = tidal, linetype = tidal) +
  geom_point() +
  geom_line() +
  labs(y = "Organic matter (%)", x = NULL) +
  theme_esteem()

ggsave(plot = ggplot_sediment_organic_matter,
       "inst/results/data_benthos/sediment/ggplot_sediment_organic_matter.jpg")


# =====================================================
# 3. Grain size
# =====================================================

# ---- Filter grain size data ----
data_grain_size <- data |>
  filter(PARAMETRE_LIBELLE != "Matière organique") |>
  group_by(estuary, tidal, YEAR, PARAMETRE_LIBELLE) |>
  summarise(median_res = median(RESULTAT), .groups = "drop") |>
  filter(median_res >= 2)

# ---- Transform PARAMETER_LIBELLE in mid-class numeric grain size
data_size_resume <- data_grain_size |>
  arrange(estuary, tidal, YEAR, desc(median_res)) |>
  mutate(mid_class = case_when(
    PARAMETRE_LIBELLE == "Fraction < 63 µm" ~ 63,
    PARAMETRE_LIBELLE == "Fraction de 63 à 80 µm" ~ 71,
    PARAMETRE_LIBELLE == "Fraction de 63 à 125 µm" ~ 94,
    PARAMETRE_LIBELLE == "Fraction de 80 à 100 µm" ~ 90,
    PARAMETRE_LIBELLE == "Fraction de 100 à 125 µm" ~ 112,
    PARAMETRE_LIBELLE == "Fraction de 125 à 250 µm" ~ 187,
    PARAMETRE_LIBELLE == "Fraction de 125 à 160 µm" ~ 142,
    PARAMETRE_LIBELLE == "Fraction de 160 à 200 µm" ~ 180,
    PARAMETRE_LIBELLE == "Fraction de 200 à 250 µm" ~ 225,
    PARAMETRE_LIBELLE == "Fraction de 250 à 315 µm" ~ 282,
    PARAMETRE_LIBELLE == "Fraction de 250 à 500 µm" ~ 375,
    PARAMETRE_LIBELLE == "Fraction de 315 à 400 µm" ~ 357,
    PARAMETRE_LIBELLE == "Fraction de 400 à 500 µm" ~ 450,
    PARAMETRE_LIBELLE == "Fraction de 500 à 630 µm" ~ 565,
    PARAMETRE_LIBELLE == "Fraction de 630 à 800 µm" ~ 715,
    PARAMETRE_LIBELLE == "Fraction de 800 µm à 1 mm" ~ 900,
    PARAMETRE_LIBELLE == "Fraction de 1 mm à 1,25 mm" ~ 1125,
    PARAMETRE_LIBELLE == "Fraction de 1,25 à 1,6 mm" ~ 1425,
    PARAMETRE_LIBELLE == "Fraction de 1,6 à 2 mm" ~ 1800,
    PARAMETRE_LIBELLE == "Fraction de 2 à 2,5 mm" ~ 2250,
    PARAMETRE_LIBELLE == "Fraction de 2,5 à 3,15 mm" ~ 2825,
    PARAMETRE_LIBELLE == "Fraction de 2 à 4 mm" ~ 3000
  ))

# ---- Graph ----

ggplot_sediment_grain_size <- ggplot(data = data_size_resume) +
  aes(x = factor(YEAR), y = median_res, fill = factor(mid_class)) +
  geom_col(position = "stack") +
  facet_grid(estuary ~ tidal)

ggsave(plot = ggplot_sediment_grain_size,
       "inst/results/data_benthos/sediment/ggplot_sediment_grain_size.jpg")
