# =====================================================
# Datasets:
#  -
# Plots:
#  - ggplot_REBENT_sp_richness.jpg
#  - ggplot_shannon_index.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 0. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(benthos)
library(writexl)

# =====================================================
# 1. Species richness (S)
# =====================================================

species_richness <- data_REBENT_biota |>
  group_by(estuary, YEAR, tidal, species) |>
  summarise(count = sum(RESULTAT)) |>
  summarise(sp_rich = species_richness(taxon = species, count = count), .groups = "drop")

ggplot_REBENT_sp_richness <- ggplot(species_richness) +
  aes(x = YEAR, y = sp_rich, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_point() +
  geom_line() +
  labs(title = "REBENT macrobenthic fauna - Species richness",
       x = NULL, y = "Species richness",
       color = "Estuary") +
  theme_esteem()

ggsave(plot = ggplot_REBENT_sp_richness,
       filename = "inst/results/data_benthos/indicators/ggplot_REBENT_species_richness.jpg",
       width = 15, height = 6, units = "cm")

# =====================================================
# 2. Shannon index (H')
# =====================================================

# ni = Number of individuals for each species per estuary, tidal zone and year
nb_ind_sp <- data_REBENT_biota |>
  group_by(estuary, tidal, YEAR, species) |>
  summarise(n_ind_sp = sum(RESULTAT), .groups = "drop")

# N = Total number of individuals per estuary, tidal zone and year
nb_ind_tot <- data_REBENT_biota |>
  group_by(estuary, tidal, YEAR) |>
  summarise(n_ind_tot = sum(RESULTAT), .groups = "drop")

# pi = Percentage abundance for each species per estuary, tidal zone and year
abundance_prop <- nb_ind_sp |>
  left_join(nb_ind_tot, by = c("estuary", "tidal", "YEAR")) |>
  group_by(estuary, tidal, YEAR) |>
  mutate(abundance_prop = n_ind_sp / n_ind_tot) |>
  ungroup()

shannon_index <- abundance_prop |>
  group_by(estuary, tidal, YEAR) |>
  summarise(shannon = - sum(abundance_prop * log2(abundance_prop)), .groups = "drop")

ggplot_shannon_index <- ggplot(shannon_index) +
  aes(x = YEAR, y = shannon, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_point() +
  geom_line() +
  labs(title = "REBENT macrobenthic fauna - Shannon index",
       x = NULL, y = "Shannon index",
       color = "Estuary") +
  theme_esteem()

ggsave(plot = ggplot_shannon_index,
       filename = "inst/results/data_benthos/indicators/ggplot_REBENT_shannon_index.jpg",
       width = 15, height = 6, units = "cm")
