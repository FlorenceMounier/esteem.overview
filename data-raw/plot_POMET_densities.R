# =====================================================
# Dataset: data_POMET_partial_density.rda
# Results: plot_POMET_*_size_class.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(dplyr)
library(changepoint)

# =====================================================
# 01. Fish density plots at estuary level
# =====================================================

data_POMET_mean_estuary_densities <- data_POMET_densities |>
  group_by(estuary, annee, saison, name) |>
  summarise(mean_density = mean(Densite, na.rm = TRUE), .groups = "drop") |>
  filter(saison != "ete")

# ---- Common sole ----

plot_POMET_estuary_density_sole <- data_POMET_mean_estuary_densities |>
  filter(name == "Solea solea") |>
  ggplot() +
  aes(x = annee, y = mean_density, color = saison) +
  geom_line() +
  labs(title = "Juvenile common sole estuarine densities") +
  facet_grid(rows = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_estuary_density_sole.jpg",
       plot = plot_POMET_estuary_density_sole, width = 20, height = 10, units = "cm")


# ---- European seabass ----

plot_POMET_estuary_density_seabass <- data_POMET_mean_estuary_densities |>
  filter(name == "Dicentrarchus labrax") |>
  ggplot() +
  aes(x = annee, y = mean_density, color = saison) +
  geom_line() +
  labs(title = "Juvenile common sole estuarine densities") +
  facet_grid(rows = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_estuary_density_seabass.jpg",
       plot = plot_POMET_estuary_density_seabass, width = 20, height = 10, units = "cm")

# =====================================================
# 02. Fish autumn density plots at haline zone level
# =====================================================

data_POMET_mean_haline_zone_densities <- data_POMET_densities |>
  group_by(estuary, annee, saison, haline_zone, name) |>
  summarise(mean_density = mean(Densite, na.rm = TRUE), .groups = "drop") |>
  filter(saison != "ete")

# ---- Common sole ----

plot_POMET_haline_density_sole <- data_POMET_mean_haline_zone_densities |>
  filter(name == "Solea solea") |>
  filter(saison == "automne") |>
  ggplot() +
  aes(x = annee, y = mean_density, color = haline_zone) +
  geom_line() +
  labs(title = "Juvenile common sole autumn densities") +
  facet_grid(rows = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_haline_density_sole.jpg",
       plot = plot_POMET_haline_density_sole, width = 20, height = 10, units = "cm")


# ---- European seabass ----

plot_POMET_haline_density_seabass <- data_POMET_mean_haline_zone_densities |>
  filter(name == "Dicentrarchus labrax") |>
  filter(saison == "automne") |>
  ggplot() +
  aes(x = annee, y = mean_density, color = haline_zone) +
  geom_line() +
  labs(title = "Juvenile european seabass autumn densities") +
  facet_grid(rows = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_haline_density_seabass.jpg",
       plot = plot_POMET_haline_density_seabass, width = 20, height = 10, units = "cm")

# =====================================================
# 03. Shrimps density plots
# =====================================================

plot_POMET_estuary_density_shrimps <- data_POMET_mean_estuary_densities |>
  filter(name %in% c("Crangon crangon", "Palaemon longirostris")) |>
  ggplot() +
  aes(x = annee, y = mean_density, colour = saison) +
  geom_line() +
  labs(title = "Shrimps densities by season") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_estuary_density_shrimps.jpg",
       plot = plot_POMET_estuary_density_shrimps, width = 20, height = 10, units = "cm")


plot_POMET_haline_density_shrimps <- data_POMET_mean_haline_zone_densities |>
  filter(name %in% c("Crangon crangon", "Palaemon longirostris")) |>
  ggplot() +
  aes(x = annee, y = mean_density, colour = haline_zone) +
  geom_line() +
  labs(title = "Shrimps densities by season") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_haline_density_shrimps.jpg",
       plot = plot_POMET_haline_density_shrimps, width = 20, height = 10, units = "cm")

