# =====================================================
# Dataset: data_POMET_partial_density.rda
# Results: plot_POMET_*_size_*.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(dplyr)

# =====================================================
# 01. Compute species abundance by size class
# =====================================================

# Calculation of abundance (= number of individuals/survey area (long line * long trawl))
#
# Small beam trawl (=CHAP3) * 1.5;
# Large beam trawl (CHAP3 DCE) * 3

data_POMET_partial_density <- data_POMET_indiv |>
  group_by(estuary, annee, saison, materiel_code, trait_id, long_trait, name, haline_zone, size_class) |>
  # tot nb of individuals
  summarise(n_indiv = n(), .groups = "drop") |>
  # density per trait
  mutate(density_trait = case_when(
    materiel_code == "CHAP3" ~ (n_indiv / (long_trait * 1.5)) * 1000,
    materiel_code == "CHAP3 DCE" ~ (n_indiv / (long_trait * 3)) * 1000,
    TRUE ~ NA,
  )) |>
  filter(saison != "ete")

usethis::use_data(data_POMET_partial_density, overwrite = TRUE)

# =====================================================
# 02. Fish density plots by size class & season at estuary level
# =====================================================

data_POMET_partial_estuary_mean_density <- data_POMET_partial_density |>
  # mean density per estuary, year, species and size class
  group_by(estuary, annee, saison, name, size_class) |>
  summarise(mean_density = mean(density_trait, na.rm = TRUE), .groups = "drop") |>
  # drop NA estuary and length
  drop_na(estuary)

plot_POMET_estuary_size_density_fish <- data_POMET_partial_estuary_mean_density |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax")) |>
  drop_na(size_class) |>
  filter(saison != "ete") |> # not enough points
  ggplot() +
  aes(x = annee, y = mean_density, colour = saison, linetype = size_class) +
  geom_line() +
  labs(title = "Juvenile fish densities by size class") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_estuary_size_density_fish.jpg",
       plot = plot_POMET_estuary_size_density_fish, width = 20, height = 10, units = "cm")


# =====================================================
# 03. Fish density plots by size class & haline zones
# =====================================================

data_POMET_partial_haline_mean_density <- data_POMET_partial_density |>
  # mean density per estuary, haline zone, year, species and size class
  group_by(estuary, annee, saison, name, haline_zone, size_class) |>
  summarise(mean_density = mean(density_trait, na.rm = TRUE), .groups = "drop") |>
  # drop NA estuary and length
  drop_na(estuary)

# ---- Common sole ----

plot_POMET_haline_size_density_sole <- data_POMET_partial_haline_mean_density |>
  filter(name == "Solea solea") |>
  drop_na(size_class) |>
ggplot() +
  aes(x = annee, y = mean_density, colour = size_class, linetype = haline_zone) +
  geom_line() +
  labs(title = "Juvenile sole densities by haline zone & size class") +
  facet_grid(rows = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_haline_size_density_sole.jpg",
       plot = plot_POMET_haline_size_density_sole, width = 20, height = 10, units = "cm")

# ---- European seabass ----

plot_POMET_haline_size_density_seabass <- data_POMET_partial_haline_mean_density |>
  filter(name == "Dicentrarchus labrax") |>
  drop_na(size_class) |>
  filter(saison != "ete") |> # not enough points
  ggplot() +
  aes(x = annee, y = mean_density, colour = size_class, linetype = haline_zone) +
  geom_line() +
  labs(title = "Juvenile seabass densities by size class") +
  facet_grid(rows = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_haline_size_density_seabass.jpg",
       plot = plot_POMET_haline_size_density_seabass, width = 20, height = 10, units = "cm")
