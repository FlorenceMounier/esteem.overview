# =====================================================
# Results: plot_POMET_*_size_class.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(dplyr)

# =====================================================
# 01. Compute species abundance
# =====================================================

# Calculation of abundance (= number of individuals/survey area (long line * long trawl))
#
# Small beam trawl (=CHAP3) * 1.5;
# Large beam trawl (CHAP3 DCE) * 3

data_POMET_density <- data_POMET |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax",
                     "Crangon crangon", "Palaemon longirostris")) |>
  group_by(estuary, annee, saison, materiel_code, trait_id, long_trait, name, haline_zone, size_class) |>
  # tot nb of individuals
  summarise(n_indiv = n(), .groups = "drop") |>
  # density per trait
  mutate(density_trait = case_when(
    materiel_code == "CHAP3" ~ (n_indiv / (long_trait * 1.5)) * 1000,
    materiel_code == "CHAP3 DCE" ~ (n_indiv / (long_trait * 3)) * 1000,
    TRUE ~ NA,
  )) |>
  # mean density per estuary, year, species and size class
  group_by(estuary, annee, saison, name, haline_zone, size_class) |>
  summarise(density = mean(density_trait), .groups = "drop") |>
  mutate(log_density = log(density)) |>
  # drop NA estuary and length
  drop_na(estuary)


# =====================================================
# 02. Save data results
# =====================================================

usethis::use_data(data_POMET_density, overwrite = TRUE)

# =====================================================
# 03. Fish density plots
# =====================================================

plot_POMET_density_fish <- data_POMET_density |>
  filter(name %in% c("Crangon crangon", "Palaemon longirostris")) |>
  filter(saison != "ete") |> # not enough points
ggplot() +
  aes(x = annee, y = density, colour = saison) +
  geom_line() +
  geom_smooth(
    method = "gam",
    se = FALSE,
    span = 0.6,
    linewidth = 1
  ) +
  labs(title = "Juvenile fish autumn densities") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_density_fish.jpg",
       plot = plot_POMET_density_fish, width = 20, height = 10, units = "cm")

# =====================================================
# 04. Shrimps density plots
# =====================================================

plot_POMET_density_shrimps <- data_POMET_density |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax")) |>
  drop_na(size_class) |>
  ggplot() +
  aes(x = annee, y = density, colour = size_class) +
  geom_line() +
  geom_smooth(
    method = "gam",
    se = FALSE,
    span = 0.6,
    linewidth = 1
  ) +
  labs(title = "Shrimps densities by season") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_density_shrimps.jpg",
       plot = plot_POMET_density_shrimps, width = 20, height = 10, units = "cm")
