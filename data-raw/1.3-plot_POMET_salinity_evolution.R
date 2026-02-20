# =====================================================
# Results: plot_POMET_salinity_evolution_*.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(dplyr)

# =====================================================
# 01. Gironde data central salinity from POMET
# =====================================================

# ----- filter central zone -----
data_center_estuary_gironde <- data_POMET_densities |>
  filter(estuary == "Gironde",
         haline_zone == "mesohalin",
         latitude >= 45.3)

# ----- map relative points -----
plot_POMET_map_central_salinity_gironde <- plot_estuary_map(
  data = data_center_estuary_gironde,
  estuary_name = "Gironde",
  colour_var = haline_zone,
  shape_var = saison
) +
  facet_wrap(vars(annee))

# ----- save the image -----
ggsave(filename = "inst/results/data_POMET/salinity/plot_POMET_map_central_salinity_gironde.jpg",
       plot = plot_POMET_map_central_salinity_gironde)

# =====================================================
# 02. Loire data central salinity from POMET
# =====================================================

# ----- filter central zone -----
data_center_estuary_loire <- data_POMET_densities |>
  filter(estuary == "Loire",
         haline_zone == "mesohalin",
         longitude <= -1.9)

# ----- map relative points -----
plot_POMET_map_central_salinity_loire <- plot_estuary_map(
  data = data_center_estuary_loire,
  estuary_name = "Loire", colour_var = haline_zone
) +
  facet_wrap(vars(annee))

# ----- save the image -----
ggsave(filename = "inst/results/data_POMET/salinity/plot_POMET_map_central_salinity_loire.jpg",
       plot = plot_POMET_map_central_salinity_loire)

# =====================================================
# 03. Seine data central salinity from POMET
# =====================================================

# ----- filter central zone -----
data_center_estuary_seine <- data_POMET_densities |>
  filter(estuary == "Seine",
         haline_zone == "mesohalin",
         longitude <= 0.4)

# ----- map relative points -----
plot_POMET_map_central_salinity_seine <- plot_estuary_map(
  data = data_center_estuary_seine,
  estuary_name = "Seine", colour_var = haline_zone
) +
  facet_wrap(vars(annee))

# ----- save the image -----
ggsave(filename = "inst/results/data_POMET/salinity/plot_POMET_map_central_salinity_seine.jpg",
       plot = plot_POMET_map_central_salinity_seine)

# =====================================================
# 04. central salinity evolution from POMET
# =====================================================

# ----- join central zone points from all estuaries -----
data_center_estuary <-
  rbind(data_center_estuary_gironde,
        data_center_estuary_loire,
        data_center_estuary_seine)

# ----- map salinity evolution by estuary, year & season -----
plot_POMET_central_salinity_evolution <- ggplot(data = data_center_estuary) +
  aes(x = annee, y = salinite) +
  labs(title = "Salinity time evolution in central zone") +
  geom_point() +
  facet_grid(rows = vars(saison), cols = vars(estuary)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 7), se = TRUE) +
  theme_esteem()

# ----- save the image -----
ggsave(filename = "inst/results/data_POMET/salinity/plot_POMET_central_salinity_evolution.jpg",
       plot = plot_POMET_central_salinity_evolution)
