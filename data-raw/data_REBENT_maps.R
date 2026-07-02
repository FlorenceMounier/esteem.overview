# =====================================================
# Plots:
#  - plot_REBENT_map_gironde.jpg
#  - plot_REBENT_map_loire.jpg
#  - plot_REBENT_map_seine.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

# =====================================================
# 01. Gironde estuary map - Haline zones from REBENT
# =====================================================

# map
plot_REBENT_map_gironde <- plot_estuary_map(
  data = data_REBENT |> dplyr::filter(estuary == "Gironde"),
  estuary_name = "Gironde",
  colour_var = haline_zone,
  shape_var = tidal
)

# save the image
ggsave(filename = "inst/results/data_maps/REBENT/plot_REBENT_map_gironde.jpg",
       plot = plot_REBENT_map_gironde)

# crop the image
img_gironde <- image_read("inst/results/data_maps/REBENT/plot_REBENT_map_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde)
image_write(img_trim_gironde, "inst/results/data_maps/REBENT/plot_REBENT_map_gironde.jpg")


# =====================================================
# 02. Loire estuary map - Haline zones from REBENT
# =====================================================

# map
plot_REBENT_map_loire <- plot_estuary_map(
  data = data_REBENT |> dplyr::filter(estuary == "Loire"),
  estuary_name = "Loire",
  colour_var = haline_zone,
  shape_var = tidal
)

# save the image
ggsave(filename = "inst/results/data_maps/REBENT/plot_REBENT_map_loire.jpg",
       plot = plot_REBENT_map_loire)

# crop the image
img_loire <- image_read("inst/results/data_maps/REBENT/plot_REBENT_map_loire.jpg")
img_trim_loire <- image_trim(img_loire)
image_write(img_trim_loire, "inst/results/data_maps/REBENT/plot_REBENT_map_loire.jpg")


# =====================================================
# 03. Seine estuary map - Haline zones from REBENT
# =====================================================


# map
plot_REBENT_map_seine <- plot_estuary_map(
  data = data_REBENT |> dplyr::filter(estuary == "Seine"),
  estuary_name = "Seine",
  colour_var = haline_zone,
  shape_var = tidal
)

# save the image
ggsave(filename = "inst/results/data_maps/REBENT/plot_REBENT_map_seine.jpg",
       plot = plot_REBENT_map_seine)

# crop the image
img_seine <- image_read("inst/results/data_maps/REBENT/plot_REBENT_map_seine.jpg")
img_trim_seine <- image_trim(img_seine)
image_write(img_trim_seine, "inst/results/data_maps/REBENT/plot_REBENT_map_seine.jpg")
