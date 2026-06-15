# =====================================================
# Data:
# -
# Plots:
#  - ggplot_intertidal_areas
# Preparation script
# Author: FM
# Date: 2026-06-02
# =====================================================

# =====================================================
# 0. Packages
# =====================================================

library(tidyverse)

data(data_intertidal_surface)

# =====================================================
# 1. Interpolation
# =====================================================

data_intertidal_surface_interp <- get_interpolation_spline(
  data = data_intertidal_surface,
  x = year,
  y = surface_ha,
  group = estuary)

usethis::use_data(data_intertidal_surface_interp, overwrite = TRUE)

# =====================================================
# 2. Graphs
# =====================================================

ggplot_intertidal_areas <- ggplot(
  data = data_intertidal_surface_interp) +
  aes(x = year, y = surface_ha, colour = estuary) +
  geom_line() +
  geom_point(data = data_intertidal_surface, aes(x = year, y = surface_ha, colour = estuary))
ggplot_intertidal_areas

ggsave(plot = ggplot_intertidal_areas, filename = "inst/mat_meth/ggplot_intertidal_areas.jpg")
