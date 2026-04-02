# =====================================================
# Results: plot_POMET_map_*.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(dplyr)
library(magick) # crop images
library(patchwork)

# =====================================================
# 01. Gironde haline zones from POMET
# =====================================================

# ----- map with haline zones -----
plot_POMET_map_gironde <- plot_estuary_map(
  data = data_POMET_densities |> filter(estuary == "Gironde"),
  estuary_name = "Gironde", colour_var = haline_zone
)

# ----- save the image -----
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg",
       plot = plot_POMET_map_gironde)

# ----- crop & save the image -----
img_gironde <- image_read("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde)
image_write(img_trim_gironde, "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg")

# ----- plot salinity distribution in haline zones -----
plot_POMET_salinity_zones_gironde <- ggplot(data = data_POMET_densities |> filter(estuary == "Gironde")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 1.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # supprime les labels
    axis.ticks.x = element_blank()   # supprime les ticks
  )

# ---- Join plots in one image ----
ggplot_POMET_haline_zones_gironde <- plot_POMET_map_gironde + plot_POMET_salinity_zones_gironde +
  plot_layout(widths = c(7, 3), heights = c(3,1))

# ----- save the image -----
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg",
       plot = ggplot_POMET_haline_zones_gironde)

# ----- crop the image -----
img_gironde <- image_read("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde)
image_write(img_trim_gironde, "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg")



# =====================================================
# 02. Loire haline zones from POMET
# =====================================================

# ----- map with haline zones -----
plot_POMET_map_loire <- plot_estuary_map(
  data = data_POMET_densities |> filter(estuary == "Loire"),
  estuary_name = "Loire", colour_var = haline_zone
)

# ----- save the image -----
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg",
       plot = plot_POMET_map_loire)

# ----- crop the image -----
img_loire <- image_read("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg")
img_trim_loire <- image_trim(img_loire)
image_write(img_trim_loire, "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg")

# ----- plot salinity distribution in haline zones -----
plot_POMET_salinity_zones_loire <- ggplot(data = data_POMET_densities |> filter(estuary == "Loire")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 1.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # supprime les labels
    axis.ticks.x = element_blank()   # supprime les ticks
  )

# ---- Join plots in one image ----
ggplot_POMET_haline_zones_loire <- plot_POMET_map_loire + plot_POMET_salinity_zones_loire +
  plot_layout(widths = c(4, 1), heights = c(1, 2))

# ----- save the image -----
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg",
       plot = ggplot_POMET_haline_zones_loire)

# ----- crop the image -----
img_loire <- image_read("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg")
img_trim_loire <- image_trim(img_loire)
image_write(img_trim_loire, "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg")


# =====================================================
# 03. Seine haline zones from POMET
# =====================================================

# ----- map with haline zones -----
plot_POMET_map_seine <- plot_estuary_map(
  data = data_POMET_densities |> filter(estuary == "Seine"),
  estuary_name = "Seine", colour_var = haline_zone
)

# ----- save the image -----
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg",
       plot = plot_POMET_map_seine)

# ----- crop the image -----
img_seine <- image_read("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg")
img_trim_seine <- image_trim(img_seine)
image_write(img_trim_seine, "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg")

# ----- plot salinity distribution in haline zones -----
plot_POMET_salinity_zones_seine <- ggplot(data = data_POMET_densities |> filter(estuary == "Seine")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 1.5) +
  theme(legend.position = "none") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # supprime les labels
    axis.ticks.x = element_blank()   # supprime les ticks
  )

# ---- Join plots in one image ----
ggplot_POMET_haline_zones_seine <- plot_POMET_map_seine + plot_POMET_salinity_zones_seine +
  plot_layout(widths = c(4, 1), heights = c(1, 2))

# ----- save the image -----
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg",
       plot = ggplot_POMET_haline_zones_seine)

# ----- crop the image -----
img_seine <- image_read("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg")
img_trim_seine <- image_trim(img_seine)
image_write(img_trim_seine, "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg")

