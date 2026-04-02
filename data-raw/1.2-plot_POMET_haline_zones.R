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
library(cowplot)

fct_img_crop <- function(path) {
  ggdraw() +
    draw_image(path) +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# =====================================================
# 01. Gironde haline zones from POMET
# =====================================================

# ----- map with haline zones -----
plot_POMET_map_gironde <- plot_estuary_map(
  data = data_POMET_densities |> filter(estuary == "Gironde"),
  estuary_name = "Gironde", colour_var = haline_zone
) +
  theme(legend.position = "bottom") +
  labs(colour = "Haline sector")
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg",
       plot = plot_POMET_map_gironde)

img_gironde <- image_read("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde) # crop image
image_write(img_trim_gironde, "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg")

# ----- plot salinity distribution in haline zones -----
plot_POMET_salinity_zones_gironde <- ggplot(data = data_POMET_densities |> filter(estuary == "Gironde")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 1.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()) +
  ggplot2::labs(x = "Salinity (g/L)") +
  coord_flip()

ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_salinity_zones_gironde.jpg",
       width = 7, height = 7, units = "cm", dpi = 300,
       plot = plot_POMET_salinity_zones_gironde)


# ---- Join images with {cowplot} ----

ggplot_POMET_haline_zones_gironde <- plot_grid(
  fct_img_crop("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_gironde.jpg"),
  fct_img_crop("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_salinity_zones_gironde.jpg"),
  ncol = 2,
  rel_widths = c(1.5, 1), scale = c(0.7, 1)
)
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg",
       plot = ggplot_POMET_haline_zones_gironde)

img_gironde <- image_read("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde) # crop the image
image_write(img_trim_gironde, "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg")


# =====================================================
# 02. Loire haline zones from POMET
# =====================================================

# ----- map with haline zones -----
plot_POMET_map_loire <- plot_estuary_map(
  data = data_POMET_densities |> filter(estuary == "Loire"),
  estuary_name = "Loire", colour_var = haline_zone
) +
  theme(legend.position = "bottom") +
  labs(colour = "Haline sector")
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg",
       plot = plot_POMET_map_loire)

img_loire <- image_read("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg")
img_trim_loire <- image_trim(img_loire) # crop the image
image_write(img_trim_loire, "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg")

# ----- plot salinity distribution in haline zones -----
plot_POMET_salinity_zones_loire <- ggplot(data = data_POMET_densities |> filter(estuary == "Loire")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 1.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()) +
  ggplot2::labs(x = "Salinity (g/L)") +
  coord_flip()

ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_salinity_zones_loire.jpg",
       width = 7, height = 7, units = "cm", dpi = 300,
       plot = plot_POMET_salinity_zones_loire)


# ---- Join images with {cowplot} ----

ggplot_POMET_haline_zones_loire <- plot_grid(
  ggdraw() + draw_image("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_loire.jpg"),
  ggdraw() + draw_image("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_salinity_zones_loire.jpg"),
  ncol = 2,
  rel_widths = c(1.5, 1)
)
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg",
       plot = ggplot_POMET_haline_zones_loire)

img_loire <- image_read("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg")
img_trim_loire <- image_trim(img_loire) # crop the image
image_write(img_trim_loire, "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg")


# =====================================================
# 03. Seine haline zones from POMET
# =====================================================

# ----- map with haline zones -----
plot_POMET_map_seine <- plot_estuary_map(
  data = data_POMET_densities |> filter(estuary == "Seine"),
  estuary_name = "Seine", colour_var = haline_zone
) +
  theme(legend.position = "bottom") +
  labs(colour = "Haline sector")

ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg",
       plot = plot_POMET_map_seine)

img_seine <- image_read("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg")
img_trim_seine <- image_trim(img_seine) # crop the image
image_write(img_trim_seine, "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg")

# ----- plot salinity distribution in haline zones -----
plot_POMET_salinity_zones_seine <- ggplot(data = data_POMET_densities |> filter(estuary == "Seine")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 1.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()) +
  ggplot2::labs(x = "Salinity (g/L)") +
  coord_flip()

ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/plot_POMET_salinity_zones_seine.jpg",
       width = 7, height = 7, units = "cm", dpi = 300,
       plot = plot_POMET_salinity_zones_seine)


# ---- Join images with {cowplot} ----

ggplot_POMET_haline_zones_seine <- plot_grid(
  ggdraw() + draw_image("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_map_seine.jpg"),
  ggdraw() + draw_image("inst/mat_meth/maps/POMET/haline_zones/plot_POMET_salinity_zones_seine.jpg"),
  ncol = 2,
  rel_widths = c(1.5, 1)
)
ggsave(filename = "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg",
       plot = ggplot_POMET_haline_zones_seine)

img_seine <- image_read("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg")
img_trim_seine <- image_trim(img_seine) # crop the image
image_write(img_trim_seine, "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg")


# =====================================================
# 04. Join Gironde, Loire, Seine haline zones from POMET
# =====================================================



ggplot_POMET_haline_zones_estuaries <- plot_grid(
  img("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_gironde.jpg"),
  img("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_loire.jpg"),
  img("inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_seine.jpg"),
  ncol = 1,
  align = "v"
)


ggsave(
  "inst/mat_meth/maps/POMET/haline_zones/ggplot_POMET_haline_zones_estuaries.png",
  ggplot_POMET_haline_zones_estuaries,
  width = 10,
  height = 12,
  dpi = 300
)
