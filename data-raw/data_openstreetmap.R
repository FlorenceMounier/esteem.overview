library(esteem.overview)
library(tidyverse)
library(magick)

# ---------------------------------------------------------------------------
# Halin limits by estuary

halin_table <- tibble::tibble(
  estuary = c("Gironde", "Loire", "Seine"),
  halin_limit_lat = c(45.4, NA, NA),
  halin_limit_lon = c(NA, -2.0, 0.3)
)

usethis::use_data(halin_table)


# ---------------------------------------------------------------------------
# Base OpenStreetMaps by estuary

fct_build_and_save_basemap(
  data = data_POMET |> filter(estuary == "Gironde"),
  estuary_name = "Gironde",
  villes_selection = c("Royan", "Pauillac", "Saint-Estèphe", "Blaye", "Saint-Christoly-Médoc")
)

fct_build_and_save_basemap(
  data = data_POMET |> filter(estuary == "Loire"),
  estuary_name = "Loire",
  villes_selection = c("Saint-Nazaire", "Cordemais")
)

fct_build_and_save_basemap(
  data = data_POMET |> filter(estuary == "Seine"),
  estuary_name = "Seine",
  villes_selection = c("Le Havre", "Honfleur", "Tancarville")
)

# ---------------------------------------------------------------------------
# Estuaries maps - Haline zones from POMET ----

# ---------------------------------------------------------------------------
# Gironde ----

# map with haline zones
plot_map_gironde <- plot_estuary_map(
  data = data_POMET |> filter(estuary == "Gironde"),
  estuary_name = "Gironde", colour_var = haline_zone
)

# save the image
ggsave(filename = "inst/results/data_maps/plot_map_gironde.jpg",
       plot = plot_map_gironde)

# crop the image
img_gironde <- image_read("inst/results/data_maps/plot_map_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde)
image_write(img_trim_gironde, "inst/results/data_maps/plot_map_gironde.jpg")

# salinity distribution in haline zones
plot_salinity_zones_gironde <- ggplot(data = data_POMET |> filter(estuary == "Gironde")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 2) +
  theme(legend.position = "none") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # supprime les labels
    axis.ticks.x = element_blank()   # supprime les ticks
  )

# save the image
ggsave(filename = "inst/results/data_maps/plot_salinity_zones_gironde.jpg",
       plot = plot_salinity_zones_gironde,
       width = 7, height = 7, units = "cm")

# ---------------------------------------------------------------------------
# Loire ----

plot_map_loire <- plot_estuary_map(
  data = data_POMET |> filter(estuary == "Loire"),
  estuary_name = "Loire", colour_var = haline_zone
)

ggsave(filename = "inst/results/data_maps/plot_map_loire.jpg",
       plot = plot_map_loire)

# crop the image
img_loire <- image_read("inst/results/data_maps/plot_map_loire.jpg")
img_trim_loire <- image_trim(img_loire)
image_write(img_trim_loire, "inst/results/data_maps/plot_map_loire.jpg")

# salinity distribution in haline zones
plot_salinity_zones_loire <- ggplot(data = data_POMET |> filter(estuary == "Loire")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 2) +
  theme(legend.position = "none") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # supprime les labels
    axis.ticks.x = element_blank()   # supprime les ticks
  )

# save the image
ggsave(filename = "inst/results/data_maps/plot_salinity_zones_loire.jpg",
       plot = plot_salinity_zones_loire,
       width = 7, height = 7, units = "cm")

# ---------------------------------------------------------------------------
# Seine ----

plot_map_seine <- plot_estuary_map(
  data = data_POMET |> filter(estuary == "Seine"),
  estuary_name = "Seine", colour_var = haline_zone
)

ggsave(filename = "inst/results/data_maps/plot_map_seine.jpg",
       plot = plot_map_seine)

# crop the image
img_seine <- image_read("inst/results/data_maps/plot_map_seine.jpg")
img_trim_seine <- image_trim(img_seine)
image_write(img_trim_seine, "inst/results/data_maps/plot_map_seine.jpg")

# salinity distribution in haline zones
plot_salinity_zones_seine <- ggplot(data = data_POMET |> filter(estuary == "Seine")) +
  aes(x = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_vline(xintercept = 18, linewidth = 2) +
  theme(legend.position = "none") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # supprime les labels
    axis.ticks.x = element_blank()   # supprime les ticks
  )

# save the image
ggsave(filename = "inst/results/data_maps/plot_salinity_zones_seine.jpg",
       plot = plot_salinity_zones_seine,
       width = 7, height = 7, units = "cm")
