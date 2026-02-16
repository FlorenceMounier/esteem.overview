---------------------------------------------------------------------------
# Libraries ----

library(esteem.overview)
library(tidyverse, quietly = TRUE)

`%!in%` = Negate(`%in%`)

# ---------------------------------------------------------------------------
# Read raw data from {esteem.overview} ----

data_POMET_biota <- esteem.overview::data_POMET_biota_ind
data_POMET_traits <- esteem.overview::data_POMET_traits

# ---------------------------------------------------------------------------
# Variables and join

# names(data_POMET_biota)
# names(data_POMET_traits)
#
# intersect(names(data_POMET_biota), names(data_POMET_traits))

data_POMET <- left_join(data_POMET_biota, data_POMET_traits,
          by = c("trait_id", "masse_eau", "annee", "distance_chalutee",
                 "saison", "engin", "long_trait", "materiel_code"))

# ---------------------------------------------------------------------------
# Create center position of traits

data_POMET <- data_POMET |>
  mutate(latitude = (pos_deb_lat_dd + pos_fin_lat_dd) / 2) |>
  mutate(longitude = (pos_deb_long_dd + pos_fin_long_dd) / 2)

# ---------------------------------------------------------------------------
# Estuary GPS delimitations and halin zones + cleaning

data_POMET <- data_POMET |>
  mutate(estuary = case_when(
    masse_eau |> str_starts("Gironde") ~ "Gironde",
    masse_eau |> str_starts("Loire") ~ "Loire",
    masse_eau |> str_starts("Seine") ~ "Seine",
    TRUE ~ NA
  )) |>
  mutate(
    estuary = case_when(
      estuary == "Gironde" & latitude > 45.0 & latitude < 45.7 & longitude < -0.6 & longitude > -1.1 ~ "Gironde",
      estuary == "Loire" & latitude > 47.22 & latitude < 47.34 & longitude < -1.8 & longitude > -2.3 ~ "Loire",
      estuary == "Seine" &
        latitude > 49.4 & longitude < 0.5 ~ "Seine",
      TRUE ~ NA
    )
  )  |>
  mutate(haline_zone = case_when(
    estuary == "Gironde" & latitude >= 45.4 ~ "polyhalin",
    estuary == "Gironde" & latitude >= 45.0 ~ "mesohalin",
    estuary == "Loire" & longitude <= -2.0 ~ "polyhalin",
    estuary == "Loire" & longitude <= -1.8 ~ "mesohalin",
    estuary == "Seine" & longitude <= 0.3 ~ "polyhalin",
    estuary == "Seine" & longitude <= 0.5 ~ "mesohalin",
    TRUE ~ NA
  )) |>
  filter(trait_id != 82) |> # fleuve Gironde
  filter(trait_id %!in% c(6185, 8494)) |>  # out of Loire
  filter(trait_id %!in% c(13477, 16087, 14362)) # filandres Seine

#------------------------------------------------------------------------------
# Save data data_POMET.rda

usethis::use_data(data_POMET, overwrite = TRUE)


# ---------------------------------------------------------------------------
# Estuaries maps - Haline zones from POMET ----

# ---------------------------------------------------------------------------
# Gironde ----

# map with haline zones
plot_POMET_map_gironde <- plot_estuary_map(
  data = data_POMET |> filter(estuary == "Gironde"),
  estuary_name = "Gironde", colour_var = haline_zone
)

# save the image
ggsave(filename = "inst/results/data_maps/POMET/plot_POMET_map_gironde.jpg",
       plot = plot_map_gironde)

# crop the image
img_gironde <- image_read("inst/results/data_maps/POMET/plot_POMET_map_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde)
image_write(img_trim_gironde, "inst/results/data_maps/POMET/plot_POMET_map_gironde.jpg")

# salinity distribution in haline zones
plot_POMET_salinity_zones_gironde <- ggplot(data = data_POMET |> filter(estuary == "Gironde")) +
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
ggsave(filename = "inst/results/data_maps/POMET/plot_POMET_salinity_zones_gironde.jpg",
       plot = plot_POMET_salinity_zones_gironde,
       width = 7, height = 7, units = "cm")

# ---------------------------------------------------------------------------
# Loire ----

plot_POMET_map_loire <- plot_estuary_map(
  data = data_POMET |> filter(estuary == "Loire"),
  estuary_name = "Loire", colour_var = haline_zone
)

ggsave(filename = "inst/results/data_maps/POMET/plot_POMET_map_loire.jpg",
       plot = plot_POMET_map_loire)

# crop the image
img_loire <- image_read("inst/results/data_maps/POMET/plot_POMET_map_loire.jpg")
img_trim_loire <- image_trim(img_loire)
image_write(img_trim_loire, "inst/results/data_maps/POMET/plot_POMET_map_loire.jpg")

# salinity distribution in haline zones
plot_POMET_salinity_zones_loire <- ggplot(data = data_POMET |> filter(estuary == "Loire")) +
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
ggsave(filename = "inst/results/data_maps/POMET/plot_POMET_salinity_zones_loire.jpg",
       plot = plot_POMET_salinity_zones_loire,
       width = 7, height = 7, units = "cm")

# ---------------------------------------------------------------------------
# Seine ----

plot_POMET_map_seine <- plot_estuary_map(
  data = data_POMET |> filter(estuary == "Seine"),
  estuary_name = "Seine", colour_var = haline_zone
)

ggsave(filename = "inst/results/data_maps/POMET/plot_POMET_map_seine.jpg",
       plot = plot_POMET_map_seine)

# crop the image
img_seine <- image_read("inst/results/data_maps/POMET/plot_POMET_map_seine.jpg")
img_trim_seine <- image_trim(img_seine)
image_write(img_trim_seine, "inst/results/data_maps/POMET/plot_POMET_map_seine.jpg")

# salinity distribution in haline zones
plot_POMET_salinity_zones_seine <- ggplot(data = data_POMET |> filter(estuary == "Seine")) +
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
ggsave(filename = "inst/results/data_maps/POMET/plot_POMET_salinity_zones_seine.jpg",
       plot = plot_POMET_salinity_zones_seine,
       width = 7, height = 7, units = "cm")

