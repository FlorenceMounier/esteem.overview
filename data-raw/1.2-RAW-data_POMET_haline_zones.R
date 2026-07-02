# =====================================================
# Define haline zones from POMET raw data
# Datasets:
#  - GPS_limits_haline.rda
#  - data_POMET_salinity.rda
# Graph in inst/mat_meth/haline_zones: ggplot_haline_zones.jpg
# Author: FM
# Date: 2026-07-02
# =====================================================


# =====================================================
# 00. Packages, functions and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(rpart)
library(cowplot)

`%!in%` = Negate(`%in%`)

data(raw_data_POMET_traits)


# =====================================================
# 01. Extract autumn salinity by estuary from raw_data_POMET_traits
# =====================================================

data_POMET_salinity <- raw_data_POMET_traits |>

# ---- Create GPS central position of traits ----

mutate(latitude = (pos_deb_lat_dd + pos_fin_lat_dd) / 2) |>
  mutate(longitude = (pos_deb_long_dd + pos_fin_long_dd) / 2) |>


# ---- Filter unrelevant observations -----

filter(trait_id != 82) |> # fleuve Gironde
  filter(trait_id %!in% c(6185, 8494)) |>  # out of Loire
  filter(trait_id %!in% c(13477, 16087, 14362)) |> # filandres Seine


# ---- Add estuary information from GPS positions of traits ----

get_estuary_from_gps_position(latitude = latitude, longitude = longitude)


# ---- Create sub-dataset by estuary ----

data_POMET_gironde_salinity_autumn <- data_POMET_salinity |> filter(estuary == "Gironde", saison == "automne")
data_POMET_loire_salinity_autumn <- data_POMET_salinity |> filter(estuary == "Loire", saison == "automne")
data_POMET_seine_salinity_autumn <- data_POMET_salinity |> filter(estuary == "Seine", saison == "automne")



# =====================================================
# 02. Estimate haline zone limits by estuary
# =====================================================

# ---- Gironde ----

tree_gironde <- rpart(salinite ~ latitude, data = data_POMET_gironde_salinity_autumn, method = "anova")
halin_limit_lat_gironde = tree_gironde$splits[[1,4]]
halin_limit_lat_salin_gironde = tree_gironde$frame$yval[1]

# ---- Loire ----

tree_loire <- rpart(salinite ~ longitude, data = data_POMET_loire_salinity_autumn, method = "anova")
halin_limit_lon_loire = tree_loire$splits[[1,4]]
halin_limit_lon_salin_loire = tree_loire$frame$yval[1]

# ---- Seine ----

tree_seine <- rpart(salinite ~ longitude, data = data_POMET_seine_salinity_autumn, method = "anova")
halin_limit_lon_seine = tree_seine$splits[[1,4]]
halin_limit_lon_salin_seine = tree_seine$frame$yval[1]


# ---- Save haline zone limits results ----

GPS_limits_haline <- tibble::tibble(
  estuary = c("Gironde", "Loire", "Seine"),
  halin_limit_lat = c(halin_limit_lat_gironde, NA, NA),
  halin_limit_lon = c(NA, halin_limit_lon_loire, halin_limit_lon_seine),
)

usethis::use_data(GPS_limits_haline, overwrite = TRUE)


# ---- Add haline_zone variable to dataset ----

data_POMET_salinity <- get_haline_zone_from_gps_position(data_POMET_salinity,
                                                         latitude = latitude,
                                                         longitude = longitude)
usethis::use_data(data_POMET_salinity, overwrite = TRUE)



# =====================================================
# 03. Map of haline zone limits and salinity boxplots by estuary
# =====================================================

# ---- Gironde ----

data_Gironde <- data_POMET_salinity |> filter(saison == "automne")

map_gironde <- plot_estuary_map(data = data_Gironde,
                                estuary_name = "Gironde",
                                colour_var = haline_zone) +
  theme(legend.position = "none")

salinity_zones_gironde <- ggplot(data = data_Gironde) +
  aes(x = haline_zone, y = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_hline(yintercept = halin_limit_lat_salin_gironde, linewidth = 0.8) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  ggplot2::labs(y = "Salinity (g/L)")


# ---- Loire ----

data_Loire <- data_POMET_salinity |> filter(saison == "automne")

map_loire <- plot_estuary_map(data = data_Loire, estuary_name = "Loire", colour_var = haline_zone) +
  theme(legend.position = "none")

salinity_zones_loire <- ggplot(data = data_Loire) +
  aes(x = haline_zone, y = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_hline(yintercept = halin_limit_lon_salin_loire, linewidth = 0.8) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  ggplot2::labs(y = "Salinity (g/L)")

# ---- Seine ----

data_Seine <- data_POMET_salinity |> filter(saison == "automne")

map_seine <- plot_estuary_map(data = data_Seine,
                              estuary_name = "Seine",
                              colour_var = haline_zone) +
  theme(legend.position = "none")

salinity_zones_seine <- ggplot(data = data_Seine) +
  aes(x = haline_zone, y = salinite, fill = haline_zone) +
  geom_boxplot() +
  geom_hline(yintercept = halin_limit_lon_salin_seine, linewidth = 0.8) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 6)) +
  ggplot2::labs(y = "Salinity (g/L)")


# ---- Cowplot ----

ggplot_haline_zone <- plot_grid(map_gironde, salinity_zones_gironde,
                                map_loire, salinity_zones_loire,
                                map_seine, salinity_zones_seine,
                                nrow = 3, rel_heights = c(1.4, 1, 1),
                                ncol = 2, rel_widths = c(2,1))

ggsave(plot = ggplot_haline_zone,
       filename = "inst/mat_meth/haline_zones/ggplot_haline_zones.jpg",
       height = 10, width = 7, units = "cm")
