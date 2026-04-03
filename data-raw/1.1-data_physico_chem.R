# =====================================================
# Preparation script
# Datasets:
#  - data_physchem.rda
#  - GPS_box.rda
#  - id_colors.rda
# Plots:
#  - ggplot_Temperature_Gironde_map.jpg
#  - ggplot_Temperature_Gironce_trend.jpg
#  - ggplot_Temperature_Loire_map.jpg
#  - ggplot_Temperature_Loire_trend.jpg
#  - ggplot_Temperature_Seine_map.jpg
#  - ggplot_Temperature_Seine_trend.jpg
# Author: FM
# Date: 2026-04-03
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(tidyverse, quietly = TRUE)

# =====================================================
# 01.Read raw data from {quadrige.explorer}
# =====================================================

data_physico_chem <- esteem.overview::raw_data_physico_chem

# =====================================================
# 02. Clean data
# =====================================================

data_physico_chem <- data_physico_chem |>

  # ---- Delete unnecessary variables ----
  dplyr::select(-c(GROUPE_TAXON_LIBELLE, TAXON_LIBELLE, NUMERO_INDIVIDU_OBSERVATION,
            PASSAGE_COORDONNEES, PRELEVEMENT_COORDONNEES,
            NIVEAU_QUALITE, QUALITE_DESCRIPTION)) |>

  # ----- Delete REPOMO program -----
filter(PROGRAMME != "REPOMO") |>

  # ----- Extract years, months -----
mutate(year = year(DATE), month = month(DATE))

#       # PARAMETRE_GROUPE: Biologie/Phytoplancton
#       "Chlorophylle a", "Phéopigments", # > production primaire
#
#       # PARAMETRE_GROUPE: Mesures physiques
#       "Température de l'eau",
#
#       # PARAMETRE_GROUPE: Mesures physiques/Matériel particulaire
#       "Turbidité",
#       "Turbidité FNU",
#       "Matière en suspension",
#
#       # PARAMETRE_GROUPE: Nutriments/Nutriments Inorganiques
#       "Silicate", # RNOHYD (1975-2016) + REPHY (2007-2024)
#       "Ammonium", "Azote nitreux (nitrite)", "Azote nitrique (nitrate)",
#       "Nitrate + nitrite", "Phosphate",
#
#       # PARAMETRE_GROUPE: Physicochimie
#       "Oxygène dissous", "pH", "Salinité"

# =====================================================
# 03. Haline zones
# =====================================================

limits_gironde <- esteem.overview::halin_table |> filter(estuary == "Gironde")
limits_seine <- esteem.overview::halin_table |> filter(estuary == "Seine")
limits_loire <- esteem.overview::halin_table |> filter(estuary == "Loire")


data_physico_chem <- data_physico_chem |>

  # ----- Filter estuaries GPS delimitations -----
mutate(
  estuary = case_when(
    estuary == "Gironde" &
      latitude > limits_gironde$estuary_limit_lat_min &
      latitude < limits_gironde$estuary_limit_lat_max &
      longitude > limits_gironde$estuary_limit_lon_min &
      longitude < limits_gironde$estuary_limit_lon_max ~ "Gironde",
    estuary == "Loire" &
      latitude > limits_loire$estuary_limit_lat_min &
      latitude < limits_loire$estuary_limit_lat_max &
      longitude > limits_loire$estuary_limit_lon_min &
      longitude < limits_loire$estuary_limit_lon_max ~ "Loire",
    estuary == "Seine" &
      latitude > limits_seine$estuary_limit_lat_min &
      latitude < limits_seine$estuary_limit_lat_max &
      longitude > limits_seine$estuary_limit_lon_min &
      longitude < limits_seine$estuary_limit_lon_max ~ "Seine",
    TRUE ~ NA
  )
)  |>
  filter(!is.na(estuary)) |>

  # ----- Create haline_zone variable -----
mutate(
  haline_zone = case_when(
    estuary == "Gironde" &
      latitude >= limits_gironde$halin_limit_lat ~ "polyhalin",
    estuary == "Gironde" &
      latitude >= limits_gironde$estuary_limit_lat_min ~ "mesohalin",
    estuary == "Loire" &
      longitude <= limits_loire$halin_limit_lon ~ "polyhalin",
    estuary == "Loire" &
      longitude <= limits_loire$estuary_limit_lon_max ~ "mesohalin",
    estuary == "Seine" &
      longitude <= limits_seine$halin_limit_lon ~ "polyhalin",
    estuary == "Seine" &
      longitude <= limits_seine$estuary_limit_lon_max ~ "mesohalin",
    TRUE ~ NA
  )
)  |>
  filter(!is.na(haline_zone)) |>

  # ----- Round GPS positions -----
mutate(latitude = round(latitude, digits = 2)) |>
  mutate(longitude = round(longitude, digits = 2))


# =====================================================
# 04. Prepare nitrogen indicator variable
# =====================================================

# Compute the sum of nitrite + nitrate

data_NO <- data_physchem |>
  filter(PARAMETRE_LIBELLE %in% c("Azote nitreux (nitrite)","Azote nitrique (nitrate)")) |>
  filter(month %in% c(9, 10, 11)) |>
  group_by(estuary, PROGRAMME, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
           PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE)) |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  summarise(sumNO2NO3 = `Azote nitreux (nitrite)` + `Azote nitrique (nitrate)`, .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sumNO2NO3",
         month = month(DATE)) |>
  rename(RESULTAT = sumNO2NO3) |>
  full_join(data_physchem |>
              filter(PARAMETRE_LIBELLE %in% c("Nitrate + nitrite", "Ammonium")) |>
              filter(month %in% c(9, 10, 11))) |>
  filter(year >= 1987)

# Compare the computed sum levels with existing sum

ggplot_N0sum_comparison <- data_NO |>
  filter(PARAMETRE_LIBELLE %in% c("sumNO2NO3", "Nitrate + nitrite")) |>
  ggplot() +
  aes(x = year, y = RESULTAT, colour = PARAMETRE_LIBELLE) +
  geom_line() +
  facet_grid(rows = vars(estuary))
ggplot_N0sum_comparison

ggsave(plot = ggplot_N0sum_comparison, filename = "inst/results/data_phychem/Nitrogen_indicator/ggplot_nitrogen_sum_comparison.jpg",width = 15, height = 10, units = "cm")

# Nitrogen cycle indicator

data_nitrogen_cycle <- data_NO |>
  group_by(estuary, PROGRAMME, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
           PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE)) |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  ungroup() |>
  mutate(N_indicator = case_when(
    is.na(`Nitrate + nitrite`) ~ Ammonium / (Ammonium + sumNO2NO3) ,
    TRUE ~ Ammonium / (Ammonium + `Nitrate + nitrite`)
  )) |>
  pivot_longer(cols = N_indicator, names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT") |>
  dplyr::select(-c(Ammonium, sumNO2NO3, `Nitrate + nitrite`))

ggplot_data_N_indicator <- ggplot(data_nitrogen_cycle) +
  aes(x = year, y = RESULTAT, colour = estuary) +
  geom_point() +
  geom_hline(yintercept = 0.2, colour = "yellow") +
  geom_hline(yintercept = 0.4, colour = "orange") +
  geom_hline(yintercept = 0.7, colour = "red") +
  facet_grid(vars(estuary), scales = "free_y")
# ggplot_data_N_indicator

ggsave(plot = ggplot_data_N_indicator,
       filename = "inst/results/data_phychem/Nitrogen_indicator/ggplot_nitrogen_cycle_indicator.jpg",width = 15, height = 10, units = "cm")

data_physico_chem <- full_join(data_physico_chem, data_nitrogen_cycle)

# ----- Save dataset -----
usethis::use_data(data_physico_chem, overwrite = TRUE)


# =====================================================
# 05. Define the colors of the measurement points
# =====================================================

id_colors <- c("orange", "red", "purple","yellow", "pink","brown", "deeppink" )

usethis::use_data(id_colors, overwrite = TRUE)


# =====================================================
# 07. Define the study areas for physical and chemical parameters
# =====================================================

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

fct_points_lieu_mnemonique <- function(estuary_name, GPS_box){

  points_polyhalin <- data_physico_chem |>
    filter(estuary == estuary_name) |>
    filter(longitude >= GPS_box$min_lon[1] & longitude <= GPS_box$max_lon[1] &
             latitude >= GPS_box$min_lat[1] & latitude <= GPS_box$max_lat[1]) |>
    distinct(haline_zone, LIEU_MNEMONIQUE, year, PROGRAMME) |>
    arrange(haline_zone, LIEU_MNEMONIQUE, year, PROGRAMME)

  points_mesohalin <- data_physico_chem |>
    filter(estuary == estuary_name) |>
    filter(longitude >= GPS_box$min_lon[2] & longitude <= GPS_box$max_lon[2] &
             latitude >= GPS_box$min_lat[2] & latitude <= GPS_box$max_lat[2]) |>
    distinct(haline_zone, LIEU_MNEMONIQUE, year, PROGRAMME) |>
    arrange(haline_zone, LIEU_MNEMONIQUE, year, PROGRAMME)

  points <- full_join(points_polyhalin, points_mesohalin)

  points_lieu_mnemonique <- points |> distinct(haline_zone, year, LIEU_MNEMONIQUE, PROGRAMME)

  return(points_lieu_mnemonique)
}

# ----  Map of measurement points ----

fct_ggplot_phychem_map <- function(data, phychem_var, estuary_name, GPS_box){

  plot_estuary_map(data = data,
                   estuary_name = estuary_name,
                   colour_var = LIEU_MNEMONIQUE) +
    scale_color_manual(values = id_colors) +
    geom_rect(
      data = GPS_box[GPS_box$haline_zone == "polyhalin",],
      aes(
        xmin = min_lon,
        xmax = max_lon,
        ymin = min_lat,
        ymax = max_lat
      ),
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    geom_rect(
      data = GPS_box[GPS_box$haline_zone == "mesohalin",],
      aes(
        xmin = min_lon,
        xmax = max_lon,
        ymin = min_lat,
        ymax = max_lat
      ),
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    labs(title = paste0(estuary_name, " - ", phychem_var, " sampling points"))
}

## ---- Filtered data per variable  ----

fct_cleaned_data_phychemvar <- function(estuary_name,
                                        phychemvar,
                                        points_lieu_mnemonique,
                                        months){

data_var <- data_physico_chem |>
  filter(PARAMETRE_LIBELLE == phychemvar) |>
  filter(estuary == estuary_name) |>
  filter(LIEU_MNEMONIQUE %in% points_lieu_mnemonique$LIEU_MNEMONIQUE) |>
  filter(month %in% months) |>
  filter(year >= 1987)

# ---- Filter outliers ----

mean_data_var <- mean(data_var$RESULTAT, na.rm = TRUE)
sd_data_var <- sd(data_var$RESULTAT, na.rm = TRUE)

data_var <- data_var |>
  filter(
    between(RESULTAT,
            mean_data_var - 1.96 * sd_data_var,
            mean_data_var + 1.96 * sd_data_var)
  )

return(data_var)
}

# # ---- Statistical tests ----
#
# library(changepoint)
# library(modifiedmk) # modified Mann-Kendall autocorrelation
# library(trend)
# # library(ggrepel)
# # library(segmented)
# # library(mgcv) # Generalized Additive Model (GAM) tendance non linéaire
#
# data_temp_gironde <- data_temp_gironde |>
#   complete(year) |>
#   mutate(period = case_when(
#     year >= min(year) & year <= min(year)+9 ~ "first period",
#     year >= max(year)-9 & year <= max(year) ~ "last period",
#   ))
#
# ## Time series segmentation
#
# temp_timeseries_gironde <- data_temp_gironde |> pull(RESULTAT)
# years_gironde <- data_temp_gironde |> pull(year)
#
# # Testing for a change in means
# cp_gironde <- changepoint::cpt.meanvar(temp_timeseries_gironde, method = "BinSeg")
# plot(cp_gironde)
# break_year_gironde <- years_gironde[changepoint::cpts(cp_gironde)]
#
# # Testing for a change in the slope with a linear model
# lm_model_gironde <- lm(RESULTAT ~ year, data = data_temp_gironde)
# summary(lm_model_gironde)
# davies.test(lm_model_gironde)
#
#
# ## Testing for a non-monotonic trend
#
# # Mann-Kendall non-monotonic trend test with autocorrelation correction
# mk_gironde <- modifiedmk::mmkh(temp_timeseries_gironde)
# # Theil–Sen estimator
# sen_gironde <- trend::sens.slope(temp_timeseries_gironde)
#
#
# ## First and last 10 years period mean differences
# data_temp_gironde_period <- data_temp_gironde |>
#   drop_na(period) |>
#   group_by(year, period) |>
#   summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
# kruskal_pvalue_gironde <- signif(kruskal.test(data_temp_gironde_period$mean_temp,
#                                               data_temp_gironde_period$period)[["p.value"]], digits = 3)


# ---- Map points + result trends ----

fct_results_data_phychemvar <- function(estuary_name,
                                        phychemvar_libelle,
                                        phychemvar_label,
                                        points_lieu_mnemonique,
                                        months,
                                        GPS_box){

data <- fct_cleaned_data_phychemvar(
  estuary_name = estuary_name,
  phychemvar = phychemvar_libelle,
  points_lieu_mnemonique = points_lieu_mnemonique,
  months = months
)

# M&M - Map
ggplot_data_map <- fct_ggplot_phychem_map(data = data,
                                                  phychem_var = phychemvar_label,
                                                  estuary_name = estuary_name,
                                                  GPS_box = GPS_box)
ggsave(ggplot_data_map,
       filename = paste0("inst/mat_meth/maps/PHY_CHEM/", phychemvar_label, "/ggplot_", phychemvar_label, "_", estuary_name, "_map.jpg"))

# Results - Trends
ggplot_trend <- ggplot(data, aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  facet_wrap(vars(haline_zone)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line()
# geom_vline(xintercept = break_year_gironde,
#            linetype = "dashed",
#            colour = "red") +
# labs(
#   title = "Gironde - Salinity and potential breaking point (September-October)",
#   subtitle = paste(
#     "Mann-Kendall p =", round(mk_gironde[["new P-value"]], 5),
#     "| Sen slope =", round(sen_gironde$estimates,3), "-/an"
#   ))

ggsave(ggplot_trend,
       filename = paste0("inst/results/data_phychem/", phychemvar_label, "/ggplot_", phychemvar_label, "_", estuary_name, "_trend.jpg"))

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06.1. Gironde
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot_gironde_map_points_year <- plot_estuary_map(data = data_physico_chem,
                 estuary_name = "Gironde",
                 colour_var = LIEU_MNEMONIQUE) +
  facet_wrap(vars(year))
# ggsave(filename = "inst/mat_meth/maps/PHY_CHEM/ggplot_gironde_map_points_year.jpg",
#        plot = ggplot_gironde_map_points_year, width = 30, height = 30)

GPS_box_gironde <- tribble(
  ~ estuary, ~ haline_zone, ~ min_lon, ~ max_lon, ~ min_lat, ~ max_lat,
  "Gironde", "polyhalin", -1.05, -0.9, 45.48, 45.55,
  "Gironde", "mesohalin", -0.78, -0.65, 45.15, 45.3)

points_lieu_mnemonique_gironde <- fct_points_lieu_mnemonique(estuary_name = "Gironde",
                                                             GPS_box = GPS_box_gironde)

# ---- Temperature ----

fct_results_data_phychemvar(estuary_name = "Gironde",
                            phychemvar_libelle = "Température de l'eau",
                            phychemvar_label = "Temperature",
                            points_lieu_mnemonique = points_lieu_mnemonique_gironde,
                            months = c(9, 10),
                            GPS_box = GPS_box_gironde)

# ---- Salinity ----

fct_results_data_phychemvar(estuary_name = "Gironde",
                            phychemvar_libelle = "Salinité",
                            phychemvar_label = "Salinity",
                            points_lieu_mnemonique = points_lieu_mnemonique_gironde,
                            months = c(9, 10),
                            GPS_box = GPS_box_gironde)

# ---- Nitrogen indicator ----

fct_results_data_phychemvar(estuary_name = "Gironde",
                            phychemvar_libelle = "N_indicator",
                            phychemvar_label = "Nitrogen_indicator",
                            points_lieu_mnemonique = points_lieu_mnemonique_gironde,
                            months = c(9, 10, 11),
                            GPS_box = GPS_box_gironde)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06.2. Loire
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot_loire_map_points_year <- plot_estuary_map(data = data_physico_chem,
                                                 estuary_name = "Loire",
                                                 colour_var = LIEU_MNEMONIQUE) +
  facet_wrap(vars(year), ncol = 4)
# ggsave(filename = "inst/mat_meth/maps/PHY_CHEM/ggplot_loire_map_points_year.jpg",
#        plot = ggplot_loire_map_points_year, width = 40, height = 60, units = "cm")

GPS_box_loire <- tribble(
  ~ estuary, ~ haline_zone, ~ min_lon, ~ max_lon, ~ min_lat, ~ max_lat,
  "Loire", "polyhalin", -2.2, -2.15, 47.26, 47.30,
  "Loire", "mesohalin", -2, -1.87, 47.27, 47.29)

points_lieu_mnemonique_loire <- fct_points_lieu_mnemonique(estuary_name = "Loire",
                                                           GPS_box = GPS_box_loire)

# ---- Temperature ----

fct_results_data_phychemvar(estuary_name = "Loire",
                            phychemvar_libelle = "Température de l'eau",
                            phychemvar_label = "Temperature",
                            points_lieu_mnemonique = points_lieu_mnemonique_loire,
                            months = c(9, 10),
                            GPS_box = GPS_box_loire)

# ---- Salinity ----

fct_results_data_phychemvar(estuary_name = "Loire",
                            phychemvar_libelle = "Salinité",
                            phychemvar_label = "Salinity",
                            points_lieu_mnemonique = points_lieu_mnemonique_loire,
                            months = c(9, 10),
                            GPS_box = GPS_box_loire)

# ---- Nitrogen indicator ----

fct_results_data_phychemvar(estuary_name = "Loire",
                            phychemvar_libelle = "N_indicator",
                            phychemvar_label = "Nitrogen_indicator",
                            points_lieu_mnemonique = points_lieu_mnemonique_loire,
                            months = c(9, 10, 11),
                            GPS_box = GPS_box_loire)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06.3. Seine
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot_seine_map_points_year <- plot_estuary_map(data = data_physico_chem,
                                                 estuary_name = "Seine",
                                                 colour_var = LIEU_MNEMONIQUE) +
  facet_wrap(vars(year), ncol = 4)
ggsave(filename = "inst/mat_meth/maps/PHY_CHEM/ggplot_seine_map_points_year.jpg",
       plot = ggplot_seine_map_points_year, width = 40, height = 60, units = "cm")

GPS_box_seine <- tribble(
  ~ estuary, ~ haline_zone, ~ min_lon, ~ max_lon, ~ min_lat, ~ max_lat,
  "Seine", "polyhalin", 0, 0.2, 49.42, 49.49)

points_lieu_mnemonique_seine <- fct_points_lieu_mnemonique(estuary_name = "Seine",
                                                           GPS_box = GPS_box_seine)

# ---- Temperature ----

fct_results_data_phychemvar(estuary_name = "Seine",
                            phychemvar_libelle = "Température de l'eau",
                            phychemvar_label = "Temperature",
                            points_lieu_mnemonique = points_lieu_mnemonique_seine,
                            months = c(9, 10),
                            GPS_box = GPS_box_seine)

# ---- Salinity ----

fct_results_data_phychemvar(estuary_name = "Seine",
                            phychemvar_libelle = "Salinité",
                            phychemvar_label = "Salinity",
                            points_lieu_mnemonique = points_lieu_mnemonique_seine,
                            months = c(9, 10),
                            GPS_box = GPS_box_seine)

# ---- Nitrogen indicator ----

fct_results_data_phychemvar(estuary_name = "Seine",
                            phychemvar_libelle = "N_indicator",
                            phychemvar_label = "Nitrogen_indicator",
                            points_lieu_mnemonique = points_lieu_mnemonique_seine,
                            months = c(9, 10, 11),
                            GPS_box = GPS_box_seine)

