
data_POMET_traits


ggplot_data_N_indicator <- ggplot(data_nitrogen_cycle) +
  aes(x = year, y = RESULTAT, colour = estuary) +
  geom_point() +
  geom_hline(yintercept = 0.2, colour = "yellow") +
  geom_hline(yintercept = 0.4, colour = "orange") +
  geom_hline(yintercept = 0.7, colour = "red") +
  facet_grid(vars(estuary), scales = "free_y") +
  theme_esteem()
# ggplot_data_N_indicator

ggsave(plot = ggplot_data_N_indicator,
       filename = "inst/results/data_phychem/Nitrogen_indicator/ggplot_nitrogen_cycle_indicator.jpg",width = 15, height = 10, units = "cm")

# =====================================================
# 05. Define the colors of the measurement points
# =====================================================

id_colors <- c("orange", "red", "purple","yellow", "pink","brown", "deeppink" )

usethis::use_data(id_colors, overwrite = TRUE)


# =====================================================
# 07. Filter the sampling point from the GPS boxes of the study areas
# =====================================================

data_physico_chem


# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

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

