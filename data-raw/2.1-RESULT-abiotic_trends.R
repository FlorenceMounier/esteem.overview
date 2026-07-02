# =====================================================
# Preparation script
# Datasets:
#  -
# Plots:
#  - ggplot_temperature.jpg in /int/mat_meth/phychem
#  - ggplot_temperature_map.jpg in /int/mat_meth/phychem
#  - ggplot_salinity.jpg in /int/mat_meth/phychem
#  - ggplot_salinity_map.jpg in /int/mat_meth/phychem
#  - ggplot_O2sat.jpg in /int/mat_meth/phychem
#  - ggplot_O2sat_map.jpg in /int/mat_meth/phychem
#  - ggplot_ammonium.jpg in /int/mat_meth/phychem
#  - ggplot_ammonium_map.jpg in /int/mat_meth/phychem
#  - ggplot_risk_NH4_temp.jpg in /int/mat_meth/phychem
#  - ggplot_risk_NH4_temp_map.jpg in /int/mat_meth/phychem
#  - ggplot_hydro_stress.jpg in /int/mat_meth/phychem
#  - ggplot_hydro_stress_map.jpg in /int/mat_meth/phychem
# Author: FM
# Date: 2026-07-02
# =====================================================

# =====================================================
# 00. Packages and data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
`%!in%` = Negate(`%in%`)

# data from Quadrige/Sextant & POMET
data(data_abiotic)
data(data_contamination)


# =====================================================
# 01. Abiotic parameters trends
# =====================================================

# ---- GAM results ----
gam_results_abiotic <- run_gam_by_parameter(
  data = data_abiotic,
  response_col = "RESULTAT",
  group_vars = c("estuary", "haline_zone", "season"),
  all_group_combinations = FALSE
)

# ---- Plots ----
plots_results_abiotic <- plot_trends_by_parameter(
  data = data_abiotic,
  gam_results = gam_results_abiotic,
  response_col = "RESULTAT",
  x_col = "year_month"
)
# "Ammonium", "Flow", "O2sat", "Salinity", "Surface area", "Temperature",
# "hydro_stress", "risk_NH4_temp"

# Surface area
ggplot_surface  = plots_results_abiotic[["Surface area"]] +
  theme(axis.title.x = element_blank())
ggsave(ggplot_surface,
       filename = "inst/results/long_term_trends/abiotic/ggplot_surface.jpg")

# Flow
ggplot_flow  = plots_results_abiotic[["Flow"]] +
  theme(axis.title.x = element_blank())
ggsave(ggplot_flow,
       filename = "inst/results/long_term_trends/abiotic/ggplot_flow.jpg")

# Temperature (Optimum seabass & common sole: 24°C)
ggplot_temperature = plots_results_abiotic[["Temperature"]] +
  geom_hline(yintercept = 24) +
  theme(axis.title.x = element_blank())
ggsave(ggplot_temperature,
       filename = "inst/results/long_term_trends/abiotic/ggplot_temperature.jpg")

# Salinity
ggplot_salinity  = plots_results_abiotic[["Salinity"]] +
  labs(y = "Salinity") +
  theme(axis.title.x = element_blank())
ggsave(ggplot_salinity,
       filename = "inst/results/long_term_trends/abiotic/ggplot_salinity.jpg")

# O2sat
ggplot_O2sat  = plots_results_abiotic[["O2sat"]] +
  labs(y = "Dissolved oxygen saturation (%)") +
  theme(axis.title.x = element_blank())
ggsave(ggplot_O2sat,
       filename = "inst/results/long_term_trends/abiotic/ggplot_O2sat.jpg")

# risk_NH4_temp
ggplot_risk_NH4_temp  = plots_results_abiotic[["risk_NH4_temp"]] +
  labs(y = "Ammonia formation toxicity indicator") +
  theme(axis.title.x = element_blank())
ggsave(ggplot_risk_NH4_temp,
       filename = "inst/results/long_term_trends/abiotic/ggplot_risk_NH4_temp.jpg")

# hydro_stress
ggplot_hydro_stress  = plots_results_abiotic[["hydro_stress"]] +
  geom_hline(yintercept = 0) +
  labs(y = "Stress indicator") +
  theme(axis.title.x = element_blank())
ggsave(ggplot_hydro_stress,
       filename = "inst/results/long_term_trends/abiotic/ggplot_hydro_stress.jpg")


# =====================================================
# 02. Contamination trends
# =====================================================

# ---- GAM results ----
gam_results_contamination_ng_gdw <- run_gam_by_parameter(
  data = data_contamination,
  response_col = "RESULTAT_ng_gdw",
  group_vars = c("estuary")
)

gam_results_contamination_ng_gdw |>
  filter(p_value < 0.05) |>
  arrange(PARAMETRE_LIBELLE, grouping, p_value) |>
  ungroup() |>
  distinct(PARAMETRE_LIBELLE)

gam_results_contamination_ng_gww <- run_gam_by_parameter(
  data = data_contamination,
  response_col = "RESULTAT_ng_gww",
  group_vars = c("estuary")
)

gam_results_contamination_ng_glw <- run_gam_by_parameter(
  data = data_contamination,
  response_col = "RESULTAT_ng_glw",
  group_vars = c("estuary")
)

# ---- Plots ----

plots_results_contamination_ng_dw <- plot_trends_by_parameter(
  data = data_contamination,
  gam_results = gam_results_contamination,
  response_col = "RESULTAT_ng_gdw",
  x_col = "year"
)

plots_results_contamination_ng_ww <- plot_trends_by_parameter(
  data = data_contamination,
  gam_results = gam_results_contamination,
  response_col = "RESULTAT_ng_gww",
  x_col = "year"
)

plots_results_contamination_ng_lw <- plot_trends_by_parameter(
  data = data_contamination,
  gam_results = gam_results_contamination,
  response_col = "RESULTAT_ng_glw",
  x_col = "year"
)

# "Anthracene", "Benzo-anthr.", "Benzo-peryl.", "Benzo-pyr.",
# "CB 101", "CB 118", "CB 138", "CB 153", "CB 180", "CB 28", "CB 52",
# "Cadmium", "Copper", "DDT total", "Fluoranthene", "Gamma-HCH",
# "Lead", "Mercury", "Naphtalene", "PFOS", "Phenanthrene", "Pyrene",
# "TBT", "Wsum_DLC", "sum_HBCDDs", "sum_PBDEs"

gg_mercury <- plots_results_contamination_ng_ww[["Mercury"]] +
  geom_hline(yintercept = 500)  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

gg_cadmium <- plots_results_contamination_ng_ww[["Cadmium"]] +
  geom_hline(yintercept = 1000)  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")

gg_lead <- plots_results_contamination_ng_ww[["Lead"]] +
  geom_hline(yintercept = 1500)  +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")

gg_copper <- plots_results_contamination_ng_ww[["Copper"]] +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom")


# ---- Cowplot ----
gg_metal <- plot_grid(gg_mercury, gg_cadmium, gg_lead, gg_copper,
                      nrow = 2, ncol = 2, rel_heights = c(0.75, 1))

ggsave(plot = gg_metal,
       filename = "inst/results/long_term_trends/contamination/gg_metal.jpg")


# =====================================================
# 03. Map
# =====================================================

# ---- Temperature ----

plot_maps_parameter_years(
  data = data_abiotic,
  parameter = "Temperature",
  filename = "inst/mat_meth/phychem/ggplot_temperature_map.jpg"
)

# ---- Salinity ----

plot_maps_parameter_years(
  data = data_abiotic,
  parameter = "Salinity",
  filename = "inst/mat_meth/phychem/ggplot_salinity_map.jpg"
)
# ---- O2sat trend ----

plot_maps_parameter_years(
  data = data_physico_chem_filtered,
  parameter = "O2sat",
  filename = "inst/mat_meth/phychem/ggplot_O2sat_map.jpg"
)

# ---- Ammonium ----

plot_maps_parameter_years(
  data = data_physico_chem_filtered,
  parameter = "Ammonium",
  filename = "inst/mat_meth/phychem/ggplot_ammonium_map.jpg"
)

# ---- Ammonia formation toxicity indicator trend ----

plot_maps_parameter_years(
  data = data_abiotic,
  parameter = "risk_NH4_temp",
  filename = "inst/mat_meth/phychem/ggplot_risk_NH4_temp_map.jpg"
)

# ---- Hydrological stress indicator ----

plot_maps_parameter_years(
  data = data_abiotic,
  parameter = "hydro_stress",
  filename = "inst/mat_meth/phychem/ggplot_hydro_stress_map.jpg"
)

############################################################



  geom_hline(yintercept = 0.2, colour = "yellow") +
  geom_hline(yintercept = 0.4, colour = "orange") +
  geom_hline(yintercept = 0.7, colour = "red") +

# =====================================================
# 05. Define the colors of the measurement points
# =====================================================

id_colors <- c("orange", "red", "purple","yellow", "pink","brown", "deeppink" )

usethis::use_data(id_colors, overwrite = TRUE)


# =====================================================
# 07. Filter the sampling point from the GPS boxes of the study areas
# =====================================================

## ---- Filtered data per variable  ----

fct_cleaned_data_phychemvar <- function(estuary_name,
                                        phychemvar,
                                        points_lieu_mnemonique,
                                        months){



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

