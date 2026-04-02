# =====================================================
# Preparation script
# Datasets:
#  - data_temp.rda
# Plots:
#  - ggplot_temp_gironde_map.jpg
#  - ggplot_temp_gironce_trend.jpg
#  - ggplot_temp_loire_map.jpg
#  - ggplot_temp_loire_trend.jpg
#  - ggplot_temp_seine_map.jpg
#  - ggplot_temp_seine_trend.jpg
# Author: FM
# Date: 2026-03-06
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(ggrepel)
library(changepoint)
library(segmented)
library(trend)
library(modifiedmk) # modified Mann-Kendall autocorrelation
library(mgcv) # Generalized Additive Model (GAM) tendance non linéaire

# =====================================================
# 01. Gironde
# =====================================================

GPS_box_gironde <- GPS_box |> filter(estuary == "Gironde")

## ---- Filtered data inside GPS box ----

data_temp_gironde <- data_physico_chem |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(estuary == "Gironde") |>
  filter(latitude >= GPS_box_gironde$min_lat &
           latitude <= GPS_box_gironde$max_lat &
           longitude >= GPS_box_gironde$min_lon &
           longitude <= GPS_box_gironde$max_lon) |>
  filter(month %in% c(9, 10)) |>
  filter(year >= 1987)


# ---- Filter outliers ----

mean_temp_gironde <- mean(data_temp_gironde$RESULTAT, na.rm = TRUE)
sd_temp_gironde <- sd(data_temp_gironde$RESULTAT, na.rm = TRUE)

data_temp_gironde <- data_temp_gironde |>
  filter(
    between(RESULTAT,
            mean_temp_gironde - 1.96 * sd_temp_gironde,
            mean_temp_gironde + 1.96 * sd_temp_gironde)
  )

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

program_lieu_gironde <- data_temp_gironde |>
  distinct(year, LIEU_LIBELLE, PROGRAMME)

# ---- Filter redondant points ----

data_temp_gironde <- data_temp_gironde |>
  filter(PROGRAMME != "REPOMO")


# ----  Map of measurement points ----

ggplot_temp_gironde_map <- plot_estuary_map(data = data_temp_gironde,
                 estuary_name = "Gironde",
                 colour_var = LIEU_MNEMONIQUE) +
  scale_color_manual(values = id_colors) +
  geom_rect(
    data = GPS_box_gironde,
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
  labs(title = "Gironde - Temperature sampling points")

ggsave(ggplot_temp_gironde_map,
       filename = "inst/results/data_phychem/temperature/ggplot_temp_gironde_map.jpg")


# ---- Temperature trend ----

data_temp_gironde <- data_temp_gironde |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

temp_timeseries_gironde <- data_temp_gironde |> pull(RESULTAT)
years_gironde <- data_temp_gironde |> pull(year)

# Testing for a change in means
cp_gironde <- changepoint::cpt.meanvar(temp_timeseries_gironde, method = "BinSeg")
plot(cp_gironde)
break_year_gironde <- years_gironde[cpts(cp_gironde)]

# Testing for a change in the slope with a linear model
lm_model_gironde <- lm(RESULTAT ~ year, data = data_temp_gironde)
summary(lm_model_gironde)
davies.test(lm_model_gironde)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_gironde <- mmkh(temp_timeseries_gironde)
# Theil–Sen estimator
sen_gironde <- sens.slope(temp_timeseries_gironde)


## First and last 10 years period mean differences
data_temp_gironde_period <- data_temp_gironde |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_gironde <- signif(kruskal.test(data_temp_gironde_period$mean_temp,
                                      data_temp_gironde_period$period)[["p.value"]], digits = 3)

## Trend graph

ggplot_temp_gironde_trend <- ggplot(data_temp_gironde,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(, color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_gironde,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Gironde - Temperature and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_gironde[["new P-value"]], 5),
      "| Sen slope =", round(sen_gironde$estimates,3), "°C/an"
    ))

ggsave(filename = "inst/results/data_phychem/temperature/ggplot_temp_gironde_trend.jpg")


# =====================================================
# 02. Loire
# =====================================================

GPS_box_loire <- GPS_box |> filter(estuary == "Loire")

## ---- Filtered data inside GPS box ----

data_temp_loire <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(estuary == "Loire") |>
  filter(latitude >= GPS_box_loire$min_lat &
           latitude <= GPS_box_loire$max_lat &
           longitude >= GPS_box_loire$min_lon &
           longitude <= GPS_box_loire$max_lon) |>
  filter(month %in% c(9, 10)) |>
  filter(year >= 1987)


# ---- Filter outliers ----

mean_temp_loire <- mean(data_temp_loire$RESULTAT, na.rm = TRUE)
sd_temp_loire <- sd(data_temp_loire$RESULTAT, na.rm = TRUE)

data_temp_loire <- data_temp_loire |>
  filter(
    between(RESULTAT,
            mean_temp_loire - 1.96 * sd_temp_loire,
            mean_temp_loire + 1.96 * sd_temp_loire)
  )

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

program_lieu_loire <- data_temp_loire |>
  distinct(year, LIEU_LIBELLE, PROGRAMME)

# ---- Filter redondant points ----

data_temp_loire <- data_temp_loire |>
  filter(LIEU_MNEMONIQUE != "070-P-035")

# ----  Map of measurement points ----

ggplot_temp_loire_map <- plot_estuary_map(data = data_temp_loire,
                                            estuary_name = "Loire",
                                            colour_var = LIEU_MNEMONIQUE) +
  scale_color_manual(values = id_colors) +
  geom_rect(
    data = GPS_box_loire,
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
  labs(title = "Loire - Temperature sampling points")

ggsave(ggplot_temp_loire_map,
       filename = "inst/results/data_phychem/temperature/ggplot_temp_loire_map.jpg")

# ---- Temperature trend ----

data_temp_loire <- data_temp_loire |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

temp_timeseries_loire <- data_temp_loire |> pull(RESULTAT)
years_loire <- data_temp_loire |> pull(year)

# Testing for a change in means
cp_loire <- changepoint::cpt.meanvar(temp_timeseries_loire, method = "BinSeg")
plot(cp_loire)
break_year_loire <- years_loire[cpts(cp_loire)]

# Testing for a change in the slope with a linear model
lm_model_loire <- lm(RESULTAT ~ year, data = data_temp_loire)
summary(lm_model_loire)
davies.test(lm_model_loire)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_loire <- mmkh(temp_timeseries_loire)
# Theil–Sen estimator
sen_loire <- sens.slope(temp_timeseries_loire)


## First and last 10 years period mean differences
data_temp_loire_period <- data_temp_loire |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_loire <- signif(kruskal.test(data_temp_loire_period$mean_temp,
                                      data_temp_loire_period$period)[["p.value"]], digits = 3)


## Trend graph
ggplot_temp_loire_trend <- ggplot(data_temp_loire,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(, color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_loire,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Loire - Temperature and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_loire[["new P-value"]], 5),
      "| Sen slope =", round(sen_loire$estimates,3), "°C/an"
    ))

ggsave(filename = "inst/results/data_phychem/temperature/ggplot_temp_loire_trend.jpg")


# =====================================================
# 03. Seine
# =====================================================

GPS_box_seine <- GPS_box |> filter(estuary == "Seine")

## ---- Filtered data inside GPS box ----

data_temp_seine <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(estuary == "Seine") |>
  filter(latitude >= GPS_box_seine$min_lat &
           latitude <= GPS_box_seine$max_lat &
           longitude >= GPS_box_seine$min_lon &
           longitude <= GPS_box_seine$max_lon) |>
  filter(month %in% c(9, 10)) |>
  filter(year >= 1987)


# ---- Filter outliers ----

mean_temp_seine <- mean(data_temp_seine$RESULTAT, na.rm = TRUE)
sd_temp_seine <- sd(data_temp_seine$RESULTAT, na.rm = TRUE)

data_temp_seine <- data_temp_seine |>
  filter(
    between(RESULTAT,
            mean_temp_seine - 1.96 * sd_temp_seine,
            mean_temp_seine + 1.96 * sd_temp_seine)
  )

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

program_lieu_seine <- data_temp_seine |>
  distinct(year, LIEU_LIBELLE, PROGRAMME)

# ---- Filter redondant points ----

data_temp_seine <- data_temp_seine |>
  filter(LIEU_MNEMONIQUE != "011-P-030") |>
  filter(LIEU_MNEMONIQUE != "011-P-031") |>
  filter(LIEU_MNEMONIQUE != "011-P-032") |>
  filter(LIEU_MNEMONIQUE != "011-P-034") |>
  filter(LIEU_MNEMONIQUE != "011-P-035")

# ----  Map of measurement points ----

ggplot_temp_seine_map <- plot_estuary_map(data = data_temp_seine,
                                            estuary_name = "Seine",
                                            colour_var = LIEU_MNEMONIQUE) +
  scale_color_manual(values = id_colors) +
  geom_rect(
    data = GPS_box_seine,
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
  labs(title = "Seine - Temperature sampling points")

ggsave(ggplot_temp_seine_map,
       filename = "inst/results/data_phychem/temperature/ggplot_temp_seine_map.jpg")


# ---- Temperature trend ----

data_temp_seine <- data_temp_seine |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

temp_timeseries_seine <- data_temp_seine |> pull(RESULTAT)
years_seine <- data_temp_seine |> pull(year)

# Testing for a change in means
cp_seine <- changepoint::cpt.meanvar(temp_timeseries_seine, method = "BinSeg")
plot(cp_seine)
break_year_seine <- years_seine[cpts(cp_seine)]

# Testing for a change in the slope with a linear model
lm_model_seine <- lm(RESULTAT ~ year, data = data_temp_seine)
summary(lm_model_seine)
davies.test(lm_model_seine)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_seine <- mmkh(temp_timeseries_seine)
# Theil–Sen estimator
sen_seine <- sens.slope(temp_timeseries_seine)

## First and last 10 years period mean differences
data_temp_seine_period <- data_temp_seine |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_seine <- signif(kruskal.test(data_temp_seine_period$mean_temp,
                                      data_temp_seine_period$period)[["p.value"]], digits = 3)


## Trend graph
ggplot_temp_seine_trend <- ggplot(data_temp_seine,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(, color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_seine,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Seine - Temperature and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_seine[["new P-value"]], 5),
      "| Sen slope =", round(sen_seine$estimates,3), "°C/an"
    ))

ggsave(filename = "inst/results/data_phychem/temperature/ggplot_temp_seine_trend.jpg")


# =====================================================
# 04. Join and save datasets
# =====================================================

data_temp <- data_temp_gironde |>
  full_join(data_temp_loire) |>
  full_join(data_temp_seine)

usethis::use_data(data_temp, overwrite = TRUE)


#------------------------------------
# Oxygène dissous

data_physchem |>
  filter(PARAMETRE_LIBELLE == "Oxygène dissous") |>
  group_by(estuary, haline_zone, year, month, latitude, longitude) |>
  summarise(res = mean(RESULTAT, na.rm = TRUE)) |>
  arrange(estuary, haline_zone, year) |>
  # filter(latitude == 45.18) |>
  View()

# Gironde mesohalin :
## XXX 1980-2006 : 45.37 / -0.81 (un seul point de mesure sur cette période, trop central)
## 1985-2006 : 45.18 / -0.72
## 2007-2024 (sauf 2009) :  45.25 / -0.73


# data_O2_gironde <- data_physchem |>
#   filter(PARAMETRE_LIBELLE == "Oxygène dissous") |>
#   filter(estuary == "Gironde") |>
#   filter(latitude %in% c(45.18, 45.25, 45.1),
#          longitude %in% c(-0.72, -0.73, -0.68)) |>
#   filter(month %in% c(9, 10)) |>
#   filter(SUPPORT_NIVEAU_PRELEVEMENT != c("Niveau : Fond-sonde-1m")) |>
#   group_by(latitude, longitude, year, PROGRAMME) |>
#   summarise(mean_O2 = mean(RESULTAT, na.rm = TRUE),
#             n = n(), .groups = "drop")
#
# plot_estuary_map(data = data_O2_gironde,
#                  estuary_name = "Gironde")
#
# ggplot(data_O2_gironde) +
#   aes(x = year, y = mean_O2) +
#   geom_point(aes(colour = PROGRAMME)) +
#   geom_smooth() +
#   labs(title = "Oxygène dissous de surface sept-octobre Pauillac Gironde")
#
