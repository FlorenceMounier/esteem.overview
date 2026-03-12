# =====================================================
# Preparation script
# Datasets:
#  - data_nitro.rda
# Plots:
#  - ggplot_nitro_gironde_map.jpg
#  - ggplot_nitro_gironce_trend.jpg
#  - ggplot_nitro_loire_map.jpg
#  - ggplot_nitro_loire_trend.jpg
#  - ggplot_nitro_seine_map.jpg
#  - ggplot_nitro_seine_trend.jpg
# Author: FM
# Date: 2026-03-10
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

options("scipen"=2, "digits"=3) # use exponential notation

# =====================================================
# 00. Prepare nitrogen indicator variable
# =====================================================

# Compute the sum of nitrite + nitrate

data_NO <- data_physchem |>
  filter(PARAMETRE_LIBELLE %in% c("Azote nitreux (nitrite)","Azote nitrique (nitrate)")) |>
  filter(month %in% c(9, 10)) |>
  group_by(estuary, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
           PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE)) |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  summarise(sumNO2NO3 = `Azote nitreux (nitrite)` + `Azote nitrique (nitrate)`, .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sumNO2NO3",
         month = month(DATE)) |>
  rename(RESULTAT = sumNO2NO3) |>
  full_join(data_physchem |>
              filter(PARAMETRE_LIBELLE %in% c("Nitrate + nitrite", "Ammonium")) |>
              filter(month %in% c(9, 10)))


# Compare the computed sum levels with existing sum

ggplot_N0sum_comparison <- data_NO |>
  filter(PARAMETRE_LIBELLE %in% c("sumNO2NO3", "Nitrate + nitrite")) |>
  ggplot() +
  aes(x = year, y = RESULTAT, colour = PARAMETRE_LIBELLE) +
  geom_line() +
  facet_grid(rows = vars(estuary))
ggplot_N0sum_comparison

ggsave(plot = ggplot_N0sum_comparison, filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitrogen_sum_comparison.jpg",width = 15, height = 10, units = "cm")

# Nitrogen cycle indicator

data_nitrogen_cycle <- data_NO |>
  group_by(estuary, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
           PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE)) |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  ungroup() |>
  mutate(N_indicator = case_when(
    is.na(`Nitrate + nitrite`) ~ Ammonium / (Ammonium + sumNO2NO3) ,
    TRUE ~ Ammonium / (Ammonium + `Nitrate + nitrite`)
  )) |>
  pivot_longer(cols = N_indicator, names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT") |>
  select(-c(Ammonium, sumNO2NO3, `Nitrate + nitrite`))

ggplot_data_N_indicator <- ggplot(data_nitrogen_cycle) +
  aes(x = year, y = RESULTAT, colour = estuary) +
  geom_line() +
  geom_hline(yintercept = 0.2, colour = "yellow") +
  geom_hline(yintercept = 0.4, colour = "orange") +
  geom_hline(yintercept = 0.7, colour = "red") +
  facet_grid(vars(estuary), scales = "free_y")
ggplot_data_N_indicator

# ggsave(plot = ggplot_data_N_indicator, filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitrogen_cycle_indicator.jpg",width = 15, height = 10, units = "cm")



# =====================================================
# 01. Gironde
# =====================================================

GPS_box_gironde <- GPS_box |> filter(estuary == "Gironde")

## ---- Filtered data inside GPS box ----

data_nitro_gironde <- data_nitrogen_cycle |>
  filter(PARAMETRE_LIBELLE == "N_indicator") |>
  filter(estuary == "Gironde") |>
  filter(latitude >= GPS_box_gironde$min_lat &
           latitude <= GPS_box_gironde$max_lat &
           longitude >= GPS_box_gironde$min_lon &
           longitude <= GPS_box_gironde$max_lon) |>
  filter(month %in% c(9, 10))


# ---- Filter outliers ----

mean_nitro_gironde <- mean(data_nitro_gironde$RESULTAT, na.rm = TRUE)
sd_nitro_gironde <- sd(data_nitro_gironde$RESULTAT, na.rm = TRUE)

data_nitro_gironde <- data_nitro_gironde |>
  filter(
    between(RESULTAT,
            mean_nitro_gironde - 1.96 * sd_nitro_gironde,
            mean_nitro_gironde + 1.96 * sd_nitro_gironde)
  )


# ----  Assigning an identifier to each measurement point ----

id_gironde <- data_nitro_gironde |>
  distinct(latitude, longitude) |>
  mutate(id = LETTERS[row_number()])

data_nitro_gironde <- data_nitro_gironde |>
  left_join(id_gironde, by = c("latitude", "longitude"))


# ----  Map of measurement points ----

ggplot_nitro_gironde_map <- plot_estuary_map(data = data_nitro_gironde,
                 estuary_name = "Gironde",
                 colour_var = id) +
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
  labs(title = "Gironde - Nitrogen sampling points")

ggsave(ggplot_nitro_gironde_map,
       filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitro_gironde_map.jpg")


# ---- Nitrogen trend ----

data_nitro_gironde <- data_nitro_gironde |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

nitro_timeseries_gironde <- data_nitro_gironde |> pull(RESULTAT)
years_gironde <- data_nitro_gironde |> pull(year)


# Testing for a change in means
cp_gironde <- changepoint::cpt.meanvar(nitro_timeseries_gironde, method = "BinSeg")
plot(cp_gironde)
break_year_gironde <- years_gironde[cpts(cp_gironde)]

# Testing for a change in the slope with a linear model
lm_model_gironde <- lm(RESULTAT ~ year, data = data_nitro_gironde)
summary(lm_model_gironde)
davies.test(lm_model_gironde)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_gironde <- mmkh(nitro_timeseries_gironde)
# Theil–Sen estimator
sen_gironde <- sens.slope(nitro_timeseries_gironde)


## First and last 10 years period mean differences
data_nitro_gironde_period <- data_nitro_gironde |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_gironde <- signif(kruskal.test(data_nitro_gironde_period$mean_temp,
                                      data_nitro_gironde_period$period)[["p.value"]], digits = 3)

## Trend graph

ggplot_nitro_gironde_trend <- ggplot(data_nitro_gironde,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = id)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_gironde,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Gironde - Nitrogen and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", signif(mk_gironde[["new P-value"]], 5),
      "| Sen slope =", signif(sen_gironde$estimates,3), "µmol/L/year"
    ))

ggsave(ggplot_nitro_gironde_trend,
       filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitro_gironde_trend.jpg")


# =====================================================
# 02. Loire
# =====================================================

GPS_box_loire <- GPS_box |> filter(estuary == "Loire")

## ---- Filtered data inside GPS box ----

data_nitro_loire <- data_nitrogen_cycle |>
  filter(PARAMETRE_LIBELLE == "N_indicator") |>
  filter(estuary == "Loire") |>
  filter(latitude >= GPS_box_loire$min_lat &
           latitude <= GPS_box_loire$max_lat &
           longitude >= GPS_box_loire$min_lon &
           longitude <= GPS_box_loire$max_lon) |>
  filter(month %in% c(9, 10))


# ---- Filter outliers ----

mean_nitro_loire <- mean(data_nitro_loire$RESULTAT, na.rm = TRUE)
sd_nitro_loire <- sd(data_nitro_loire$RESULTAT, na.rm = TRUE)

data_nitro_loire <- data_nitro_loire |>
  filter(
    between(RESULTAT,
            mean_nitro_loire - 1.96 * sd_nitro_loire,
            mean_nitro_loire + 1.96 * sd_nitro_loire)
  )


# ----  Assigning an identifier to each measurement point ----

id_loire <- data_nitro_loire |>
  distinct(latitude, longitude) |>
  mutate(id = LETTERS[row_number()])

data_nitro_loire <- data_nitro_loire |>
  left_join(id_loire, by = c("latitude", "longitude"))


# ----  Map of measurement points ----

ggplot_nitro_loire_map <- plot_estuary_map(data = data_nitro_loire,
                                            estuary_name = "Loire",
                                            colour_var = id) +
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
  labs(title = "Loire - Nitrogen sampling points")

ggsave(ggplot_nitro_loire_map,
       filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitro_loire_map.jpg")

# ---- Nitrogen trend ----

data_nitro_loire <- data_nitro_loire |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

nitro_timeseries_loire <- data_nitro_loire |> pull(RESULTAT)
years_loire <- data_nitro_loire |> pull(year)

# Testing for a change in means
cp_loire <- changepoint::cpt.meanvar(nitro_timeseries_loire, method = "BinSeg")
plot(cp_loire)
break_year_loire <- years_loire[cpts(cp_loire)]

# Testing for a change in the slope with a linear model
lm_model_loire <- lm(RESULTAT ~ year, data = data_nitro_loire)
summary(lm_model_loire)
davies.test(lm_model_loire)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_loire <- mmkh(nitro_timeseries_loire)
# Theil–Sen estimator
sen_loire <- sens.slope(nitro_timeseries_loire)


## First and last 10 years period mean differences
data_nitro_loire_period <- data_nitro_loire |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_loire <- signif(kruskal.test(data_nitro_loire_period$mean_temp,
                                      data_nitro_loire_period$period)[["p.value"]], digits = 3)


## Trend graph
ggplot_nitro_loire_trend <- ggplot(data_nitro_loire,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = id)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_loire,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Loire - Nitrogen and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", signif(mk_loire[["new P-value"]], 5),
      "| Sen slope =", signif(sen_loire$estimates,3), "µmol/L/year"
    ))

ggsave(ggplot_nitro_loire_trend,
       filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitro_loire_trend.jpg")


# =====================================================
# 03. Seine
# =====================================================

GPS_box_seine <- GPS_box |> filter(estuary == "Seine")

## ---- Filtered data inside GPS box ----

data_nitro_seine <- data_nitrogen_cycle |>
  filter(PARAMETRE_LIBELLE == "N_indicator") |>
  filter(estuary == "Seine") |>
  filter(latitude >= GPS_box_seine$min_lat &
           latitude <= GPS_box_seine$max_lat &
           longitude >= GPS_box_seine$min_lon &
           longitude <= GPS_box_seine$max_lon) |>
  filter(month %in% c(9, 10))


# ---- Filter outliers ----

mean_nitro_seine <- mean(data_nitro_seine$RESULTAT, na.rm = TRUE)
sd_nitro_seine <- sd(data_nitro_seine$RESULTAT, na.rm = TRUE)

data_nitro_seine <- data_nitro_seine |>
  filter(
    between(RESULTAT,
            mean_nitro_seine - 1.96 * sd_nitro_seine,
            mean_nitro_seine + 1.96 * sd_nitro_seine)
  )


# ----  Assigning an identifier to each measurement point ----

id_seine <- data_nitro_seine |>
  distinct(latitude, longitude) |>
  filter(latitude != 49.5 & longitude != 0.15) |>  # point in the harbour
  mutate(id = LETTERS[row_number()])

data_nitro_seine <- data_nitro_seine |>
  left_join(id_seine, by = c("latitude", "longitude")) |>
  drop_na(id)


# ----  Map of measurement points ----

ggplot_nitro_seine_map <- plot_estuary_map(data = data_nitro_seine,
                                            estuary_name = "Seine",
                                            colour_var = id) +
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
  labs(title = "Seine - Nitrogen sampling points")

ggsave(ggplot_nitro_seine_map,
       filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitro_seine_map.jpg")


# ---- Nitrogen trend ----

data_nitro_seine <- data_nitro_seine |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

nitro_timeseries_seine <- data_nitro_seine |> pull(RESULTAT)
years_seine <- data_nitro_seine |> pull(year)

# Testing for a change in means
cp_seine <- changepoint::cpt.meanvar(nitro_timeseries_seine, method = "BinSeg")
plot(cp_seine)
break_year_seine <- years_seine[cpts(cp_seine)]

# Testing for a change in the slope with a linear model
lm_model_seine <- lm(RESULTAT ~ year, data = data_nitro_seine)
summary(lm_model_seine)
davies.test(lm_model_seine)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_seine <- mmkh(nitro_timeseries_seine)
# Theil–Sen estimator
sen_seine <- sens.slope(nitro_timeseries_seine)

## First and last 10 years period mean differences
data_nitro_seine_period <- data_nitro_seine |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_seine <- signif(kruskal.test(data_nitro_seine_period$mean_temp,
                                      data_nitro_seine_period$period)[["p.value"]], digits = 3)


## Trend graph
ggplot_nitro_seine_trend <- ggplot(data_nitro_seine,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = id)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_seine,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Seine - Nitrogen and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", signif(mk_seine[["new P-value"]], 5),
      "| Sen slope =", signif(sen_seine$estimates,3), "µmol/L/year"
    ))

ggsave(ggplot_nitro_seine_trend,
       filename = "inst/results/data_physico_chemistry/nitrogen/ggplot_nitro_seine_trend.jpg")


# =====================================================
# 04. Join and save datasets
# =====================================================

data_nitro <- data_nitro_gironde |>
  full_join(data_nitro_loire) |>
  full_join(data_nitro_seine)

usethis::use_data(data_nitro, overwrite = TRUE)
