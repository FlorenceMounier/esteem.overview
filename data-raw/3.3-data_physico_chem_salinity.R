# =====================================================
# Preparation script
# Datasets:
#  - data_salin.rda
# Plots:
#  - ggplot_salin_gironde_map.jpg
#  - ggplot_salin_gironce_trend.jpg
#  - ggplot_salin_loire_map.jpg
#  - ggplot_salin_loire_trend.jpg
#  - ggplot_salin_seine_map.jpg
#  - ggplot_salin_seine_trend.jpg
# Author: FM
# Date: 2026-03-09
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

data_salin_gironde <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Salinité") |>
  filter(estuary == "Gironde") |>
  filter(latitude >= GPS_box_gironde$min_lat &
           latitude <= GPS_box_gironde$max_lat &
           longitude >= GPS_box_gironde$min_lon &
           longitude <= GPS_box_gironde$max_lon) |>
  filter(month %in% c(9, 10)) |>
  filter(year >= 1987)


# ---- Filter outliers ----

mean_salin_gironde <- mean(data_salin_gironde$RESULTAT, na.rm = TRUE)
sd_salin_gironde <- sd(data_salin_gironde$RESULTAT, na.rm = TRUE)

data_salin_gironde <- data_salin_gironde |>
  filter(
    between(RESULTAT,
            mean_salin_gironde - 1.96 * sd_salin_gironde,
            mean_salin_gironde + 1.96 * sd_salin_gironde)
  )

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

program_lieu_gironde <- data_salin_gironde |>
  distinct(year, LIEU_LIBELLE, PROGRAMME)

# ---- Filter redondant points ----

data_salin_gironde <- data_salin_gironde |>
  filter(PROGRAMME != "REPOMO")

# ----  Map of measurement points ----

ggplot_salin_gironde_map <- plot_estuary_map(data = data_salin_gironde,
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
  labs(title = "Gironde - Salinity sampling points")

ggsave(ggplot_salin_gironde_map,
       filename = "inst/results/data_phychem/salinity/ggplot_salin_gironde_map.jpg")


# ---- Salinity trend ----

data_salin_gironde <- data_salin_gironde |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

salin_timeseries_gironde <- data_salin_gironde |> pull(RESULTAT)
years_gironde <- data_salin_gironde |> pull(year)


# Testing for a change in means
cp_gironde <- changepoint::cpt.meanvar(salin_timeseries_gironde, method = "BinSeg")
plot(cp_gironde)
break_year_gironde <- years_gironde[cpts(cp_gironde)]

# Testing for a change in the slope with a linear model
lm_model_gironde <- lm(RESULTAT ~ year, data = data_salin_gironde)
summary(lm_model_gironde)
davies.test(lm_model_gironde)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_gironde <- mmkh(salin_timeseries_gironde)
# Theil–Sen estimator
sen_gironde <- sens.slope(salin_timeseries_gironde)


## First and last 10 years period mean differences
data_salin_gironde_period <- data_salin_gironde |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_gironde <- signif(kruskal.test(data_salin_gironde_period$mean_temp,
                                      data_salin_gironde_period$period)[["p.value"]], digits = 3)

## Trend graph

ggplot_salin_gironde_trend <- ggplot(data_salin_gironde,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_gironde,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Gironde - Salinity and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_gironde[["new P-value"]], 5),
      "| Sen slope =", round(sen_gironde$estimates,3), "-/an"
    ))

ggsave(ggplot_salin_gironde_trend,
       filename = "inst/results/data_phychem/salinity/ggplot_salin_gironde_trend.jpg")


# =====================================================
# 02. Loire
# =====================================================

GPS_box_loire <- GPS_box |> filter(estuary == "Loire")

## ---- Filtered data inside GPS box ----

data_salin_loire <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Salinité") |>
  filter(estuary == "Loire") |>
  filter(latitude >= GPS_box_loire$min_lat &
           latitude <= GPS_box_loire$max_lat &
           longitude >= GPS_box_loire$min_lon &
           longitude <= GPS_box_loire$max_lon) |>
  filter(month %in% c(9, 10))  |>
  filter(year >= 1987)


# ---- Filter outliers ----

mean_salin_loire <- mean(data_salin_loire$RESULTAT, na.rm = TRUE)
sd_salin_loire <- sd(data_salin_loire$RESULTAT, na.rm = TRUE)

data_salin_loire <- data_salin_loire |>
  filter(
    between(RESULTAT,
            mean_salin_loire - 1.96 * sd_salin_loire,
            mean_salin_loire + 1.96 * sd_salin_loire)
  )

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

program_lieu_loire <- data_salin_loire |>
  distinct(year, LIEU_LIBELLE, PROGRAMME)

# ---- Filter redondant points ----

data_temp_loire <- data_temp_loire |>
  filter(LIEU_MNEMONIQUE != "070-P-035")

# ----  Map of measurement points ----

ggplot_salin_loire_map <- plot_estuary_map(data = data_salin_loire,
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
  labs(title = "Loire - Salinity sampling points")

ggsave(ggplot_salin_loire_map,
       filename = "inst/results/data_phychem/salinity/ggplot_salin_loire_map.jpg")

# ---- Salinity trend ----

data_salin_loire <- data_salin_loire |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

salin_timeseries_loire <- data_salin_loire |> pull(RESULTAT)
years_loire <- data_salin_loire |> pull(year)

# Testing for a change in means
cp_loire <- changepoint::cpt.meanvar(salin_timeseries_loire, method = "BinSeg")
plot(cp_loire)
break_year_loire <- years_loire[cpts(cp_loire)]

# Testing for a change in the slope with a linear model
lm_model_loire <- lm(RESULTAT ~ year, data = data_salin_loire)
summary(lm_model_loire)
davies.test(lm_model_loire)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_loire <- mmkh(salin_timeseries_loire)
# Theil–Sen estimator
sen_loire <- sens.slope(salin_timeseries_loire)


## First and last 10 years period mean differences
data_salin_loire_period <- data_salin_loire |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_loire <- signif(kruskal.test(data_salin_loire_period$mean_temp,
                                      data_salin_loire_period$period)[["p.value"]], digits = 3)


## Trend graph
ggplot_salin_loire_trend <- ggplot(data_salin_loire,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_loire,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Loire - Salinity and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_loire[["new P-value"]], 5),
      "| Sen slope =", round(sen_loire$estimates,3), "-/an"
    ))

ggsave(ggplot_salin_loire_trend,
       filename = "inst/results/data_phychem/salinity/ggplot_salin_loire_trend.jpg")


# =====================================================
# 03. Seine
# =====================================================

GPS_box_seine <- GPS_box |> filter(estuary == "Seine")

## ---- Filtered data inside GPS box ----

data_salin_seine <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Salinité") |>
  filter(estuary == "Seine") |>
  filter(latitude >= GPS_box_seine$min_lat &
           latitude <= GPS_box_seine$max_lat &
           longitude >= GPS_box_seine$min_lon &
           longitude <= GPS_box_seine$max_lon) |>
  filter(month %in% c(9, 10))  |>
  filter(year >= 1987)


# ---- Filter outliers ----

mean_salin_seine <- mean(data_salin_seine$RESULTAT, na.rm = TRUE)
sd_salin_seine <- sd(data_salin_seine$RESULTAT, na.rm = TRUE)

data_salin_seine <- data_salin_seine |>
  filter(
    between(RESULTAT,
            mean_salin_seine - 1.96 * sd_salin_seine,
            mean_salin_seine + 1.96 * sd_salin_seine)
  )

# ---- Identify PROGRAMME and LIEU_MNEMONIQUE ----

program_lieu_seine <- data_salin_seine |>
  distinct(year, LIEU_LIBELLE, PROGRAMME)

# ---- Filter redondant points ----

data_salin_seine <- data_salin_seine |>
  filter(PROGRAMME != "REPOMO")


# ----  Map of measurement points ----

ggplot_salin_seine_map <- plot_estuary_map(data = data_salin_seine,
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
  labs(title = "Seine - Salinity sampling points")

ggsave(ggplot_salin_seine_map,
       filename = "inst/results/data_phychem/salinity/ggplot_salin_seine_map.jpg")


# ---- Salinity trend ----

data_salin_seine <- data_salin_seine |>
  complete(year) |>
  mutate(period = case_when(
    year >= min(year) & year <= min(year)+9 ~ "first period",
    year >= max(year)-9 & year <= max(year) ~ "last period",
  ))

## Time series segmentation

salin_timeseries_seine <- data_salin_seine |> pull(RESULTAT)
years_seine <- data_salin_seine |> pull(year)

# Testing for a change in means
cp_seine <- changepoint::cpt.meanvar(salin_timeseries_seine, method = "BinSeg")
plot(cp_seine)
break_year_seine <- years_seine[cpts(cp_seine)]

# Testing for a change in the slope with a linear model
lm_model_seine <- lm(RESULTAT ~ year, data = data_salin_seine)
summary(lm_model_seine)
davies.test(lm_model_seine)


## Testing for a non-monotonic trend

# Mann-Kendall non-monotonic trend test with autocorrelation correction
mk_seine <- mmkh(salin_timeseries_seine)
# Theil–Sen estimator
sen_seine <- sens.slope(salin_timeseries_seine)

## First and last 10 years period mean differences
data_salin_seine_period <- data_salin_seine |>
  drop_na(period) |>
  group_by(year, period) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE))
kruskal_pvalue_seine <- signif(kruskal.test(data_salin_seine_period$mean_temp,
                                      data_salin_seine_period$period)[["p.value"]], digits = 3)


## Trend graph
ggplot_salin_seine_trend <- ggplot(data_salin_seine,
                                    aes(x = year, y = RESULTAT)) +
  geom_point(aes(colour = LIEU_MNEMONIQUE)) +
  scale_color_manual(values = id_colors) +
  geom_smooth(color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_seine,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Seine - Salinity and potential breaking point (September-October)",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_seine[["new P-value"]], 5),
      "| Sen slope =", round(sen_seine$estimates,3), "-/an"
    ))

ggsave(ggplot_salin_seine_trend,
       filename = "inst/results/data_phychem/salinity/ggplot_salin_seine_trend.jpg")


# =====================================================
# 04. Join and save datasets
# =====================================================

data_salin <- data_salin_gironde |>
  full_join(data_salin_loire) |>
  full_join(data_salin_seine)

usethis::use_data(data_salin, overwrite = TRUE)
