# =====================================================
# Datasets:
#
# Plots:
#  - ggplot_temp_loire_map.jpg
# Preparation script
# Author: FM
# Date: 2026-03-05
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(ggrepel)
library(changepoint)
library(trend)
library(modifiedmk) # modified Mann-Kendall autocorrelation
library(mgcv) # Generalized Additive Model (GAM) tendance non linéaire

# =====================================================
# 01. Gironde
# =====================================================

## ---- Code for exploration ----

data_physchem |>
  filter(estuary == "Gironde") |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  distinct(haline_zone, year, latitude, longitude) |>
  arrange(haline_zone, year) |>
  filter(latitude == 45.18) |>
  View("Gironde")


## ---- Recurrent GPS positions ----

# Gironde mesohalin :
## XXX 1980-2006 : 45.37 / -0.81 (un seul point de mesure sur cette période, trop central)
## 1985-2006 : 45.18 / -0.72 => OK
## 2007-2024 (sauf 2009) :  45.25 / -0.73 => OK

# Gironde polyhalin : => trop à l'embouchure
## XXX 1974-1978 + 1997 : latitude 45.58 longitude -1.05
## 1975-1984 : 45.55 / -0.98
## 1985-2024 (sauf 2009) : 45.52 / -0.99


## ---- Filtered data ----

data_temp_gironde <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(estuary == "Gironde") |>
  filter(latitude %in% c(45.18, 45.25),
         longitude %in% c(-0.72, -0.73)) |>
  filter(month %in% c(9, 10)) |>
  filter(SUPPORT_NIVEAU_PRELEVEMENT %in%  c("Niveau : Surface (0-1m)",
                                            "Support : Masse d'eau, eau brute - Niveau : Surface (0-1m)")) |>
  mutate(point = case_when(longitude == -0.72 ~ "A",
                           longitude == -0.73 ~ "B",
                           TRUE ~ NA)) |>
  group_by(year, point, latitude, longitude) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE),
         n = n(), .groups = "drop") |>
  mutate(estuary = "Gironde")


# ---- Map with selected points ----

ggplot_temp_gironde_map <- plot_estuary_map(data = data_temp_gironde,
                 estuary_name = "Gironde", colour = point) +
  geom_text_repel(data = data_temp_gironde |> distinct(point, latitude, longitude),
             aes(x = longitude, y = latitude, label = point, colour = point),
            fontface = "bold") +
  theme(legend.position = "none")

ggsave(filename = "inst/results/data_physico_chemistry/temperature/ggplot_temp_gironde_map.jpg")

# ---- Graph of temperature trend ----

ggplot(data_temp_gironde) +
  aes(x = year, y = mean_temp) +
  geom_point(aes(colour = point)) +
  geom_smooth() +
  labs(title = "Température de surface sept-octobre Pauillac Gironde")



# =====================================================
# 02. Loire
# =====================================================

## ---- Code for exploration ----

data_physchem |>
  filter(estuary == "Loire") |>
  filter(month %in% c(9, 10)) |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(SUPPORT_NIVEAU_PRELEVEMENT %in% c("Niveau : Surface (0-1m)",
                                           "Support : Masse d'eau, eau brute - Niveau : Surface (0-1m)")) |>
  distinct(haline_zone, year, latitude, longitude, SUPPORT_NIVEAU_PRELEVEMENT) |>
  arrange(haline_zone, year) |>
  # filter(latitude == 47.28 & longitude == -1.97) |>
  View("Loire")

## ---- Recurrent GPS positions ----

# Loire mesohalin :
# 1985-2007 : 47.28 / -1.97
# 1992-2024 : 47.28 / -1.90

## ---- Filtered data ----

data_temp_loire <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(estuary == "Loire") |>
  filter(latitude %in% c(47.28, 47.28),
         longitude %in% c(-1.97, -1.90)) |>
  filter(month %in% c(9, 10)) |>
  filter(SUPPORT_NIVEAU_PRELEVEMENT %in% c("Niveau : Surface (0-1m)",
                                           "Support : Masse d'eau, eau brute - Niveau : Surface (0-1m)")) |>
  mutate(point = case_when(longitude == -1.97 ~ "A",
                           longitude == -1.90 ~ "B",
                           TRUE ~ NA)) |>
  group_by(year, point, latitude, longitude) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE),
            n = n(), .groups = "drop") |>
  mutate(estuary = "Loire")


# ---- Map with selected points ----

ggplot_temp_loire_map <- plot_estuary_map(data = data_temp_loire,
                 estuary_name = "Loire", colour = point) +
  geom_text_repel(data = data_temp_loire |> distinct(point, latitude, longitude),
                  aes(x = longitude, y = latitude, label = point, colour = point),
                  fontface = "bold") +
  theme(legend.position = "none")

ggsave(filename = "inst/results/data_physico_chemistry/temperature/ggplot_temp_loire_map.jpg")


# ---- Graph of temperature trend ----

ggplot(data_temp_loire) +
  aes(x = year, y = mean_temp) +
  geom_point(aes(colour = point)) +
  geom_smooth() +
  labs(title = "Température de surface sept-octobre Cordemais Loire")



# =====================================================
# 03. Seine
# =====================================================

## ---- Code for exploration ----

data_physchem |>
  filter(estuary == "Seine") |>
  filter(month %in% c(9, 10)) |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(haline_zone == "polyhalin") |>
  filter(SUPPORT_NIVEAU_PRELEVEMENT %in% c("Niveau : Surface (0-1m)",
                                           "Support : Masse d'eau, eau brute - Niveau : Surface (0-1m)")) |>
  distinct(haline_zone, year, month, latitude, longitude, SUPPORT_NIVEAU_PRELEVEMENT) |>
  arrange(haline_zone, year, month) |>
  filter(latitude == 49.44 & longitude == 0.11) |>
  View("Seine")


## ---- Recurrent GPS positions ----

# Seine mesohalin :
# 1977-2004 : 49.47 / 0.47 => no data after 2004

# Seine polyhalin :
# 1980-2006 : 49.44 / 0.11
# 1974-1980 & 2008-2024 : 49.48 / 0.05


## ---- Filtered data ----

data_temp_seine <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Température de l'eau") |>
  filter(estuary == "Seine") |>
  filter(latitude %in% c(49.48, 49.44),
         longitude %in% c(0.05, 0.11)) |>
  filter(month %in% c(9, 10)) |>
  filter(SUPPORT_NIVEAU_PRELEVEMENT %in% c("Niveau : Surface (0-1m)",
                                           "Support : Masse d'eau, eau brute - Niveau : Surface (0-1m)")) |>
  mutate(point = case_when(latitude == 49.48 ~ "A",
                           latitude == 49.44 ~ "B",
                           TRUE ~ NA)) |>
  group_by(year, month, point, latitude, longitude) |>
  summarise(mean_temp = mean(RESULTAT, na.rm = TRUE),
            n = n(), .groups = "drop") |>
  group_by(year, point, latitude, longitude) |>
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE),
            n = n(), .groups = "drop") |>
  mutate(estuary = "Seine")


# ---- Map with selected points ----

ggplot_temp_seine_map <- plot_estuary_map(data = data_temp_seine,
                 estuary_name = "Seine", colour = point) +
  geom_text_repel(data = data_temp_seine |> distinct(point, latitude, longitude),
                  aes(x = longitude, y = latitude, label = point, colour = point),
                  fontface = "bold") +
  theme(legend.position = "none")

ggsave(filename = "inst/results/data_physico_chemistry/temperature/ggplot_temp_seine_map.jpg")


# ---- Graph of temperature trend ----

ggplot(data_temp_seine) +
  aes(x = year, y = mean_temp) +
  geom_point(aes(colour = point)) +
  geom_smooth() +
  labs(title = "Température de surface sept-octobre embouchure Seine")

# =====================================================
# 04. Join datasets
# =====================================================

data_temp <- full_join(data_temp_gironde, data_temp_loire) |>
  full_join(data_temp_seine)

usethis::use_data(data_temp)

ggplot(data_temp) +
  aes(x = year, y = mean_temp) +
  geom_point(aes(colour = point)) +
  geom_smooth() +
  facet_grid(vars(estuary), scales = "free_y")

# =====================================================
# 05. Trends tests
# =====================================================

# ---- Gironde ----

data_temp_gironde_meanpoint <- data_temp_gironde |>
  group_by(year, point) |>
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") |>
  complete(year)

lm_model_gironde <- lm(mean_temp ~ year, data = data_temp_gironde_meanpoint)
summary(lm_model_gironde)

temp_timeseries_gironde <- data_temp_gironde_meanpoint |> pull(mean_temp)
years_gironde <- data_temp_gironde_meanpoint |> pull(year)

# Package {changepoint}
cp_gironde <- cpt.meanvar(temp_timeseries_gironde, method = "BinSeg")
plot(cp_gironde)
break_year_gironde <- years_gironde[cpts(cp_gironde)] # no breakpoint

# Test de tendance Mann-Kendall
acf(temp_timeseries_gironde) # => pas d'autocorrelation
mk_gironde <- mk.test(temp_timeseries_gironde) # => NS
# Theil–Sen estimator
sen_gironde <- sens.slope(temp_timeseries_gironde)

ggplot_temp_gironde_trend <- ggplot(data_temp_gironde_meanpoint, aes(x = year, y = mean_temp)) +
  geom_point(aes(colour = point)) +
  geom_smooth(method = "lm", color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_gironde,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Gironde - Tendance de la température et potentiel point de rupture",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_gironde$p.value,3),
      "| Sen slope =", round(sen_gironde$estimates,3), "°C/an"
    ))

ggsave(filename = "inst/results/data_physico_chemistry/temperature/ggplot_temp_gironde_trend.jpg")


# ---- Loire ----

data_temp_loire_meanpoint <- data_temp_loire |>
  group_by(year, point) |>
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") |>
  complete(year)

lm_model_loire <- lm(mean_temp ~ year, data = data_temp_loire_meanpoint)
summary(lm_model_loire)

temp_timeseries_loire <- data_temp_loire_meanpoint |> pull(mean_temp)
years_loire <- data_temp_loire_meanpoint |> pull(year)

# Package {changepoint}
cp_loire <- cpt.meanvar(temp_timeseries_loire, method = "BinSeg")
plot(cp_loire)
break_year_loire <- years_loire[cpts(cp_loire)] # => no break point

# Test de tendance Mann-Kendall {trend}
acf(temp_timeseries_loire) # => pas d'autocorrelation
mk_loire <- mk.test(temp_timeseries_loire) # => NS
# Theil–Sen estimator
sen_loire <- sens.slope(temp_timeseries_loire) #  °C/an


ggplot_temp_loire_trend <- ggplot(data_temp_loire_meanpoint, aes(x = year, y = mean_temp)) +
  geom_point(aes(point)) +
  geom_smooth(method = "lm", color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_loire,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Loire - Tendance de la température et potentiel point de rupture",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_loire$p.value,3),
      "| Sen slope =", round(sen_loire$estimates,3), "°C/an"
    ))

ggsave(filename = "inst/results/data_physico_chemistry/temperature/ggplot_temp_loire_trend.jpg")


# ---- Seine ----

data_temp_seine_meanpoint <- data_temp_seine |>
  group_by(year, point) |>
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE), .groups = "drop") |>
  complete(year)

lm_model_seine <- lm(mean_temp ~ year, data = data_temp_seine_meanpoint)
summary(lm_model_seine)

temp_timeseries_seine <- data_temp_seine_meanpoint |>
  pull(mean_temp)
years_seine <- data_temp_seine_meanpoint |>
  pull(year)

# Package {changepoint}
cp_seine <- cpt.meanvar(temp_timeseries_seine, method = "BinSeg")
plot(cp_seine)
break_year_seine <- years[cpts(cp_seine)] # => 1986

# Test de tendance Mann-Kendall {trend}
acf(temp_timeseries_seine) # => pas d'autocorrelation
mk_seine <- mk.test(temp_timeseries_seine)
# Theil–Sen estimator
sen_seine <- sens.slope(temp_timeseries_seine)

ggplot_temp_seine_trend <- ggplot(data_temp_seine_meanpoint, aes(x = year, y = mean_temp)) +
  geom_point(aes(colour = point)) +
  geom_smooth(method = "lm", color = "red") +
  geom_line() +
  geom_vline(xintercept = break_year_seine,
             linetype = "dashed",
             colour = "red") +
  labs(
    title = "Seine - Tendance de la température et potentiel point de rupture",
    subtitle = paste(
      "Mann-Kendall p =", round(mk_seine$p.value,3),
      "| Sen slope =", round(sen_seine$estimates,3), "°C/an"
    ))

ggsave(filename = "inst/results/data_physico_chemistry/temperature/ggplot_temp_seine_trend.jpg")

# Une tendance significative a été détectée avec le test de Mann-Kendall (p = 0.01),
# mais la pente de la régression linéaire n’est pas significative (p = 0.109).
# En l'absence d'autocorrélation détectée, et avec un point de rupture détecté en 1986
# ces résultats suggèrent une évolution non strictement linéaire sur la période.

# =====================================================
# 02. Salinité
# =====================================================

data_physchem_GPS_ronded |>
  filter(PARAMETRE_LIBELLE == "Salinité") |>
  group_by(estuary, haline_zone, year, month, latitude, longitude) |>
  summarise(res = mean(RESULTAT, na.rm = TRUE)) |>
  arrange(estuary, haline_zone, year) |>
  # filter(latitude == 45.18) |>
  View()

data_salin_gironde <- data_physchem_GPS_ronded |>
  filter(PARAMETRE_LIBELLE == "Salinité") |>
  filter(estuary == "Gironde") |>
  filter(latitude %in% c(45.18, 45.25, 45.27),
         longitude %in% c(-0.72, -0.73, -0.75)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(mean_salin = mean(RESULTAT, na.rm = TRUE),
            n = n(), .groups = "drop")

plot_estuary_map(data = data_salin_gironde,
                 estuary_name = "Gironde")

ggplot(data_salin_gironde) +
  aes(x = year, y = mean_salin) +
  geom_point() +
  geom_smooth() +
  labs(title = "Salinité de surface sept-octobre Pauillac Gironde")

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


data_O2_gironde <- data_physchem |>
  filter(PARAMETRE_LIBELLE == "Oxygène dissous") |>
  filter(estuary == "Gironde") |>
  filter(latitude %in% c(45.18, 45.25, 45.1),
         longitude %in% c(-0.72, -0.73, -0.68)) |>
  filter(month %in% c(9, 10)) |>
  filter(SUPPORT_NIVEAU_PRELEVEMENT != c("Niveau : Fond-sonde-1m")) |>
  group_by(latitude, longitude, year, PROGRAMME) |>
  summarise(mean_O2 = mean(RESULTAT, na.rm = TRUE),
            n = n(), .groups = "drop")

plot_estuary_map(data = data_O2_gironde,
                 estuary_name = "Gironde")

ggplot(data_O2_gironde) +
  aes(x = year, y = mean_O2) +
  geom_point(aes(colour = PROGRAMME)) +
  geom_smooth() +
  labs(title = "Oxygène dissous de surface sept-octobre Pauillac Gironde")

