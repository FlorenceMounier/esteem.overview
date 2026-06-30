# =====================================================
# Preparation script for river flows data
# Datasets:
#  - data_flow.rda
# Author: FM
# Date: 2026-06-30
# =====================================================


# =====================================================
# 00. Packages and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

# raw_data_daily_flow_loire <- read_csv("../HYDROPORTAIL/Daily_flows/MontjeanSurLoire.csv")
# usethis::use_data(raw_data_daily_flow_loire)
data(raw_data_daily_flow_loire)

# raw_data_daily_flow_seine <- read_csv("../HYDROPORTAIL/Daily_flows/Vernon_Seine.csv")
# usethis::use_data(raw_data_daily_flow_seine)
data(raw_data_daily_flow_seine)

# raw_data_monthly_flow_seine <- read_csv("../HYDROPORTAIL/Monthly_mean_flows/H320000101_VernonSeine.csv")
# usethis::use_data(raw_data_monthly_flow_seine)
data(raw_data_monthly_flow_seine)

# raw_data_daily_flow_dordogne <- read_csv("../HYDROPORTAIL/Daily_flows/PessacSurDordogne.csv")
# usethis::use_data(raw_data_daily_flow_dordogne)
data(raw_data_daily_flow_dordogne)

# raw_data_daily_flow_garonne <- read_csv2("../HYDROPORTAIL/Daily_flows/Garonne Tonneins.csv")
# usethis::use_data(raw_data_daily_flow_garonne)
data(raw_data_daily_flow_garonne)

# raw_data_monthly_flow_gironde <- read_csv("../HYDROPORTAIL/Monthly_mean_flows/DebitGirondeEDF_1963-2013.csv")
# usethis::use_data(raw_data_monthly_flow_gironde)
data(raw_data_monthly_flow_gironde)



# =====================================================
# 01. Loire
# =====================================================

# ---- Monthly means from raw daily data (1960-2025) ----

data_flow_loire <- raw_data_daily_flow_loire |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Loire")



# =====================================================
# 02. Seine
# =====================================================

# ---- Monthly means from raw daily data (1990-2025) ----

data_flow_seine_daily <- raw_data_daily_flow_seine |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Seine") |>
  mutate(source = "daily data")

# ---- Monthly means from raw monthly data (1975-2010) ----

data_flow_seine_monthly <- raw_data_monthly_flow_seine |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  rename(RESULTAT = `Valeur (en m³/s)`) |>
  mutate(estuary = "Seine") |>
  mutate(source = "monthly data")

# ---- Join datasets ----

data_flow_seine_raw <- data_flow_seine_daily |>
  full_join(data_flow_seine_monthly) |>
  select(year, month, year_month, season, year_season, RESULTAT,
         estuary, source)

# ---- Compare common months from daily and monthly raw data ----
# Percentage of the absolute difference from the mean values

data_flow_seine_raw  |>
  group_by(year_month)  |>
  filter(n() > 1)  |>
  ungroup() |>
  pivot_wider(names_from = source, values_from = RESULTAT) |>
  mutate(mean_RESULTAT = (`daily data` + `monthly data`) / 2) |>
  mutate(DIFF = abs(`daily data` - `monthly data`) / mean_RESULTAT * 100) |>
  summarise(
    stat_DIFF_5 = stats::quantile(DIFF, probs = 0.05) |>  round(digits = 2),
    stat_DIFF_25 = stats::quantile(DIFF, probs = 0.25) |>  round(digits = 2),
    stat_DIFF_50 = stats::quantile(DIFF, probs = 0.50) |>  round(digits = 2),
    stat_DIFF_75 = stats::quantile(DIFF, probs = 0.75) |>  round(digits = 2),
    stat_DIFF_95 = stats::quantile(DIFF, probs = 0.95) |>  round(digits = 2)
  )

# ---- Compute final data using mean(RESULTAT) from `daily data` and `monthly data`

data_flow_seine <- data_flow_seine_raw |>
  group_by(year, month, year_month, season, year_season, estuary) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")

# ---- Check absence of duplicates ----
data_flow_seine  |>
  group_by(year_month)  |>
  filter(n() > 1)  |>
  ungroup()



# =====================================================
# 03. Gironde
# =====================================================

# ---- Dordogne - Monthly means from raw daily data (1996-2024) ----

data_flow_dordogne_daily <- raw_data_daily_flow_dordogne  |>
  select(`Date (TU)`, `Valeur (en m³/s)`) |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`, na.rm = TRUE), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Dordogne")

# ---- Garonne - Monthly means from raw daily data (1985-2025) ----

data_flow_garonne_daily <- raw_data_daily_flow_garonne |>
  select(Date, Valeur_m3_per_s) |>
  mutate(Valeur_m3_per_s = as.numeric(Valeur_m3_per_s)) |>
  get_info_from_dates(date_variable = Date) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(Valeur_m3_per_s, na.rm = TRUE), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Garonne")

# ---- Gironde - Monthly means from raw daily data (1996-2024) ----

data_flow_gironde_daily <- data_flow_dordogne_daily |>
  full_join(data_flow_garonne_daily) |>
  pivot_wider(names_from = zone, values_from = RESULTAT) |>
  mutate(RESULTAT = Dordogne + Garonne) |>
  drop_na() |>
  select(-c(Garonne, Dordogne)) |>
  mutate(estuary = "Gironde") |>
  mutate(source = "daily data")

# ---- Gironde - Monthly means from raw monthly data (1960-2015) ----

data_flow_gironde_monthly <- raw_data_monthly_flow_gironde |>
  rename(year = Annee,
         month = mois) |>
  mutate(year_month = str_c(year, month, sep = "_")) |>
  mutate(future_date = lubridate::ym(year_month)) |>
  get_info_from_dates(date_variable = future_date) |>
  select(-c(MoyMensGaronne, MoyMensDordogne)) |>
  rename(RESULTAT = `MoyMensDébit Total`) |>
  drop_na() |>
  mutate(estuary = "Gironde") |>
  mutate(source = "monthly data") |>
  select(-future_date)

# ---- Join datasets ----

data_flow_gironde_raw <- full_join(data_flow_gironde_daily,
                               data_flow_gironde_monthly)

# ---- Compare common months from daily and monthly raw data ----
# Percentage of the absolute difference from the mean values

data_flow_gironde_raw  |>
  group_by(year_month)  |>
  filter(n() > 1)  |>
  ungroup() |>
  pivot_wider(names_from = source, values_from = RESULTAT) |>
  mutate(mean_RESULTAT = (`daily data` + `monthly data`) / 2) |>
  mutate(DIFF = abs(`daily data` - `monthly data`) / mean_RESULTAT * 100) |>
  summarise(
    stat_DIFF_5 = stats::quantile(DIFF, probs = 0.05) |>  round(digits = 2),
    stat_DIFF_25 = stats::quantile(DIFF, probs = 0.25) |>  round(digits = 2),
    stat_DIFF_50 = stats::quantile(DIFF, probs = 0.50) |>  round(digits = 2),
    stat_DIFF_75 = stats::quantile(DIFF, probs = 0.75) |>  round(digits = 2),
    stat_DIFF_95 = stats::quantile(DIFF, probs = 0.95) |>  round(digits = 2),
    )

# ---- Compute final data using mean(RESULTAT) from `daily data` and `monthly data`

data_flow_gironde <- data_flow_gironde_raw |>
  group_by(year, month, year_month, season, year_season, estuary) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")

# ---- Check absence of duplicates ----

data_flow_gironde  |>
  group_by(year_month)  |>
  filter(n() > 1)  |>
  ungroup()


# =====================================================
# 04. All estuaries (1975-2024)
# =====================================================

data_flow <- data_flow_gironde |>
  full_join(data_flow_loire) |>
  full_join(data_flow_seine) |>
  mutate(year_month = lubridate::ym(year_month)) |>
  mutate(PARAMETRE_LIBELLE = "Flow") |>
  mutate(PROGRAMME = "Hydroportail") |>
  filter(year >= 1975 & year <= 2014)

usethis::use_data(data_flow, overwrite = TRUE)
