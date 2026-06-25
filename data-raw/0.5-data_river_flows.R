# =====================================================
# Preparation script
# Datasets:
#  - data_flow.rda
# Author: FM
# Date: 2026-06-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)


# =====================================================
# 01. Loire
# =====================================================

# Daily dataset
data_flow_loire_daily <- read_csv("../DATA débits/Data débits journaliers/MontjeanSurLoire.csv")

# Monthly dataset from daily dataset
data_flow_loire <- data_flow_loire_daily |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Loire")

# =====================================================
# 02. Seine
# =====================================================

# --- DAILY DATASET ----

# Load daily dataset
data_flow_seine_daily_data <- read_csv("../DATA débits/Data débits journaliers/Vernon_Seine.csv")

# Monthly dataset from daily dataset
data_flow_seine_daily_means <- data_flow_seine_daily_data |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Seine")


# ---- MONTHLY DATASET ----

# Load monthly dataset
data_flow_seine_monthly_data <- read_csv("../DATA débits/Data débits mensuels moyens/H320000101_VernonSeine.csv")

data_flow_seine_monthly_means <- data_flow_seine_monthly_data |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Seine")


# ---- FULL DATASET ----

data_flow_seine <- data_flow_seine_daily_means |>
  full_join(data_flow_seine_monthly_means)


# =====================================================
# 03. Gironde
# =====================================================

### MONTHLY MEANS FROM DAILY DATASET 1996 - 2024 ###


# ---- Dordogne daily flow ----

data_flow_dordogne_daily_data <- read_csv("../DATA débits/Data débits journaliers/PessacSurDordogne.csv") |>
  select(`Date (TU)`, `Valeur (en m³/s)`)

# Monthly means from daily dataset
data_flow_dordogne_daily_means <- data_flow_dordogne_daily_data |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`, na.rm = TRUE), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Dordogne")

# ---- Garonne daily flow ----

fun_correct_csv <- function(path, zone_name){
# lire les lignes
lines <- read_lines(path)
# enlever le guillemet début/fin
lines <- str_remove(lines, '^"')
lines <- str_remove(lines, '"$')
# transformer en table
data <- read_csv(
paste(lines, collapse = "\n"),
show_col_types = FALSE
)
data <- data %>%
# enlever guillemets dans les noms de colonnes
rename_with(~ str_remove_all(.x, '"')) %>%
# enlever guillemets restants dans toutes les colonnes texte
mutate(across(where(is.character), ~ str_remove_all(.x, '"')))
# colonne names
names(data) <- c(
"Date",
"Valeur_m3_per_s",
"Statut",
"Qualification",
"Methode",
"Continuite"
)
return(data)
}
# corrupted csv for garonne
data_flow_garonne_daily_data <- fun_correct_csv(path = "../DATA débits/Data débits journaliers/Garonne Tonneins.csv") |>
  select(Date, Valeur_m3_per_s) |>
mutate(Valeur_m3_per_s = as.numeric(Valeur_m3_per_s))

# Monthly means from daily dataset
data_flow_garonne_daily_means <- data_flow_garonne_daily_data |>
  get_info_from_dates(date_variable = Date) |>
  group_by(year, month, year_month, season, year_season) |>
  summarise(RESULTAT = mean(Valeur_m3_per_s, na.rm = TRUE), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Garonne")


# ---- Gironde monthly means flow from daily dataset ----

data_flow_gironde_daily_means <- data_flow_dordogne_daily_means |>
  full_join(data_flow_garonne_daily_means) |>
  pivot_wider(names_from = zone, values_from = RESULTAT) |>
  mutate(RESULTAT = Dordogne + Garonne) |>
  drop_na() |>
  select(-c(Garonne, Dordogne)) |>
  mutate(estuary = "Gironde") |>
  mutate(source = "daily data")


### MONTHLY DATASET ###

data_flow_gironde_monthly_data <- read_delim(
  "../DATA débits/Data débits mensuels moyens/DebitGirondeEDF_1963-2013.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

data_flow_gironde_monthly_means <- data_flow_gironde_monthly_data |>
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

# Join monthly means from daily and monthly means data
data_flow_gironde_raw <- full_join(data_flow_gironde_daily_means,
                               data_flow_gironde_monthly_means)

# Compare months with monthly means computed from daily and monthly data
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

# Compute final data using mean(RESULTAT) from `daily data` and `monthly data`
data_flow_gironde <- data_flow_gironde_raw |>
  group_by(year, month, year_month, season, year_season, estuary) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")

# Check the absence of duplicates
data_flow_gironde  |>
  group_by(year_month)  |>
  filter(n() > 1)  |>
  ungroup()


# =====================================================
# 04. All estuaries
# =====================================================

data_flow <- data_flow_gironde |>
  full_join(data_flow_loire) |>
  full_join(data_flow_seine) |>
  mutate(year_month = lubridate::ym(year_month)) |>
  mutate(PARAMETRE_LIBELLE = "Flow")


# Add haline zone
data_flow_outer <- data_flow |>
  mutate(haline_zone = "outer")

data_flow_inner <- data_flow |>
  mutate(haline_zone = "inner")

data_flow <- rbind(data_flow_outer, data_flow_inner)

usethis::use_data(data_flow, overwrite = TRUE)
