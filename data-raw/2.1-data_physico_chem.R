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
library(marelac)
library(cowplot)
`%!in%` = Negate(`%in%`)

# =====================================================
# 01.Read raw data
# =====================================================

#  from Quadrige/Sextant
data(raw_data_physico_chem)

# from POMET
data(data_POMET_traits)

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
mutate(year = year(DATE),
       month = month(DATE),
       year_month = paste(year,month, sep = "_")) |>

# ----- Extract seasons -----

mutate(season = case_when(
  month %in% c(12, 1, 2) ~ "winter",
  month %in% c(3,4,5) ~ "spring",
  month %in% c(6,7,8) ~ "summer",
  month %in% c(9,10,11) ~ "autumn"
)) |>
  mutate(year_season = paste(year, season, sep = "_"))

# =====================================================
# 03. Define Haline zone from GPS position & data_POMET salinity
# =====================================================

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
# 04. Identify parameters of interest
# =====================================================

gantt_data <- data_physico_chem  |>
  arrange(estuary, haline_zone, PARAMETRE_LIBELLE, year) |>
  group_by(estuary, haline_zone, PARAMETRE_LIBELLE) |>
  mutate(
    grp = cumsum(c(TRUE, diff(year) > 1))
  ) |>
  group_by(
    estuary,
    haline_zone,
    PARAMETRE_LIBELLE,
    grp
  ) |>
  summarise(
    start_year = min(year),
    end_year   = max(year),
    .groups = "drop"
  )

ggplot(
  gantt_data,
  aes(
    x = start_year,
    xend = end_year,
    y = PARAMETRE_LIBELLE,
    yend = PARAMETRE_LIBELLE
  )
) +
  geom_segment(
    linewidth = 1,
    lineend = "round"
  ) +
  facet_grid(
    haline_zone ~ estuary
  ) +
  theme(axis.title.y = "") +
  theme_bw()



# =====================================================
# 05. Prepare nitrogen indicator variable
# =====================================================

# Compute the sum of nitrite + nitrate

data_NO <- data_physico_chem |>
  filter(PARAMETRE_LIBELLE %in% c("Azote nitreux (nitrite)", "Azote nitrique (nitrate)")) |>
  group_by(estuary, PROGRAMME, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
           PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  group_by(estuary, PROGRAMME, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
             longitude, latitude, haline_zone, year, month) |>
  summarise(sumNO2NO3 = `Azote nitreux (nitrite)` + `Azote nitrique (nitrate)`, .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sumNO2NO3") |>
  rename(RESULTAT = sumNO2NO3) |>
  full_join(data_physico_chem |>
              filter(PARAMETRE_LIBELLE %in% c("Nitrate + nitrite", "Ammonium"))) |>
  mutate(year = year(DATE),
         month = month(DATE),
         year_month = paste(year,month, sep = "_")) |>
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3,4,5) ~ "spring",
    month %in% c(6,7,8) ~ "summer",
    month %in% c(9,10,11) ~ "autumn"
  )) |>
  mutate(year_season = paste(year, season, sep = "_"))

# Compare the computed sum levels with existing sum

ggplot_N0sum_comparison <- data_NO |>
  filter(PARAMETRE_LIBELLE %in% c("sumNO2NO3", "Nitrate + nitrite")) |>
  ggplot() +
  aes(x = year_season, y = RESULTAT, colour = PARAMETRE_LIBELLE) +
  geom_line() +
  facet_grid(rows = vars(estuary))
ggplot_N0sum_comparison

ggsave(plot = ggplot_N0sum_comparison, filename = "inst/mat_meth/phychem/nitro_indicator/ggplot_nitrogen_sum_comparison.jpg",width = 15, height = 10, units = "cm")

# Nitrogen cycle indicator

data_nitrogen_cycle <- data_NO |>
  group_by(estuary, PROGRAMME, LIEU_MNEMONIQUE, DATE, SUPPORT_NIVEAU_PRELEVEMENT, PRELEVEMENT_DESCRIPTION,
           PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  ungroup() |>
  mutate(N_indicator = case_when(
    is.na(`Nitrate + nitrite`) ~ Ammonium / (Ammonium + sumNO2NO3) ,
    TRUE ~ Ammonium / (Ammonium + `Nitrate + nitrite`)
  )) |>
  pivot_longer(cols = N_indicator, names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT") |>
  mutate(year = year(DATE),
         month = month(DATE),
         year_month = paste(year,month, sep = "_")) |>
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "winter",
    month %in% c(3,4,5) ~ "spring",
    month %in% c(6,7,8) ~ "summer",
    month %in% c(9,10,11) ~ "autumn"
  )) |>
  mutate(year_season = paste(year, season, sep = "_"))

# Join tables

data_physico_chem <- full_join(data_physico_chem, data_nitrogen_cycle)

# =====================================================
# 06. Unify oxygen unit
# =====================================================

# Transformation of ml/l to mg/L => O2_mgL <- O2_mLL * 1.429
data_physico_chem <- data_physico_chem |>
  mutate(
    RESULTAT = case_when(
      PARAMETRE_LIBELLE == "Oxygène dissous" &
        UNITE == "ml.l-1" ~ RESULTAT * 1.429,
      TRUE ~ RESULTAT
    ),
    UNITE = case_when(
      PARAMETRE_LIBELLE == "Oxygène dissous" &
        UNITE == "ml.l-1" ~ "mg.l-1"
    )
  )

# Transformation of mg/L to %saturation => %sat = 02obs/ 02sat * 100 with:
# O2obs = observed concentration (mg/L)
# 02sat = maximum theoretical concentration at equilibrium with the atmosphere (mg/L),
#         which depends on temperature and salinity
#         computed using gas_O2sat() function from {marelac} package

data_physico_chem <- data_physico_chem |>
  select(estuary, DATE, year, month, year_month, season, year_season,
         haline_zone, latitude, longitude, LIEU_MNEMONIQUE,
         PROGRAMME, PARAMETRE_LIBELLE, RESULTAT) |>
  filter(PARAMETRE_LIBELLE %in% c("N_indicator", "Oxygène dissous", "Température de l'eau", "Salinité")) |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT, values_fn = ~ mean(.x, na.rm = TRUE)) |>
  drop_na() |>
  mutate(O2sat = 100 * `Oxygène dissous` / gas_O2sat(S = as.numeric(`Salinité`),
                                                     t = as.numeric(`Température de l'eau`))) |>
  select(- `Oxygène dissous`) |>
  pivot_longer(cols = c("N_indicator", "Température de l'eau", "Salinité", "O2sat"),
               names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT")

# =====================================================
# 07. Translate parameters of interest
# =====================================================

data_physico_chem <- data_physico_chem |>
  mutate(
    PARAMETRE_LIBELLE = case_when(
      PARAMETRE_LIBELLE == "Température de l'eau" ~ "Temperature",
      PARAMETRE_LIBELLE == "Salinité" ~ "Salinity",
      PARAMETRE_LIBELLE == "O2sat" ~ "%O2",
      PARAMETRE_LIBELLE == "N_indicator" ~ "Nitrogen"
    )
  )

# ----- Save dataset -----
usethis::use_data(data_physico_chem, overwrite = TRUE)


# =====================================================
# 08. Identify time series completion by estuary, haline zone, season and parameter
# =====================================================

n_data_physico_chem <- data_physico_chem |>
  group_by(PARAMETRE_LIBELLE, estuary, year, season, haline_zone) |>
  summarise(n = n(), .groups = "drop")

n_data_physico_chem_spring <- n_data_physico_chem |> filter(season == "spring")
n_data_physico_chem_summer <- n_data_physico_chem |> filter(season == "summer")
n_data_physico_chem_autumn <- n_data_physico_chem |> filter(season == "autumn")
n_data_physico_chem_winter <- n_data_physico_chem |> filter(season == "winter")

ggplot_n_data_physico_chem <- function(n_data_physico_chem, title_plot){
  n_data_physico_chem |>
    filter(n != 0) |>
  ggplot() +
    aes(
      x = year,
      y = n,
      color = haline_zone

    ) +
    geom_point(size = 0.8) +
    facet_grid(
      PARAMETRE_LIBELLE ~ estuary, scales = "free_y"
    ) +
    labs(title = title_plot, y = "N measurements", x = "") +
    theme(legend.position="none")
}

p1 <- ggplot_n_data_physico_chem(n_data_physico_chem_spring, title_plot = "Spring")
p2 <- ggplot_n_data_physico_chem(n_data_physico_chem_summer, title_plot = "Summer")
p3 <- ggplot_n_data_physico_chem(n_data_physico_chem_autumn, title_plot = "Autumn")
p4 <- ggplot_n_data_physico_chem(n_data_physico_chem_winter, title_plot = "Winter")

ggplot_n_data_physico_chem <- plot_grid(p1, p2, p3, p4, nrow=2)
ggplot_n_data_physico_chem

ggsave(plot = ggplot_n_data_physico_chem,
       filename = "inst/mat_meth/phychem/ggplot_n_data_physicochem.jpg",
       height = 15, width = 30, units = "cm")

