# =====================================================
# Preparation script
# Datasets:
#  - data_physchem.rda
#  - id_colors.rda
# Plots:
#  - ggplot_Temperature_Gironde_map.jpg
#  - ggplot_Temperature_Gironce_trend.jpg
#  - ggplot_Temperature_Loire_map.jpg
#  - ggplot_Temperature_Loire_trend.jpg
#  - ggplot_Temperature_Seine_map.jpg
#  - ggplot_Temperature_Seine_trend.jpg
# Author: FM
# Date: 2026-06-15
# =====================================================

# =====================================================
# 00. Packages and data
# =====================================================

library(tidyverse, quietly = TRUE)
library(marelac)
library(cowplot)
`%!in%` = Negate(`%in%`)

# data from Quadrige/Sextant
data(raw_data_physico_chem)

# data from POMET
data(data_POMET_traits)

# =====================================================
# 01. Filter GPS analysis boxes & Summarise by month
# =====================================================

# ---- Data Surval ----

data_physico_chem_filtered <- raw_data_physico_chem |>
  mutate(PARAMETRE_LIBELLE = case_when(
    PARAMETRE_LIBELLE == "Température de l'eau" ~ "Temperature",
    PARAMETRE_LIBELLE == "Salinité" ~ "Salinity",
    TRUE ~ PARAMETRE_LIBELLE
  )) |>
  # Transformation of ml/l to mg/L => O2_mgL <- O2_mLL * 1.429
  mutate(
    RESULTAT = case_when(
      PARAMETRE_LIBELLE == "Oxygène dissous" &
        UNITE == "ml.l-1" ~ RESULTAT * 1.429,
      TRUE ~ RESULTAT
    ),
    UNITE = case_when(
      PARAMETRE_LIBELLE == "Oxygène dissous" &
        UNITE == "ml.l-1" ~ "mg.l-1",
      TRUE ~ UNITE
    )
  ) |>
  # Transformation of mg/L to %saturation => %sat = 02obs/ 02sat * 100 with:
  # O2obs = observed concentration (mg/L)
  # 02sat = maximum theoretical concentration at equilibrium with the atmosphere (mg/L),
  #         which depends on temperature and salinity
  #         computed using gas_O2sat() function from {marelac} package
  group_by(estuary, PROGRAMME, DATE, PARAMETRE_LIBELLE, RESULTAT, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT, values_fn = ~ mean(.x, na.rm = TRUE)) |>
  mutate(
    O2sat = NA_real_
  ) |>
  mutate(
    O2sat = replace(
      O2sat,
      !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature),
      100 * as.numeric(`Oxygène dissous`[
        !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature)
      ]) /
        marelac::gas_O2sat(
          S = as.numeric(Salinity[
            !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature)
          ]),
          t = as.numeric(Temperature[
            !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature)
          ])
        )
    )
  ) |>
  select(- `Oxygène dissous`) |>
  pivot_longer(cols = c("Temperature", "Salinity", "O2sat", "Turbidité", "Turbidité FNU",
                        "pH", "Ammonium", "Azote nitreux (nitrite)", "Azote nitrique (nitrate)",
                        "Phosphate", "Silicate", "Nitrate + nitrite", "Matière en suspension",
                        "Phéopigments", "Chlorophylle a"),
               names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT") |>
  filter(
    case_when(
      estuary == "Gironde" & haline_zone == "outer" &
        latitude >= 45.48 & latitude < 45.56 & longitude > -1.1 & longitude < -0.9 ~ TRUE,
      estuary == "Gironde" & haline_zone == "inner" &
        latitude >= 45.15 & latitude < 45.30 & longitude > -0.8 & longitude < -0.6 ~ TRUE,
      estuary == "Loire" & haline_zone == "outer" &
        latitude >= 47.26 & latitude < 47.30 & longitude > -2.2 & longitude < -2.15 ~ TRUE,
      estuary == "Loire" & haline_zone == "inner" &
        latitude >= 47.27 & latitude < 47.29 & longitude > -2.0 & longitude < -1.8 ~ TRUE,
      estuary == "Seine" & haline_zone == "outer" &
        latitude >= 49.42 & latitude < 49.49 & longitude > 0 & longitude < 0.2 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  group_by(estuary, PROGRAMME, DATE, PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")


# ---- Data POMET ----

data_POMET_traits_filtered <- data_POMET_traits |>
  mutate(PROGRAMME = "POMET") |>
  mutate(PARAMETRE_LIBELLE = case_when(
    PARAMETRE_LIBELLE == "temperature" ~ "Temperature",
    PARAMETRE_LIBELLE == "salinity" ~ "Salinity",
    TRUE ~ PARAMETRE_LIBELLE
  )) |>
  # Filter GPS analysis boxes
  filter(
    case_when(
      estuary == "Gironde" & haline_zone == "outer" &
        latitude >= 45.48 & latitude < 45.56 & longitude > -1.1 & longitude < -0.9 ~ TRUE,
      estuary == "Gironde" & haline_zone == "inner" &
        latitude >= 45.15 & latitude < 45.30 & longitude > -0.8 & longitude < -0.6 ~ TRUE,
      estuary == "Loire" & haline_zone == "outer" &
        latitude >= 47.26 & latitude < 47.30 & longitude > -2.2 & longitude < -2.15 ~ TRUE,
      estuary == "Loire" & haline_zone == "inner" &
        latitude >= 47.27 & latitude < 47.29 & longitude > -2.0 & longitude < -1.8 ~ TRUE,
      estuary == "Seine" & haline_zone == "outer" &
        latitude >= 49.42 & latitude < 49.49 & longitude > 0 & longitude < 0.2 ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  # Summarise by month
  group_by(estuary, PROGRAMME, DATE, PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")

# ---- Join datasets ----
data_physico_chem_joined <- rbind(data_physico_chem_filtered, data_POMET_traits_filtered)


# =====================================================
# 02. Identify parameters of interest from SEXTANT
# =====================================================

gantt_data <- data_physico_chem_joined |>
  filter(!is.na(RESULTAT)) |>
  filter(!is.na(year)) |>
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

gantt_data <- ggplot(
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
  theme_esteem() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

gantt_data


ggsave(plot = gantt_data, height = 10, width = 17, units = "cm",
       filename = "inst/mat_meth/phychem/ggplot_gantt_data_sextant.jpg")

# =====================================================
# 03. Temperature data completion and localisation
# =====================================================

data_physico_chem_joined |>
  filter(PARAMETRE_LIBELLE %in% c("Temperature")) |>
  ggplot() +
  aes(x = DATE, y = RESULTAT) +
  geom_line() +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone))

# =====================================================
# 04. Salinity data completion and localisation
# =====================================================

data_physico_chem_joined |>
  filter(PARAMETRE_LIBELLE %in% c("Salinity")) |>
  ggplot() +
  aes(x = DATE, y = RESULTAT) +
  geom_line() +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone))


# =====================================================
# 03. Nitrogen cycle indicator variable
# =====================================================

# Compute the sum of nitrite + nitrate

data_physico_chem_joined |>
  filter(PARAMETRE_LIBELLE %in% c("Azote nitreux (nitrite)", "Azote nitrique (nitrate)", "Nitrate + nitrite")) |>
  ggplot() +
  aes(x = DATE, y = RESULTAT, colour = PARAMETRE_LIBELLE) +
  geom_line() +
  facet_grid(rows = vars(estuary))

data_physico_chem_joined |>
  filter(PARAMETRE_LIBELLE %in% c("Ammonium")) |>
  ggplot() +
  aes(x = DATE, y = RESULTAT) +
  geom_line() +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone), scale = "free_y")


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

