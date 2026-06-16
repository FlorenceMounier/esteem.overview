# =====================================================
# Preparation script
# Datasets:
#  - data_physico_chem_joined.rda
#  - data_physico_chem_filtered.rda
#  - data_physico_chem_complete.rda
# Plots:
#  - ggplot_gantt_data.jpg in /int/mat_meth/phychem
#  - ggplot_n_data_physicochem.jpg in /int/mat_meth/phychem
#  - ggplot_temperature.jpg in /int/mat_meth/phychem
#  - ggplot_temperature_map.jpg in /int/mat_meth/phychem
#  - ggplot_salinity.jpg in /int/mat_meth/phychem
#  - ggplot_salinity_map.jpg in /int/mat_meth/phychem
#  - ggplot_O2sat.jpg in /int/mat_meth/phychem
#  - ggplot_O2sat_map.jpg in /int/mat_meth/phychem
#  - ggplot_ammonium.jpg in /int/mat_meth/phychem
#  - ggplot_ammonium_map.jpg in /int/mat_meth/phychem
#  - ggplot_ammonia_risk_synergy.jpg in /int/mat_meth/phychem
#  - ggplot_ammonia_risk_tile.jpg in /int/mat_meth/phychem
# Author: FM
# Date: 2026-06-16
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

# ---- Data Surval/Sextant ----

data_physico_chem_filtered <- raw_data_physico_chem  |>
  mutate(year_month = lubridate::make_date(year, month, 1)) |>
  mutate(PARAMETRE_LIBELLE = case_when(
    PARAMETRE_LIBELLE == "Température de l'eau" ~ "Temperature",
    PARAMETRE_LIBELLE == "Salinité" ~ "Salinity",
    TRUE ~ PARAMETRE_LIBELLE
  )) |>

  # Transformation of µmol/l to mg/L => mg/L NH4+ = µmol/L × 0.01804
  mutate(
    RESULTAT = case_when(
      PARAMETRE_LIBELLE == "Ammonium" &
        UNITE == "µmol.l-1" ~ RESULTAT * 0.01804,
      TRUE ~ RESULTAT
    ),
    UNITE = case_when(
      PARAMETRE_LIBELLE == "Ammonium" &
        UNITE == "µmol.l-1" ~ "mg.l-1",
      TRUE ~ UNITE
    )
  ) |>

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

  group_by(estuary, PROGRAMME, PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month, year_month, season) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop") |>


  # Transformation of mg/L to %saturation => %sat = 02obs/ 02sat * 100 with:
  # O2obs = observed concentration (mg/L)
  # 02sat = maximum theoretical concentration at equilibrium with the atmosphere (mg/L),
  #         which depends on temperature and salinity
  #         computed using gas_O2sat() function from {marelac} package
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT, values_fn = ~ mean(.x, na.rm = TRUE)) |>
  mutate(
    O2sat = NA_real_
  ) |>
  mutate(
    O2sat = replace(
      O2sat,
      !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature),
      as.numeric(`Oxygène dissous`[
        !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature)
      ]) /
        marelac::gas_O2sat(
          S = as.numeric(Salinity[
            !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature)
          ]),
          t = as.numeric(Temperature[
            !is.na(`Oxygène dissous`) & !is.na(Salinity) & !is.na(Temperature)
          ])
        ) * 100
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
  )


# ---- Data POMET ----

data_POMET_traits_filtered <- data_POMET_traits |>
  mutate(PROGRAMME = "POMET") |>
  mutate(year_month = lubridate::make_date(year, month, 1)) |>
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
  group_by(estuary, PROGRAMME, PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month, year_month, season) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")


# ---- Join datasets ----
data_physico_chem_joined <- rbind(data_physico_chem_filtered, data_POMET_traits_filtered)


# =====================================================
# 02. Identify parameters of interest
# =====================================================

gantt_data <- data_physico_chem_joined |>
  filter(!is.na(RESULTAT)) |>
  filter(!is.na(year)) |>
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
       filename = "inst/mat_meth/phychem/ggplot_gantt_data.jpg")

# ---- Filtered  datasets ----

data_physico_chem_filtered <- data_physico_chem_joined |>
  filter(PARAMETRE_LIBELLE %in% c("Temperature", "Salinity",
                                  "Azote nitreux (nitrite)",
                                  "Azote nitrique (nitrate)",
                                  "Nitrate + nitrite",
                                  "Ammonium",
                                  "O2sat"))


# =====================================================
# 03. Temperature data completion and localisation
# =====================================================

# Optimum seabass & common sole: 24°C

# ---- Trend ----

ggplot_temperature <- data_physico_chem_filtered |>
  filter(PARAMETRE_LIBELLE %in% c("Temperature")) |>
  ggplot() +
  aes(x = year_month, y = RESULTAT, colour = season) +
  geom_line() +
  geom_hline(yintercept = 24) +
  labs(y = "Temperature (°C)") +
  theme(axis.title.x = element_blank()) +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone))

ggplot_temperature

ggsave(ggplot_temperature,
       filename = "inst/mat_meth/phychem/ggplot_temperature.jpg")

# ---- Map ----

plot_maps_parameter_years(
  data = data_physico_chem_filtered,
  parameter = "Temperature",
  filename = "inst/mat_meth/phychem/ggplot_temperature_map.jpg"
)

# =====================================================
# 04. Salinity data completion and localisation
# =====================================================


# ---- Trend ----

ggplot_salinity <- data_physico_chem_filtered |>
  filter(PARAMETRE_LIBELLE %in% c("Salinity")) |>
  ggplot() +
  aes(x = year_month, y = RESULTAT, colour = season) +
  geom_line() +
  # Optimum seabass
  geom_hline(yintercept = 10, color = "blue") +
  geom_hline(yintercept = 25, color = "blue") +
  # Optimum common sole
  geom_hline(yintercept = 20, color = "red") +
  geom_hline(yintercept = 35, color = "red") +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone))

ggplot_salinity

ggsave(ggplot_salinity,
       filename = "inst/mat_meth/phychem/ggplot_salinity.jpg")

# ---- Map ----

plot_maps_parameter_years(
  data = data_physico_chem_filtered,
  parameter = "Salinity",
  filename = "inst/mat_meth/phychem/ggplot_salinity_map.jpg"
)

# =====================================================
# 05. O2sat data completion and localisation
# =====================================================

# ---- Trend ----

ggplot_O2sat <- data_physico_chem_filtered |>
  filter(PARAMETRE_LIBELLE == "O2sat") |>
  ggplot() +
  aes(x = year_month, y = RESULTAT, colour = season) +
  geom_line() +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone))

ggplot_O2sat

ggsave(ggplot_O2sat,
       filename = "inst/mat_meth/phychem/ggplot_O2sat.jpg")

# ---- Map ----

plot_maps_parameter_years(
  data = data_physico_chem_filtered,
  parameter = "O2sat",
  filename = "inst/mat_meth/phychem/ggplot_O2sat_map.jpg"
)

# =====================================================
# 06. Nitrogen cycle variables
# =====================================================

# ---- All nitrogen forms ----
# Shift in Seine due to only one point from 2008 that is offshore
data_physico_chem_filtered |>
  filter(PARAMETRE_LIBELLE %in% c("Azote nitreux (nitrite)", "Azote nitrique (nitrate)", "Nitrate + nitrite")) |>
  ggplot() +
  aes(x = year_month, y = RESULTAT, colour = PARAMETRE_LIBELLE) +
  geom_line() +
  facet_grid(rows = vars(estuary))


# ---- Trend ----

ggplot_ammonium <- data_physico_chem_filtered |>
  filter(PARAMETRE_LIBELLE %in% c("Ammonium")) |>
  ggplot() +
  aes(x = year_month, y = RESULTAT, colour = season) +
  geom_line() +
  geom_hline(yintercept = 0.2) +
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = 1) +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone), scale = "free_y")

ggplot_ammonium

ggsave(ggplot_ammonium,
       filename = "inst/mat_meth/phychem/ggplot_ammonium.jpg")

# ---- Map ----

plot_maps_parameter_years(
  data = data_physico_chem_filtered,
  parameter = "Ammonium",
  filename = "inst/mat_meth/phychem/ggplot_ammonium_map.jpg"
  )

# ---- Ammonia formation risk from ammonium x temperature ----
risk_summary <- data_physico_chem_filtered |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
mutate(
  risque_NH4_temp = case_when(
    Ammonium >= 1 & Temperature >= 20 ~ 3,
    Ammonium >= 0.5 & Temperature >= 20 ~ 2,
    Ammonium >= 0.5 ~ 1,
    TRUE ~ 0
  )
)

ggplot_ammonia_risk_tile <- ggplot(risk_summary |> mutate(risque_NH4_temp = as.factor(risque_NH4_temp)),
       aes(year_month, estuary, fill = risque_NH4_temp)) +
  geom_tile() +
  scale_fill_manual(values = c(
    "0" = "lightblue",
    "1" = "orange",
    "2" = "red",
    "3" = "darkred"
  )) +
  theme_esteem()

ggplot_ammonia_risk_tile

ggsave(ggplot_ammonia_risk_tile,
       filename = "inst/mat_meth/phychem/ggplot_ammonia_risk_tile.jpg")


ggplot_ammonia_risk_synergy <- ggplot(risk_summary |> mutate(risque_NH4_temp = as.factor(risque_NH4_temp)),
       aes(Temperature, Ammonium,
           color = risque_NH4_temp)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ estuary) +
  theme_esteem()

ggplot_ammonia_risk_synergy

ggsave(ggplot_ammonia_risk_synergy,
       filename = "inst/mat_meth/phychem/ggplot_ammonia_risk_synergy.jpg")

# =====================================================
# 07. Filter selected parameters
# =====================================================

data_physico_chem_complete <- risk_summary |>
  select(-c("Azote nitreux (nitrite)", "Azote nitrique (nitrate)","Nitrate + nitrite")) |>
  pivot_longer(cols = c("Temperature", "Salinity", "O2sat", "Ammonium", "risque_NH4_temp"),
               names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT")

# =====================================================
# 08. Synthetic indicator
# =====================================================

# ---- Compute the indicator -----
data_physico_chem_complete_full <- data_physico_chem_complete |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  mutate(
    z_temp = scale(Temperature)[,1],
    z_amm  = scale(Ammonium)[,1],
    z_o2   = scale(O2sat)[,1],
    hydro_stress = z_temp + z_amm - z_o2) |>
  select(-c("z_temp", "z_amm","z_o2")) |>
  pivot_longer(cols = c("Temperature", "Salinity", "O2sat", "Ammonium", "risque_NH4_temp", "hydro_stress"),
               names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT")

usethis::use_data(data_physico_chem_complete_full, overwrite = TRUE)

# ---- Trend ----

ggplot_hydro_stress <- data_physico_chem_complete_full |>
  filter(PARAMETRE_LIBELLE == "hydro_stress") |>
  drop_na(RESULTAT) |>
  ggplot() +
  aes(x = year_month, y = RESULTAT, colour = season) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(estuary), cols = vars(haline_zone), scale = "free_y")

ggplot_hydro_stress

ggsave(ggplot_hydro_stress,
       filename = "inst/mat_meth/phychem/ggplot_hydro_stress.jpg")

# ---- Map ----

plot_maps_parameter_years(
  data = data_physico_chem_complete_full,
  parameter = "hydro_stress",
  filename = "inst/mat_meth/phychem/ggplot_hydro_stress_map.jpg"
)

# =====================================================
# 09. Identify time series completion by estuary, haline zone, season and parameter
# =====================================================

n_data_physico_chem <- data_physico_chem_complete_full |>
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
    geom_point(size = 0.8, alpha = 0.3) +
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

