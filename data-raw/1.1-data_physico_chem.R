# =====================================================
# Preparation script
# Datasets:
#  - data_physico_chem_joined.rda
#  - data_physico_chem_complete_full.rda
# Plots:
#  - plot_data_physico_chem_completion.jpg in /int/mat_meth/phychem
#  - ggplot_gantt_data.jpg in /int/mat_meth/phychem
#  - ggplot_ammonia_risk_synergy.jpg in /int/mat_meth/phychem
#  - ggplot_ammonia_risk_tile.jpg in /int/mat_meth/phychem

#  - map_gironde_physico_chem.jpg in /int/mat_meth/phychem
# Author: FM
# Date: 2026-06-17
# =====================================================

# =====================================================
# 00. Packages and data
# =====================================================

library(esteem.overview)
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

data_physico_chem <- raw_data_physico_chem  |>
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
  filter_studied_gps_boxes()


# ---- Data POMET ----

data_POMET_traits_filtered <- data_POMET_traits |>
  mutate(PROGRAMME = "POMET") |>
  mutate(year_month = lubridate::make_date(year, month, 1)) |>
  mutate(PARAMETRE_LIBELLE = case_when(
    PARAMETRE_LIBELLE == "temperature" ~ "Temperature",
    PARAMETRE_LIBELLE == "salinity" ~ "Salinity",
    TRUE ~ PARAMETRE_LIBELLE
  )) |>
  filter_studied_gps_boxes() |>
  # Summarise by month
  group_by(estuary, PROGRAMME, PARAMETRE_LIBELLE, longitude, latitude, haline_zone, year, month, year_month, season) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop")


# ---- Join datasets ----
data_physico_chem_joined <- rbind(data_physico_chem, data_POMET_traits_filtered)

# ---- Time series completion period ----
table_data_physico_chem_completion <- data_physico_chem_joined |>
  group_by(estuary, year, haline_zone) |>
  summarise(n = n(), .groups = "drop") |>
  pivot_wider(names_from = estuary, values_from = n) |>
  arrange(year, haline_zone) |>
  drop_na() |>
  pivot_longer(cols = c("Gironde", "Loire", "Seine"),
               names_to = "estuary", values_to = "n_points")

plot_data_physico_chem_completion <- ggplot(table_data_physico_chem_completion) +
  aes(x = year, y = n_points, colour = haline_zone) +
  geom_point() +
  facet_grid(vars(estuary)) +
  theme(axis.title.x = element_blank())

ggsave(plot_data_physico_chem_completion,
       filename = "inst/mat_meth/phychem/plot_data_physico_chem_completion.jpg")

# Data in inner and outer haline zone from all estuaries from 1985
data_physico_chem_joined <- data_physico_chem_joined |>
  filter(year >= 1985 & year < 2025)

usethis::use_data(data_physico_chem_joined, overwrite = TRUE)

# ---- Maps ----
data_physico_chem_joined |>  filter(estuary == "Gironde") |> distinct(PROGRAMME) |>  pull()
gironde_programs <- c("REPHY", "RNOHYD", "POMET")
map_gironde_physico_chem <- plot_estuary_map(data = data_physico_chem_joined |>
                   mutate(PROGRAMME = factor(x = PROGRAMME, levels = gironde_programs, labels = gironde_programs, ordered = TRUE)),
                 estuary_name = "Gironde",
                 colour_var = PROGRAMME, size_points = 1) +
  facet_wrap(vars(year), ncol = 5)
ggsave(plot = map_gironde_physico_chem,
       filename = "inst/mat_meth/phychem/map_gironde_physico_chem.jpg",
       width = 25, height = 30, units = "cm")

data_physico_chem_joined |>  filter(estuary == "Loire") |> distinct(PROGRAMME) |>  pull()
loire_programs <- c("REPHY", "RNOHYD", "RESLOC_44", "POMET")
map_loire_physico_chem <- plot_estuary_map(data = data_physico_chem_joined |>
                   mutate(PROGRAMME = factor(x = PROGRAMME, levels = loire_programs, labels = loire_programs, ordered = TRUE)),
                 estuary_name = "Loire",
                 colour_var = PROGRAMME, size_points = 1) +
  facet_wrap(vars(year), ncol = 5)
ggsave(plot = map_loire_physico_chem,
       filename = "inst/mat_meth/phychem/map_loire_physico_chem.jpg",
       width = 25, height = 30, units = "cm")


data_physico_chem_joined |>  filter(estuary == "Seine") |> distinct(PROGRAMME) |>  pull()
seine_programs <- c("REPHY", "RHLN", "REPHY;RHLN", "RNOHYD", "POMET")
map_seine_physico_chem <- plot_estuary_map(data = data_physico_chem_joined |>
                   mutate(PROGRAMME = factor(x = PROGRAMME,
                                             levels = seine_programs,
                                             labels = seine_programs, ordered = TRUE)),
                 estuary_name = "Seine",
                 colour_var = PROGRAMME, size_points = 1) +
  facet_wrap(vars(year), ncol = 5)
ggsave(plot = map_seine_physico_chem,
       filename = "inst/mat_meth/phychem/map_seine_physico_chem.jpg",
       width = 25, height = 25, units = "cm")


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

# ---- Ammonia formation risk from ammonium x temperature ----
risk_summary <- data_physico_chem_filtered |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
mutate(
  risk_NH4_temp = case_when(
    Ammonium >= 1 & Temperature >= 20 ~ 3,
    Ammonium >= 0.5 & Temperature >= 20 ~ 2,
    Ammonium >= 0.5 ~ 1,
    TRUE ~ 0
  )
)

ggplot_ammonia_risk_tile <- ggplot(risk_summary |> mutate(risk_NH4_temp = as.factor(risk_NH4_temp)),
       aes(year_month, estuary, fill = risk_NH4_temp)) +
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


ggplot_ammonia_risk_synergy <- ggplot(risk_summary |> mutate(risk_NH4_temp = as.factor(risk_NH4_temp)),
       aes(Temperature, Ammonium,
           color = risk_NH4_temp)) +
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
  pivot_longer(cols = c("Temperature", "Salinity", "O2sat", "Ammonium", "risk_NH4_temp"),
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
  pivot_longer(cols = c("Temperature", "Salinity", "O2sat", "Ammonium", "risk_NH4_temp", "hydro_stress"),
               names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT")

usethis::use_data(data_physico_chem_complete_full, overwrite = TRUE)

