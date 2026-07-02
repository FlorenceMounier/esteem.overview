# =====================================================
# Preparation script
# Datasets:
#  -
# Graphs in /inst/mat_meth
#  - plot_POMET_seabass_size_class.jpg
#  - plot_POMET_sole_size_class.jpg
# Author: FM
# Date: 2026-06-30
# =====================================================


# =====================================================
# 00. Packages, functions and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)


# =====================================================
# 06. Compute species abundance by size class
# =====================================================

# Calculation of abundance (= number of individuals/survey area (long line * long trawl))
#
# Small beam trawl (=CHAP3) * 1.5;
# Large beam trawl (CHAP3 DCE) * 3

data_POMET_partial_density <- data_POMET_indiv_biom |>
  group_by(estuary, year, season, materiel_code, trait_id, long_trait, name, haline_zone, size_class) |>
  # tot nb of individuals
  summarise(n_indiv = n(), .groups = "drop") |>
  # density per trait
  mutate(density_trait = case_when(
    materiel_code == "CHAP3" ~ (n_indiv / (long_trait * 1.5)) * 1000,
    materiel_code == "CHAP3 DCE" ~ (n_indiv / (long_trait * 3)) * 1000,
    TRUE ~ NA,
  )) |>
  full_join(data_POMET_traits |>
              pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT))

usethis::use_data(data_POMET_partial_density, overwrite = TRUE)

# =====================================================
# 07. Compute species salinity preferendum
# =====================================================

data_POMET_partial_density_studied_species <- data_POMET_partial_density |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax"))

plot_maps_parameter_years(
  data = data_POMET_partial_density_studied_species |>
    pivot_longer(cols = salinity, names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT"),
  parameter = "salinity",
  filename = "inst/mat_meth/POMET/ggplot_POMET_salinity_map.jpg"
)

#-----------------------------------
# Fonction calcul optimum
#-----------------------------------

get_optimum <- function(dat) {

  # sécurité : assez de données ?
  if(nrow(dat) < 15) {
    return(tibble(
      optimum = NA_real_,
      max_density = NA_real_,
      n = nrow(dat)
    ))
  }

  # modèle
  m <- try(
    gam(
      density_trait ~ s(salinity, k = 5),
      data = dat,
      family = tw()
    ),
    silent = TRUE
  )

  # si modèle échoue
  if(inherits(m, "try-error")) {
    return(tibble(
      optimum = NA_real_,
      max_density = NA_real_,
      n = nrow(dat)
    ))
  }

  # gradient de prédiction
  newdata <- tibble(
    salinity = seq(
      min(dat$salinity, na.rm = TRUE),
      max(dat$salinity, na.rm = TRUE),
      length.out = 500
    )
  )

  # prédictions
  newdata$pred <- predict(
    m,
    newdata = newdata,
    type = "response"
  )

  # optimum
  i_max <- which.max(newdata$pred)

  tibble(
    optimum = newdata$salinity[i_max],
    max_density = newdata$pred[i_max],
    n = nrow(dat)
  )
}

optima <- data_POMET_partial_density_studied_species %>%
  group_by(name, estuary, season) %>%
  nest() %>%
  mutate(results = map(data, get_optimum)) %>%
  unnest(results) %>%
  select(-data)

optima


optima_plot <- optima %>%
  ungroup() %>%
  mutate(
    season = factor(season, levels = c("spring", "summer", "autumn")),
    estuary = factor(estuary, levels = c("Gironde", "Loire", "Seine")),
    name = fct_recode(
      name,
      "Bar européen" = "Dicentrarchus labrax",
      "Sole commune" = "Solea solea"
    )
  )

ggplot(optima_plot, aes(x = optimum, y = season)) +
  geom_point(aes(size = n), alpha = 0.8, na.rm = TRUE) +
  facet_grid(name ~ estuary) +
  scale_size_continuous(name = "n traits") +
  labs(
    x = "Optimum de salinité estimé",
    y = NULL
  ) +
  theme_bw()

# =====================================================
# 08. Fish density plots by size class & season at estuary level
# =====================================================

data_POMET_partial_estuary_mean_density <- data_POMET_partial_density |>
  # mean density per estuary, year, species and size class
  group_by(estuary, year, season, name, size_class) |>
  summarise(mean_density = mean(density_trait, na.rm = TRUE), .groups = "drop") |>
  # drop NA estuary and length
  drop_na(estuary)

plot_POMET_estuary_size_season_density_fish <- data_POMET_partial_estuary_mean_density |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax")) |>
  drop_na(size_class) |>
  ggplot() +
  aes(x = year, y = mean_density, colour = season, linetype = size_class) +
  geom_line() +
  labs(title = "Juvenile fish densities by size class and season") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_estuary_size_season_density_fish.jpg",
       plot = plot_POMET_estuary_size_season_density_fish, width = 20, height = 10, units = "cm")


# =====================================================
# 09. Fish density plots by size class & haline zones
# =====================================================

data_POMET_partial_estuary_mean_density <- data_POMET_partial_density |>
  # mean density per estuary, year, species and size class
  group_by(estuary, year, haline_zone, name, size_class) |>
  summarise(mean_density = mean(density_trait, na.rm = TRUE), .groups = "drop") |>
  # drop NA estuary and length
  drop_na(estuary)

plot_POMET_estuary_size_salinity_density_fish <- data_POMET_partial_estuary_mean_density |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax")) |>
  drop_na(size_class) |>
  ggplot() +
  aes(x = year, y = mean_density, colour = haline_zone, linetype = size_class) +
  geom_line() +
  labs(title = "Juvenile fish densities by size class and haline zone") +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y") +
  theme_esteem()

ggsave(filename = "inst/results/data_POMET/species_densities/plot_POMET_estuary_size_salinity_density_fish.jpg",
       plot = plot_POMET_estuary_size_salinity_density_fish, width = 20, height = 10, units = "cm")
