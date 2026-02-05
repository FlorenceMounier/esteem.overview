# ---------------------------------------------------------------------------
# Libraries ----

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(mixtools)

# ---------------------------------------------------------------------------
# Read raw data from {esteem.overview} ----

data_POMET_biota <- esteem.overview::data_pomet
data_POMET_traits <- esteem.overview::data_POMET_traits

# ---------------------------------------------------------------------------
# Variables and join

names(data_POMET_biota)
names(data_POMET_traits)

intersect(names(data_POMET_biota), names(data_POMET_traits))

data_POMET <- left_join(data_POMET_biota, data_POMET_traits,
          by = c("trait_id", "masse_eau", "annee", "distance_chalutee",
                 "saison", "engin", "long_trait", "materiel_code"))

# ---------------------------------------------------------------------------
# Create "estuary" variable

data_POMET <- data_POMET |>
  mutate(estuary = case_when(
    masse_eau |> str_starts("Gironde") ~ "Gironde",
    masse_eau |> str_starts("Loire") ~ "Loire",
    masse_eau |> str_starts("Seine") ~ "Seine",
    TRUE ~ NA
  )) |>
  filter(masse_eau != "Gironde externe")

# ---------------------------------------------------------------------------
# DCE zones and salinity

data_POMET |>
  ggplot() +
  aes(x = salinite, fill = masse_eau) +
  geom_boxplot() +
  facet_grid(vars(estuary))

# ---------------------------------------------------------------------------
# Ecological guild list

data_POMET |>
  distinct(Ecological_guild)

# ---------------------------------------------------------------------------
# Prey species

data_POMET |>
  distinct(phylum)

data_POMET |>
  filter(phylum != "Chordata") |>
  distinct(phylum, famille, name) |>
  arrange(phylum, famille) |>
  print(n = 48)

# ---------------------------------------------------------------------------
# Marine juvenile species list

data_POMET |>
  filter(Ecological_guild == "MJ") |>
  distinct(phylum, name, nom_fr) |>
  arrange(phylum) |>
  print(n = 24)

# ---------------------------------------------------------------------------
# Subdataset of marine indicators in autumn

data_POMET_MJ_indicator <- data_POMET |>
  filter(name %in% c("Solea solea", "Dicentrarchus labrax"),
         saison == "automne")

# Distribution per water zone and fish length
data_POMET_MJ_indicator |>
  ggplot() +
  aes(x = longueur, colour = masse_eau) +
  geom_density() +
  facet_grid(rows = vars(estuary),
             cols = vars(name), scales = "free_y")

# ---------------------------------------------------------------------------
# Biological size class - Seabass

longueurs_bar <- data_POMET_MJ_indicator |>
  filter(name == "Dicentrarchus labrax") |>
  pull(longueur)

# Ajustement du mélange
mix <- normalmixEM(longueurs_bar, k = 2)
summary(mix)

# Grille de valeurs
xgrid <- tibble(
  x = seq(min(longueurs_bar), max(longueurs_bar), length.out = 5000)
)

#  Densités pondérées
densites <- xgrid |>
  mutate(
    dens1 = mix$lambda[1] * dnorm(x, mix$mu[1], mix$sigma[1]),
    dens2 = mix$lambda[2] * dnorm(x, mix$mu[2], mix$sigma[2]),
    dens_tot = dens1 + dens2
  )

i1 <- which.max(densites$dens1)
i2 <- which.max(densites$dens2)

i_min <- min(i1, i2)
i_max <- max(i1, i2)

idx_creux <- i_min + which.min(densites$dens_tot[i_min:i_max]) - 1
seuil_bar <- densites$x[idx_creux]

# Graph

dens_long <- densites |>
  select(x, dens1, dens2)  |>
  pivot_longer(-x, names_to = "groupe", values_to = "densite")

ggplot() +
  # Histogramme
  geom_histogram(
    aes(x = longueurs_bar, y = after_stat(density)),
    bins = 40,
    fill = "grey80",
    color = "white"
  ) +

  # Gaussiennes
  geom_line(
    data = dens_long,
    aes(x = x, y = densite, color = groupe),
    linewidth = 1.2
  ) +

  # Seuil
  geom_vline(
    xintercept = seuil_bar,
    linetype = "dashed",
    linewidth = 1,
    color = "purple"
  ) +

  # Couleurs & labels
  scale_color_manual(
    values = c("dens1" = "red", "dens2" = "blue"),
    labels = c("Groupe 1", "Groupe 2"),
    name = "Distribution"
  ) +

  labs(
    x = "Longueur (mm)",
    y = "Densité",
    title = "Mélange gaussien et seuils de séparation - Bar"
  )

# ---------------------------------------------------------------------------
# Biological size class - Sole

longueurs_sole <- data_POMET_MJ_indicator |>
  filter(name == "Solea solea") |>
  drop_na(longueur) |>
  pull(longueur)

# Ajustement du mélange
mix <- normalmixEM(longueurs_sole, k = 2)
summary(mix)

# Grille de valeurs
xgrid <- tibble(
  x = seq(min(longueurs_sole), max(longueurs_sole), length.out = 5000)
)

#  Densités pondérées
densites <- xgrid |>
  mutate(
    dens1 = mix$lambda[1] * dnorm(x, mix$mu[1], mix$sigma[1]),
    dens2 = mix$lambda[2] * dnorm(x, mix$mu[2], mix$sigma[2]),
    dens_tot = dens1 + dens2
  )

i1 <- which.max(densites$dens1)
i2 <- which.max(densites$dens2)

i_min <- min(i1, i2)
i_max <- max(i1, i2)

idx_creux <- i_min + which.min(densites$dens_tot[i_min:i_max]) - 1
seuil_sole <- densites$x[idx_creux]

# Graph

dens_long <- densites |>
  select(x, dens1, dens2)  |>
  pivot_longer(-x, names_to = "groupe", values_to = "densite")

ggplot() +
  # Histogramme
  geom_histogram(
    aes(x = longueurs_sole, y = after_stat(density)),
    bins = 40,
    fill = "grey80",
    color = "white"
  ) +

  # Gaussiennes
  geom_line(
    data = dens_long,
    aes(x = x, y = densite, color = groupe),
    linewidth = 1.2
  ) +

  # Seuil
  geom_vline(
    xintercept = seuil_sole,
    linetype = "dashed",
    linewidth = 1,
    color = "purple"
  ) +

  # Couleurs & labels
  scale_color_manual(
    values = c("dens1" = "red", "dens2" = "blue"),
    labels = c("G0", "G1"),
    name = "Distribution"
  ) +

  labs(
    x = "Longueur (mm)",
    y = "Densité",
    title = "Mélange gaussien et seuils de séparation - Sole"
  )


# ---------------------------------------------------------------------------
# Biological size class repartition of individuals

data_POMET_MJ_indicator <- data_POMET_MJ_indicator |>
  mutate(size_class = case_when(
    name == "Dicentrarchus labrax" & longueur <= 130 ~ "G0",
    name == "Dicentrarchus labrax" & longueur > 130 ~ "G1",
    name == "Solea solea" & longueur <= 180 ~ "G0",
    name == "Solea solea" & longueur > 180 ~ "G1",
    TRUE ~ NA
  ))

# ---------------------------------------------------------------------------
# ## Calcul de l'Abondance poissons (= Nb d'indiv / surface ech. (long trait * long chalut) )
#### Petit chalut a perche (=CHAP3) * 1.5 ; Grand chalut a perche (CHAP3 DCE) * 3


data_POMET_density <- data_POMET_MJ_indicator |>
  group_by(materiel_code, estuary, annee, trait_id, long_trait, name, size_class) |>
  # tot nb of individuals
  summarise(n_indiv = n(), .groups = "drop") |>
  # density per trait
  mutate(density_trait = case_when(
    materiel_code == "CHAP3" ~ (n_indiv / long_trait * 1.5) * 1000,
    materiel_code == "CHAP3 DCE" ~ (n_indiv / long_trait * 3) * 1000,
    TRUE ~ NA,
  )) |>
  # mean density per estuary, year, species and size class
  group_by(estuary, annee, name, size_class) |>
  summarise(density = mean(density_trait), .groups = "drop") |>
  drop_na(size_class)

ggplot(data_POMET_density) +
  aes(x = annee, y = density, colour = size_class) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    span = 0.6,
    linewidth = 1
  ) +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y")


# ---------------------------------------------------------------------------
# ## Calcul de la biomasse crevettes /unité surf (= Biomasse / surface ech. (long trait * long chalut) )
#### Petit chalut a perche

data_POMET_shrimps <- data_POMET |>
  filter(name %in% c("Crangon crangon", "Palaemon longirostris"),
         saison == "automne")

data_POMET_shrimps_biomass <- data_POMET_shrimps |>
  filter(!is.na(pt)) |>
  # biomass per trait
  mutate(biomass_trait = case_when(
    materiel_code == "CHAP3" ~ (pt / long_trait * 1.5) * 1000,
    materiel_code == "CHAP3 DCE" ~ (pt / long_trait * 3) * 1000,
    TRUE ~ NA,
  )) |>
  # mean biomass per estuary, year, species
  group_by(estuary, annee, name) |>
  summarise(biomass_trait = mean(biomass_trait), .groups = "drop")

ggplot(data_POMET_shrimps_biomass) +
  aes(x = annee, y = biomass_trait, colour = name) +
  geom_line() +
  geom_smooth(
    method = "loess",
    se = FALSE,
    span = 0.6,
    linewidth = 1
  ) +
  facet_grid(rows = vars(name), cols = vars(estuary), scales = "free_y")
