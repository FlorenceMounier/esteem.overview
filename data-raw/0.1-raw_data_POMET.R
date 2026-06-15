# =====================================================
# Import, join and formatting of POMET raw data
# Datasets:
#  - data_POMET_traits.rda
#  - data_POMET_species.rda
#  - data_POMET_ALLspecies_densities.rda
#  - data_POMET_indiv_biom.rda
# Graphs in /inst/mat_meth
#  - plot_POMET_seabass_size_class.jpg
#  - plot_POMET_sole_size_class.jpg
# Preparation script
# Author: FM
# Date: 2026-06-11
# =====================================================

# =====================================================
# 00. Packages, functions and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(rpart)

`%!in%` = Negate(`%in%`)

data(raw_data_POMET_densities)
data(raw_data_POMET_indiv)
data(raw_data_POMET_traits)



# =====================================================
# 01. data_POMET_traits
# =====================================================

data_POMET_traits <- raw_data_POMET_traits |>

# ---- Create GPS central position of traits ----
mutate(latitude = (pos_deb_lat_dd + pos_fin_lat_dd) / 2) |>
  mutate(longitude = (pos_deb_long_dd + pos_fin_long_dd) / 2) |>

# ----- Filter unrelevant observations -----
filter(trait_id != 82) |> # fleuve Gironde
  filter(trait_id %!in% c(6185, 8494)) |>  # out of Loire
  filter(trait_id %!in% c(13477, 16087, 14362))# filandres Seine

# ----- Select and rename variable of interest -----
data_POMET_traits <- data_POMET_traits |>
  rename(DATE = madate,
         O2sat = oxygene,
         salinity = salinite) |>
  select(trait_id, DATE, latitude, longitude,
         maree, O2sat, salinity, temperature)

# ----- Define estuary from GPS delimitations -----
data_POMET_traits <- get_estuary_from_gps_position(data = data_POMET_traits,
                                                   latitude = latitude,
                                                   longitude = longitude)

# ----- Define haline_zone from GPS delimitations -----
data_POMET_traits <- get_haline_zone_from_gps_position(data = data_POMET_traits,
                                                latitude = latitude,
                                                longitude = longitude)

# ----- Define information on year, month, season from date -----
data_POMET_traits <- get_info_from_dates(data = data_POMET_traits, date_variable = DATE)

# ---- Pivot longer ----
data_POMET_traits <- data_POMET_traits |>
  pivot_longer(cols = c(maree, O2sat, salinity, temperature),
               names_to = "PARAMETRE_LIBELLE",
               values_to = "RESULTAT")

# ---- Save data_POMET_traits.rda ----
usethis::use_data(data_POMET_traits, overwrite = TRUE)



# =====================================================
# 03. data_POMET_species
# =====================================================

data_POMET_species <- raw_data_POMET_densities |>
  distinct(phylum, nom, nom_fr,
           famille,
           genre,
           espece_id,
           name,
           Ecological_guild,
           Position_guild,
           Trophic_guild,
           trophic_index_fishbase)

# ---- Save data_POMET_species.rda ----
usethis::use_data(data_POMET_species, overwrite = TRUE)



# =====================================================
# 04. data_POMET_densities for ALL species
# =====================================================

data_POMET_ALLspecies_densities <- raw_data_POMET_densities |>

# ----- Select and rename variable of interest -----
select(trait_id, name, nt, Densite) |>

# ---- Compute total densities by species and traits
  group_by(trait_id, name) |>
  summarise(nt = sum(nt, na.rm = TRUE),
            Densite = sum(Densite, na.rm = TRUE),
            .groups = "drop") |>
  drop_na(nt, Densite) |>

# ---- Join with traits information ----
left_join(data_POMET_traits, by = "trait_id")

# ---- Save data_POMET_ALLspecies_densities.rda ----
usethis::use_data(data_POMET_ALLspecies_densities, overwrite = TRUE)


# =====================================================
# 05. data_POMET_indiv_biom for Solea solea & Dicentrarchus labrax
# =====================================================

# ---- Individual biometrics ----

data_POMET_indiv_biom <- raw_data_POMET_indiv  |>

# ----- Select and rename variable of interest -----
select(trait_id, name, longueur, poids) |>
rename(length_mm = longueur,
       mass_g = poids) |>

# ---- Join with traits information ----
left_join(data_POMET_traits, by = "trait_id")

# ---- Fish size class for Solea solea & Dicentrarchus labrax ----

sole_size_class <- fct_size_class(data = data_POMET_indiv_biom, species = "Solea solea")
seabass_size_class <- fct_size_class(data = data_POMET_indiv_biom, species = "Dicentrarchus labrax")

plot_POMET_sole_size_class <- sole_size_class$gg_size
ggsave(filename = "inst/mat_meth/fish_size_class/plot_POMET_sole_size_class.jpg",
       plot = plot_POMET_sole_size_class, width = 15, height = 10, units = "cm")

plot_POMET_seabass_size_class <- seabass_size_class$gg_size
ggsave(filename = "inst/mat_meth/fish_size_class/plot_POMET_seabass_size_class.jpg",
       plot = plot_POMET_seabass_size_class, width = 15, height = 10, units = "cm")

data_POMET_indiv_biom <- data_POMET_indiv_biom |>
  mutate(size_class = case_when(
    name == "Dicentrarchus labrax" & length_mm <= seabass_size_class$size_threshold ~ "G0",
    name == "Dicentrarchus labrax" & length_mm > seabass_size_class$size_threshold ~ "G1",
    name == "Solea solea" & length_mm <= sole_size_class$size_threshold ~ "G0",
    name == "Solea solea" & length_mm > sole_size_class$size_threshold ~ "G1",
    TRUE ~ NA
  ))

# ---- Save data_POMET_indiv_biom.rda ----
usethis::use_data(data_POMET_indiv_biom, overwrite = TRUE)

