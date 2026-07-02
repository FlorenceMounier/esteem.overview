# =====================================================
# Plots:
#  - ggplot_REBENT_sp_richness.jpg
#  - ggplot_shannon_index.jpg
#  - ggplot_REBENT_density.jpg
#  - ggplot_REBENT_AMBI_index.jpg
#  - ggplot_REBENT_BEQIFR_index.jpg
# Preparation script
# Author: FM
# Date: 2026-02-18
# =====================================================

# =====================================================
# 0. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(benthos)
library(writexl)

# =====================================================
# 1. Species richness (S)
# =====================================================

# ---- Species richness ----
species_richness <- data_REBENT_biota |>
  group_by(estuary, YEAR, tidal, species) |>
  summarise(count = sum(RESULTAT)) |>
  summarise(sp_rich = species_richness(taxon = species, count = count), .groups = "drop")

ggplot_REBENT_sp_richness <- ggplot(species_richness) +
  aes(x = YEAR, y = sp_rich, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_point() +
  geom_line() +
  labs(title = "REBENT macrobenthic fauna - Species richness",
       x = NULL, y = "Species richness",
       color = "Estuary") +
  theme_esteem()

ggsave(plot = ggplot_REBENT_sp_richness,
       filename = "inst/results/data_benthos/indicators/ggplot_REBENT_species_richness.jpg",
       width = 15, height = 6, units = "cm")

# Species richness per log square metre



# =====================================================
# 2. Shannon index (H')
# =====================================================

# ni = Number of individuals for each species per estuary, tidal zone and year
nb_ind_sp <- data_REBENT_biota |>
  group_by(estuary, tidal, YEAR, species) |>
  summarise(n_ind_sp = sum(RESULTAT), .groups = "drop")

# N = Total number of individuals per estuary, tidal zone and year
nb_ind_tot <- data_REBENT_biota |>
  group_by(estuary, tidal, YEAR) |>
  summarise(n_ind_tot = sum(RESULTAT), .groups = "drop")

# pi = Percentage abundance for each species per estuary, tidal zone and year
abundance_prop <- nb_ind_sp |>
  left_join(nb_ind_tot, by = c("estuary", "tidal", "YEAR")) |>
  group_by(estuary, tidal, YEAR) |>
  mutate(abundance_prop = n_ind_sp / n_ind_tot) |>
  ungroup()

shannon_index <- abundance_prop |>
  group_by(estuary, tidal, YEAR) |>
  summarise(shannon = - sum(abundance_prop * log2(abundance_prop)), .groups = "drop")

ggplot_shannon_index <- ggplot(shannon_index) +
  aes(x = YEAR, y = shannon, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_point() +
  geom_line() +
  labs(title = "REBENT macrobenthic fauna - Shannon index",
       x = NULL, y = "Shannon index",
       color = "Estuary") +
  theme_esteem()

ggsave(plot = ggplot_shannon_index,
       filename = "inst/results/data_benthos/indicators/ggplot_REBENT_shannon_index.jpg",
       width = 15, height = 6, units = "cm")


# =====================================================
# 3. Abundance / density
# =====================================================

# ---- Number of individual per m2 at a given LIEU_MNEMONIQUE & DATE ---

# One line per species: nb of individuals
data_benthos_ind_species_m2 <- data_REBENT_biota |>
  # nb of sub-sample per LIEU_MNEMONIQUE & DATE & sampler_type
  group_by(estuary, LIEU_MNEMONIQUE, DATE, sampler_type, tidal) |>
  mutate(nb_samples_lieu_mnemonique_date = unique(mnemonique) |>  length()) |>
  # summarise nb of ind per species at a given LIEU_MNEMONIQUE & DATE & sampler_type
  group_by(estuary, LIEU_MNEMONIQUE, DATE, sampler_type, sampler_surface,
           tidal, nb_samples_lieu_mnemonique_date, species) |>
  summarise(nb_ind_species_lieu_mnemonique_date = sum(RESULTAT)) |>
  # compute nb of in per species for the total surface of LIEU_MNEMONIQUE
  mutate(nb_ind_species_per_m2 = nb_ind_species_lieu_mnemonique_date / (sampler_surface * nb_samples_lieu_mnemonique_date)) |>
  ungroup()

# ---- Yearly mean of individuals per surface ----
data_benthos_ind_m2 <- data_benthos_ind_species_m2 |>
  mutate(YEAR = year(DATE)) |>
  group_by(estuary, YEAR, DATE, LIEU_MNEMONIQUE, tidal) |>
  summarise(nb_ind_per_m2 = sum(nb_ind_species_per_m2)) |>
  group_by(estuary, YEAR, tidal) |>
  summarise(mean_abundance_m2 = mean(nb_ind_per_m2)) |>
  ungroup()

# ---- Plot ----
ggplot_REBENT_density <- ggplot(data_benthos_ind_m2) +
  aes(x = YEAR, y = mean_abundance_m2, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_line(position = "dodge") +
  geom_point() +
  labs(title = "REBENT macrobenthic fauna: Density",
       x = NULL,
       y = "Density (mean nb ind/m²)",
       color = "Estuary") +
  theme_esteem()

ggsave(plot = ggplot_REBENT_density,
       "inst/results/data_benthos/indicators/ggplot_REBENT_density.jpg",
       width = 17, height = 10, units = "cm")


# =====================================================
# 4. AZTI Marine Biotic Index (AMBI)
# =====================================================

# Ecological groups of sensitivity to anthropic disturbance

# ---- List of ESTEEM species ----
list_species_REBENT <- data_REBENT_biota |>
  distinct(estuary, YEAR, AphiaID, phylum, class, order, species) |>
  mutate(species = case_when(
    species == "Nemertes" ~ "Nemertea",
    TRUE ~ species))

# ---- benthos ecological groups ----
List_Group_Taxon_AMBI <- left_join(
  data.frame(readRDS(system.file("extdata", "azti.rds", package = "benthos"))), # DB Azti
  as.data.frame(get_ambi(which = "NL")))  # Dutch DB

# ---- Joining ----
grp_taxon_ambi <- list_species_REBENT |> left_join(List_Group_Taxon_AMBI, by = c("species" = "TAXON"))

# ---- Get percentage of species with AMBI ecological group ----
percent_without_ambi_grp_taxon <- grp_taxon_ambi |>
  group_by(estuary, YEAR) |>
  summarise(nb_without_ambi = sum(is.na(GROUP)),
            nb_tot = n()) |>
  mutate(percent_without_ambi_grp_taxon = nb_without_ambi / nb_tot * 100)

# ---- Get list of species with no AMBI ecological group ----
grp_taxon_ambi |>
  filter(is.na(GROUP)) |>
  distinct(phylum, class, order, species) |>
  distinct(species) |> arrange(species)

# ---- Compute AMBI ----
ambi_index <- data_REBENT_biota |>
  mutate(species = case_when(
    species == "Nemertes" ~ "Nemertea",
    TRUE ~ species)) |>
  group_by(estuary, YEAR, tidal, species) |>
  summarise(count = sum(RESULTAT), .groups = "drop") |>
  left_join(grp_taxon_ambi, by = c("species", "YEAR", "estuary")) |>
  group_by(estuary, YEAR, tidal) |>
  summarise(ambi = esteem.overview::compute_ambi(
    data = cur_data(),
    taxon = species,
    count = count,
    group = GROUP),
    .groups = "drop")

# ---- Graph of AMBI index ----
ggplot_ambi_index <- ggplot(ambi_index) +
  aes(x = YEAR, y = ambi, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_point() +
  geom_line() +
  labs(title = "REBENT macrobenthic fauna - AMBI",
       x = NULL, y = "AZTI Marine Biotic Index",
       color = "Estuary") +
  theme_esteem()

ggsave(plot = ggplot_ambi_index,
       filename = "inst/results/data_benthos/indicators/ggplot_REBENT_AMBI_index.jpg",
       width = 15, height = 6, units = "cm")

# =====================================================
# 5. BEQI-FR
# =====================================================

# ---- Reference values to compute BEQI-FR
ref_values <- tribble(
  ~ estuary, ~ tidal, ~ EUNIS, ~ AMBI_ref, ~ H_ref, ~ S_ref,
  "Gironde", "intertidal", "A2.31", 2.5, 2.9, 14,
  "Gironde", "subtidal", "A5.32", 1.9, 2.5, 10,
  "Loire", "intertidal", "A2.31", 2.5, 2.9, 14,
  "Loire", "subtidal", "A5.25", 1.0, 3.8, 33,
  "Seine", "intertidal", "A2.24", 1.4, 3.7, 26,
  "Seine", "subtidal", "A5.22", 0.3, 2.7, 9
)

# ---- Compute BEQI-FR ----
BEQI <- species_richness |>
  full_join(shannon_index) |>
  full_join(ambi_index) |>
  left_join(ref_values, by = c("estuary", "tidal")) |>
  mutate(EQR_ambi = (ambi - 7)/(AMBI_ref - 7),
         EQR_H = shannon / H_ref,
         EQR_S = sp_rich / S_ref,
         BEQIFR = (EQR_ambi + EQR_H + EQR_S)/3) |>
  mutate(BEQIFR = case_when(
    BEQIFR >= 1 ~ 1,
    TRUE ~ BEQIFR
  ))

# ---- Graph ----
ggplot_BEQI_index <- ggplot(BEQI) +
  aes(x = YEAR, y = BEQIFR, color = estuary,
      linetype = tidal, shape = tidal) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.86) +
  geom_hline(yintercept = 0.67) +
  geom_hline(yintercept = 0.4) +
  labs(title = "REBENT macrobenthic fauna - BEQI-FR",
       x = NULL, y = "Benthic Ecosystem Quality Index (BEQI-FR)",
       color = "Estuary") +
  annotate("text", x = 2015, y = 0.92, label = "High", size = 4, color = "black") +
  annotate("text", x = 2015, y = 0.76, label = "Good", size = 4, color = "black") +
  annotate("text", x = 2015, y = 0.53, label = "Moderate", size = 4, color = "black") +
  theme_esteem()

ggsave(plot = ggplot_BEQI_index,
       filename = "inst/results/data_benthos/indicators/ggplot_REBENT_BEQIFR_index.jpg",
       width = 15, height = 6, units = "cm")
