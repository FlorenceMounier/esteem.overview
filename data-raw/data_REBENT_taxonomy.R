# =====================================================
# Datasets:
#  - abundance_prop_species.xlsx
# Plots:
#  - ggplot_REBENT_abundance_taxonomy.jpg
#  - ggplot_REBENT_abundance_species.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 0. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(writexl)

# =====================================================
# 1. Taxonomic composition - Phylum level
# =====================================================

# ni = Number of individuals for each phylum per estuary, tidal zone and year
nb_ind_sp <- data_REBENT_biota |>
  group_by(estuary, tidal, YEAR, phylum) |>
  summarise(n_ind_sp = sum(RESULTAT), .groups = "drop")

# N = Total number of individuals per estuary, tidal zone and year
nb_ind_tot <- data_REBENT_biota |>
  group_by(estuary, tidal, YEAR) |>
  summarise(n_ind_tot = sum(RESULTAT), .groups = "drop")

# pi = Percentage abundance for each phylum per estuary, tidal zone and year
abundance_prop <- nb_ind_sp |>
  left_join(nb_ind_tot, by = c("estuary", "tidal", "YEAR")) |>
  group_by(estuary, tidal, YEAR) |>
  mutate(abundance_prop = n_ind_sp / n_ind_tot * 100) |>
  ungroup() |>
  drop_na()


ggplot_REBENT_abundance_taxonomy <- abundance_prop |>
  filter(phylum %in% c("Annelida", "Arthropoda", "Mollusca")) |>
  ggplot() +
  aes(x = YEAR, y = abundance_prop, color = phylum, fill = phylum) +
  geom_col() +
  facet_grid(rows = vars(estuary), cols = vars(tidal)) +
  labs(title = "REBENT macrobenthic fauna: Taxonomic repartition",
       x = NULL,
       y = "Abundance (%)",
       color = "Phylum", fill = "Phylum") +
  theme_esteem()

ggsave(plot = ggplot_REBENT_abundance_taxonomy,
       "inst/results/data_benthos/indicators/ggplot_REBENT_abundance_taxonomy.jpg",
       width = 15, height = 6, units = "cm")

# =====================================================
# 2. Taxonomic composition - Species level
# =====================================================

nb_ind_tot <- data_REBENT_biota |>
  group_by(estuary, tidal) |>
  summarise(n_ind_tot = sum(RESULTAT), .groups = "drop")

nb_ind_sp <- data_REBENT_biota |>
  group_by(estuary, tidal, phylum, class, order, species) |>
  summarise(n_ind_sp = sum(RESULTAT), .groups = "drop")

abundance_prop <- nb_ind_sp |>
  left_join(nb_ind_tot, by = c("estuary", "tidal")) |>
  group_by(estuary, tidal, phylum, class, order) |>
  mutate(abundance_prop = n_ind_sp / n_ind_tot * 100) |>
  ungroup() |>
  drop_na()

abundance_prop_species <- abundance_prop |>
  filter(abundance_prop > 10) |>
  arrange(estuary, tidal, phylum, class, order, desc(abundance_prop)) |>
  select(-c(n_ind_sp, n_ind_tot))
abundance_prop_species
write_xlsx(abundance_prop_species, "inst/results/data_benthos/abundance_prop_species.xlsx")

abundance_prop_species_5 <- abundance_prop |>
  filter(abundance_prop > 5) |>
  arrange(estuary, tidal, phylum, class, order, desc(abundance_prop)) |>
  select(-c(n_ind_sp, n_ind_tot))
abundance_prop_species_5

ggplot_REBENT_abundance_species <- abundance_prop_species_5 |>
  filter(phylum %in% c("Annelida", "Arthropoda", "Mollusca")) |>
  mutate(species_grouped = paste(phylum, species, sep=": ")) |>
  ggplot() +
  aes(x = phylum, y = abundance_prop, fill = species_grouped) +
  geom_col() +
  facet_grid(rows = vars(estuary), cols = vars(tidal)) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2)) +
  scale_fill_manual(values = c("pink", "forestgreen", "lightblue", "gold1",
                               "red", "blue", "purple", "green", "orange",
                               "yellow", "gold4", "blue4", "coral", "grey",
                               "royalblue", "pink3","green2", "orange3", "black"),
                    guide = guide_legend(title = "Species (by Phylum)")) +
  labs(title = "REBENT macrobenthic fauna: Species composition",
       x = NULL,
       y = "Abundance (%)",
       fill = "Species") +
  theme_esteem()

ggplot_REBENT_abundance_species

ggsave(plot = ggplot_REBENT_abundance_species,
       "inst/results/data_benthos/indicators/ggplot_REBENT_abundance_species.jpg",
       height = 10, width = 8)
