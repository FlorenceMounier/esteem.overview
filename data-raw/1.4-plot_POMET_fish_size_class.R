# =====================================================
# Results: plot_POMET_*_size_class.jpg
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(dplyr)

# =====================================================
# 01. Biological size class
# =====================================================

sole_size_class <- fct_size_class(data = data_POMET_indiv, species = "Solea solea")
seabass_size_class <- fct_size_class(data = data_POMET_indiv, species = "Dicentrarchus labrax")

# =====================================================
# 02. Save plot results
# =====================================================

plot_POMET_sole_size_class <- sole_size_class$gg_size
ggsave(filename = "inst/results/data_POMET/fish_size_class/plot_POMET_sole_size_class.jpg",
       plot = plot_POMET_sole_size_class, width = 15, height = 10, units = "cm")

plot_POMET_seabass_size_class <- seabass_size_class$gg_size
ggsave(filename = "inst/results/data_POMET/fish_size_class/plot_POMET_seabass_size_class.jpg",
       plot = plot_POMET_seabass_size_class, width = 15, height = 10, units = "cm")

# =====================================================
# 03. Increment data_POMET
# =====================================================

data_POMET_indiv <- data_POMET_indiv |>
  mutate(size_class = case_when(
    name == "Dicentrarchus labrax" & longueur <= seabass_size_class$size_threshold ~ "G0",
    name == "Dicentrarchus labrax" & longueur > seabass_size_class$size_threshold ~ "G1",
    name == "Solea solea" & longueur <= sole_size_class$size_threshold ~ "G0",
    name == "Solea solea" & longueur > sole_size_class$size_threshold ~ "G1",
    TRUE ~ NA
  ))

usethis::use_data(data_POMET_indiv, overwrite = TRUE)
