# =====================================================
# Plots:
#   - ggplot_PCA.jpg
# Preparation script
# Author: FM
# Date: 2026-03-09
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
# ACP
library(FactoMineR)
library(factoextra)

# =====================================================
#
# =====================================================

data_PCA <- data_temp |>
  full_join(data_salin) |>
  full_join(data_nitro) |>
  mutate(DECADE = case_when(
    year %in% c(1970:1979) ~ "1970's",
    year %in% c(1980:1989) ~ "1980's",
    year %in% c(1990:1999) ~ "1990's",
    year %in% c(2000:2009) ~ "2000's",
    year %in% c(2010:2019) ~ "2010's",
    year %in% c(2020:2029) ~ "2020's",
  )) |>
  group_by(DECADE, PARAMETRE_LIBELLE, estuary) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop") |>
  dplyr::select(DECADE, PARAMETRE_LIBELLE, RESULTAT, estuary) |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT)

# Get only numerical values for PCA
data_pca_numerical <- data_PCA |>
  dplyr::select(Salinité, `Température de l'eau`, N_indicator)

# Compute PCA
res_pca <- PCA(data_pca_numerical, scale.unit = TRUE, graph = FALSE)

# Points coordinates
scores <- as.data.frame(res_pca$ind$coord) |>
  mutate(
    DECADE = data_PCA$DECADE,
    estuary = data_PCA$estuary
  )

# Variables coordinates & contributions to PCA factorial design
var <- as.data.frame(res_pca$var$coord)
var$varname <- rownames(var)
var$contrib <- res_pca$var$contrib[,1] + res_pca$var$contrib[,2]

# Explained variance
eig <- res_pca$eig
pc1 <- round(eig[1,2],1)
pc2 <- round(eig[2,2],1)

#

fviz_contrib(res_pca, choice="var", axes = 1 )
fviz_contrib(res_pca, choice="var", axes = 2 )

# Biplot: Individual ESTUARY & DECADE graph

ggplot_PCA <- ggplot(scores, aes(Dim.1, Dim.2)) +
  geom_path(aes(group = estuary, color = estuary)) + # decade trajectories
  geom_point(aes(color = estuary)) +
  geom_text(aes(label = DECADE), size = 3) +
  stat_ellipse(aes(color = estuary), linewidth = 1) + # estuary ellipses
  # variables projetées
  geom_segment(
    data = var,
    aes(x = 0, y = 0, xend = Dim.1*3, yend = Dim.2*3),
    color = "black",
    arrow = arrow(length = unit(0.25,"cm")),
    linewidth = 1
  ) +
  geom_text(
    data = var,
    aes(x = Dim.1*3, y = Dim.2*3, label = varname),
    color = "black",
    vjust = -0.5
  ) +
  labs(
    x = paste0("PC1 (", pc1, "%)"),
    y = paste0("PC2 (", pc2, "%)"),
    color = "Estuary"
  ) +
  theme_minimal()

ggplot_PCA

#------------------------------------------------------------------------




# Primary production activity

# Compute pheopigment/chlorophyl


data_summarized_p1 <- data_summarized |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  mutate(p1_indicator = Phéopigments / `Chlorophylle a`) |>
  pivot_longer(cols = -c(ESTUARY, YEAR, PROGRAMME),
               names_to = "PARAMETRE_LIBELLE", values_to = "RESULTAT")

data_p1 <- data_summarized_p1 |>
  filter(PARAMETRE_LIBELLE == "p1_indicator")

ggplot_data_p1 <- ggplot(data_p1) +
  aes(x = YEAR, y = RESULTAT, colour = ESTUARY) +
  geom_line()
ggplot_data_p1

# ggsave(plot = ggplot_data_p1, filename = "../inst/results/data_physico_chemistry/ggplot_primary_prod.jpg",width = 15, height = 10, units = "cm")



