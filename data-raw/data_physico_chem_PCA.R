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
# 0. Data
# =====================================================

data_PCA_full <- data_temp |>
  mutate(PARAMETRE_LIBELLE = "Temperature") |>
  full_join(data_salin |> mutate(PARAMETRE_LIBELLE = "Salinity")) |>
  full_join(data_nitro) |>
  full_join(data_flow_autumn) |>
  mutate(DECADE = case_when(
    year %in% c(1980:1989) ~ "1980's",
    year %in% c(1990:1999) ~ "1990's",
    year %in% c(2000:2009) ~ "2000's",
    year %in% c(2010:2019) ~ "2010's",
    year %in% c(2020:2029) ~ "2020's",
  )) |>
  group_by(DECADE, PARAMETRE_LIBELLE, estuary) |>
  summarise(RESULTAT = mean(RESULTAT, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  drop_na()


# =====================================================
# 1. PCA results for all estuaries
# =====================================================

data_PCA_numerical <- data_PCA |>
  dplyr::select(Flow, Salinity, Temperature, N_indicator)

fct_compute_pca(data_PCA = data_PCA,
                data_PCA_numerical = data_PCA_numerical,
                path = "inst/results/data_phychem/PCA/")


# =====================================================
# 2. PCA by estuary
# =====================================================

#---- Function PCA results ----

fct_compute_pca <- function(data_PCA, data_PCA_numerical, path){

  # Compute PCA
  res_pca <- PCA(data_PCA_numerical, scale.unit = TRUE, graph = FALSE)

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

  # Contributions to PCA axes

  ggplot_contrib_axe1 <- fviz_contrib(res_pca, choice="var", axes = 1)
  ggsave(ggplot_contrib_axe1,
         filename = file.path(path, "ggplot_contrib_axe1.jpg"))

  ggplot_contrib_axe2 <- fviz_contrib(res_pca, choice="var", axes = 2)
  ggsave(ggplot_contrib_axe2,
         filename = file.path(path, "ggplot_contrib_axe2.jpg"))

  # Biplot: Individual ESTUARY & DECADE graph

  ggplot_biplot_PCA <- ggplot(scores, aes(Dim.1, Dim.2)) +
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

  ggsave(ggplot_biplot_PCA,
         filename = file.path(path, "ggplot_biplot_PCA.jpg"))
}

# --- Applications
data_PCA_gironde <- data_PCA |> filter(estuary == "Gironde")
data_pca_gironde_numerical <- data_PCA_gironde |>
  dplyr::select(Flow, Salinity, Temperature, N_indicator)
fct_compute_pca(data_PCA = data_PCA_gironde,
                data_PCA_numerical = data_pca_gironde_numerical,
                path = "inst/results/data_phychem/PCA/Gironde/")

data_PCA_loire <- data_PCA |> filter(estuary == "Loire")
data_pca_loire_numerical <- data_PCA_loire |>
  dplyr::select(Flow, Salinity, Temperature, N_indicator)
fct_compute_pca(data_PCA = data_PCA_loire,
                data_PCA_numerical = data_pca_loire_numerical,
                path = "inst/results/data_phychem/PCA/Loire/")

data_PCA_seine <- data_PCA |> filter(estuary == "Seine")
data_pca_seine_numerical <- data_PCA_seine |>
  dplyr::select(Flow, Salinity, Temperature, N_indicator)
fct_compute_pca(data_PCA = data_PCA_seine,
                data_PCA_numerical = data_pca_seine_numerical,
                path = "inst/results/data_phychem/PCA/Seine/")


# =====================================================
# 3. MFA (Multiple Factorial Analysis)
# =====================================================

data_MFA_full_wide <- data_PCA_full |>
  pivot_wider(
    names_from = estuary,
    values_from = c(Flow, N_indicator, Salinity, Temperature),
    names_sep = "_"
  ) |>
  mutate(DECADE = as.factor(DECADE)) |>
  # reorder for MFA
  select(
    DECADE,
    # Flow
    Flow_Gironde, Flow_Loire, Flow_Seine,
    # N_indicator
    N_indicator_Gironde, N_indicator_Loire, N_indicator_Seine,
    # Salinity
    Salinity_Gironde, Salinity_Loire, Salinity_Seine,
    # Temperature
    Temperature_Gironde, Temperature_Loire, Temperature_Seine
  )


res_mfa <- MFA(
  data_MFA_full_wide |>  select(-DECADE),
  group = c(3, 3, 3, 3),  # 4 variables, 3 estuaires
  type = rep("s", 4),  # variables quantitatives
  name.group = c("Flow", "N_indicator", "Salinity", "Temperature"),
  graph = FALSE
)

# Individuals (decades)
fviz_mfa_ind(
  res_mfa,
  habillage = data_MFA_full_wide$DECADE,
  palette = "Dark2",
  addEllipses = FALSE,
  repel = TRUE
)

coords <- as.data.frame(res_mfa$ind$coord)
coords$DECADE <- data_MFA_full_wide$DECADE

ggplot(coords, aes(x = Dim.1, y = Dim.2)) +
  geom_point(size = 3) +
  geom_path(aes(group = 1), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(aes(label = DECADE), vjust = -1) +
  theme_minimal()

fviz_mfa_var(res_mfa, "group")
