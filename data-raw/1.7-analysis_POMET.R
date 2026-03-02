
# =====================================================
# 0. Libraries
# =====================================================

library(vegan)
library(tidyverse)
# remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan)

`%!in%` = Negate(`%in%`)

# =====================================================
# 1. PCA
# =====================================================

data_POMET_sole <- data_POMET_densities |>
  filter(name %in% c("Solea solea")) |>
  select(trait_id, annee, saison, estuary, Densite, temperature, haline_zone) |>
  drop_na() |>
  group_by(annee, saison, estuary, haline_zone) |>
  summarise(mean_Densite = mean(Densite, na.rm = TRUE),
            mean_temperature = mean(temperature, na.rm = TRUE), .groups = "drop")

# ---- PCA ----
data_acp <- data_POMET_sole |>
  select(mean_Densite, mean_temperature) |>
  scale()

acp <- prcomp(data_acp, center = TRUE, scale. = TRUE)

# Scores des observations
scores <- as.data.frame(acp$x)
scores$annee   <- data_POMET_sole$annee
scores$estuary <- data_POMET_sole$estuary
scores$haline_zone  <- data_POMET_sole$haline_zone
scores$saison  <- data_POMET_sole$saison
scores <- scores |> arrange(estuary, annee)

# Loadings (variables)
loadings <- as.data.frame(acp$rotation)
loadings$Variable <- rownames(loadings)

# ---- Biplot ----
mult <- 3
var_expl <- summary(acp)$importance[2, 1:2] * 100

ggplot() +
  # Points
  geom_point(data = scores,
             aes(PC1, PC2, color = estuary),
             alpha = 0.7) +

  # Flèches
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * mult,
                   yend = PC2 * mult),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +

  # Labels des variables
  geom_text(data = loadings,
            aes(x = PC1 * mult,
                y = PC2 * mult,
                label = Variable),
            vjust = -0.5) +

  coord_equal() +
  theme_minimal() +
  labs(title = "Biplot ACP",
       x = "PC1",
       y = "PC2") +
  labs(
    x = paste0("PC1 (", round(var_expl[1], 1), "%)"),
    y = paste0("PC2 (", round(var_expl[2], 1), "%)")
  )




# ---- Ellipses par estuaire ----
ggplot(scores, aes(PC1, PC2)) +
  geom_point(aes(color = estuary), alpha = 0.7) +
  stat_ellipse(aes(group = estuary, color = estuary),
               type = "norm",
               level = 0.95) +
  theme_minimal() +
  labs(title = "ACP - Ellipses par estuaire")

# ---- Ellipses par zones halines ----
ggplot(scores, aes(PC1, PC2)) +
  geom_point(aes(color = haline_zone), alpha = 0.7) +
  stat_ellipse(aes(group = haline_zone, color = haline_zone),
               type = "norm",
               level = 0.95) +
  theme_minimal() +
  labs(title = "ACP - Ellipses par zone haline")

# ---- Ellipses par saison ----
ggplot(scores, aes(PC1, PC2)) +
  geom_point(aes(color = saison), alpha = 0.7) +
  stat_ellipse(aes(group = saison, color = saison),
               type = "norm",
               level = 0.95) +
  theme_minimal() +
  labs(title = "ACP - Ellipses par saison")

# ---- Trajectoires annuelles ----

# Centres annuels (trajectoire)
centroids <- scores |>
  group_by(estuary, annee) |>
  summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    .groups = "drop"
  ) |>
  arrange(estuary, annee)
centroids$annee_num <- as.numeric(as.character(centroids$annee))

ggplot() +

  # Points bruts
  geom_point(data = scores,
             aes(PC1, PC2), color = "grey85",
             alpha = 0.3) +

  # Trajectoires
  geom_path(data = centroids,
            aes(PC1, PC2, group = estuary), color = "grey50",
            linewidth = 1.2) +

  # Centres annuels
  geom_point(data = centroids,
             aes(PC1, PC2, color = annee_num),
             size = 3) +
  # geom_text(data = centroids,
  #           aes(PC1, PC2, label = annee, color = estuary),
  #           vjust = -1,
  #           size = 4) +
  scale_color_viridis_c(option = "plasma") +

  # Flèches des variables
  geom_segment(data = loadings,
               aes(x = 0, y = 0,
                   xend = PC1 * mult,
                   yend = PC2 * mult),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +

  # Labels variables
  geom_text(data = loadings,
            aes(x = PC1 * mult,
                y = PC2 * mult,
                label = Variable),
            vjust = -0.5) +

  coord_equal() +
  theme_minimal() +
  labs(
    title = "ACP - Trajectoires interannuelles par estuaire",
    x = paste0("PC1 (", round(var_expl[1],1), "%)"),
    y = paste0("PC2 (", round(var_expl[2],1), "%)")
  ) +
  facet_wrap(~ estuary)


# =====================================================
# 1. RDA pour vision globale structure de communauté
# =====================================================

# Les assemblages piscicoles varient-ils selon estuaire, salinité, saison, température ?
# => Analyse multivariée (RDA: Redundancy Analysis)

# ---- 1.1. Préparer les données communauté ----

# Traits sans espèces identifiées
trait_id_zero <- data_POMET_ALL_densities |>
  group_by(trait_id, nt) |>
  summarise(ntt = sum(nt, na.rm = TRUE)) |>
  filter(ntt == 0) |>
  pull(trait_id)

# Matrice espèces x échantillons
communities <- data_POMET_ALL_densities |>
  filter(trait_id %!in% trait_id_zero) |>
  filter(saison != "ete") |>
  drop_na(name, temperature) |>
  select(trait_id, annee, name, Densite) |>
  pivot_wider(names_from = name,
              values_from = Densite,
              values_fill = 0)

# Variables environnementales
env <- data_POMET_ALL_densities |>
  filter(trait_id %!in% trait_id_zero) |>
  filter(saison != "ete") |>
  drop_na(name, temperature) |>
  select(trait_id, estuary, temperature, saison, haline_zone) |>
  distinct() |>
  drop_na(temperature)


# ---- 1.2. Transformation Hellinger ----

comm_hell <- vegan::decostand(communities[,-1], method = "hellinger")
# vegan::decorana(communities) # < 3 => RDA et pas CCA

# ---- 1.3. RDA ----

# RDA : Structure de la communauté expliquée par l’environnement
# RDA = régression multivariée + PCA
# 1. On fait une régression multivariée : communauté ~ environnement
# 2. On fait une PCA sur les valeurs ajustées

# Interprétation RDA : PCA contrainte par des variables environnementales
# - % variance expliquée
# - Gradient principal = salinité ?

rda_mod <- vegan::rda(comm_hell ~ estuary + haline_zone + temperature + saison +
                        temperature:estuary + haline_zone:estuary,
               data = env)

# Proportion de variance expliquée
summary(rda_mod)$cont$importance

# ---- Diagramme de base -----

# Scores
sites   <- as.data.frame(scores(rda_mod, display="sites"))
species <- as.data.frame(scores(rda_mod, display="species"))
env     <- as.data.frame(scores(rda_mod, display="bp"))

# Ajouter les noms pour les labels
sites$Label <- rownames(sites)
species$Label <- rownames(species)
env$Label <- rownames(env)

# Graphique RDA1 x RDA2
ggplot() +
  # Points des sites
  geom_point(data=sites, aes(x=RDA1, y=RDA2), color="black", size = 0.5) +
  # Flèches des variables dépendantes
  geom_segment(data=species, aes(x=0, y=0, xend=RDA1, yend=RDA2),
               arrow=arrow(length=unit(0.2,"cm")), color="red") +
  geom_text(data=species, aes(x=RDA1, y=RDA2, label=Label),
            color="red", vjust=-0.5) +
  # Flèches des variables explicatives
  geom_segment(data=env, aes(x=0, y=0, xend=RDA1, yend=RDA2),
               arrow=arrow(length=unit(0.2,"cm")), color="blue") +
  geom_text(data=env, aes(x=RDA1, y=RDA2, label=Label),
            color="blue", vjust=-0.5) +
  coord_equal() +
  theme_minimal() +
  labs(title="RDA - diagramme biplot", x="RDA1 (16.5%)", y="RDA2 (5.5%)")

# Tests de significativité => Donne importance relative des facteurs.
anova(rda_mod, permutations = 999)
anova(rda_mod, by = "term", permutations = 999)
plot(rda_mod, scaling = 2)


# Espèces contributrices
scores(rda_mod, display = "species") |>
  as.tibble() |>
  arrange(RDA1)
# Les espèces les plus corrélées aux axes structurants => Ce sont elles que tu modélises en GLMM.


# Variation partitioning
# => Qui explique le plus : environnement ou estuaire ?
varpart(comm_hell,
        ~ haline_zone + temperature + saison,
        ~ estuary,
        data = env)

# =====================================================
# 3. GLMM espèce par espèce Gamma log-link
# =====================================================

# GLMM: Réponse spécifique d’une espèce à l’environnement.
# Quelles espèces conduisent ces différences ?
# Comment chaque espèce répond-elle au gradient ?

library(glmmTMB)
library(DHARMa)
library(performance)
# Effets fixes (interprétables) : estuaire, annee, zone haline, température, saison
# Effets aléatoires (structure des données) : trait_id et année

# ---- Sole ----
data_POMET_densities_sole <- data_POMET_densities |>
  filter(name == "Solea solea") |>
  select(trait_id, annee, saison, estuary, Densite, temperature, haline_zone) |>
  drop_na()
hist(data_POMET_densities_sole$Densite, breaks = 50)

mod_sole <- glmmTMB(
  Densite ~ estuary + (1 | estuary:annee) + saison + temperature + haline_zone +
     (1 | estuary:trait_id),
  family = Gamma(link = "log"),
  data = data_POMET_densities_sole
)
summary(mod_sole)
sim_res <- simulateResiduals(mod_sole)
plot(sim_res)
performance::check_collinearity(mod_sole)

# ---- Seabass ----
data_POMET_densities_seabass <- data_POMET_densities |>
  filter(name == "Dicentrarchus labrax") |>
  select(trait_id, annee, saison, estuary, Densite, temperature, haline_zone) |>
  drop_na()
hist(data_POMET_densities_seabass$Densite, breaks = 50)

mod_seabass <- glmmTMB(
  Densite ~ estuary + (1 | estuary:annee) + saison + temperature + haline_zone +
    (1 | estuary:trait_id),
  family = Gamma(link = "log"),
  data = data_POMET_densities_seabass
)
summary(mod_seabass)
sim_res_seabass <- simulateResiduals(mod_seabass)
plot(sim_res_seabass)
performance::check_collinearity(mod_seabass)

# =====================================================
# 4. Graphiques prédictifs (ggeffects)
# =====================================================
