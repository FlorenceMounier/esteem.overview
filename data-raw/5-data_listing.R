
plan_REBENT <- data_REBENT_biota |>
  distinct(YEAR, estuary) |>
  rename(year = YEAR) |>
  mutate(parameter = "REBENT", value = 1)

plan_DCE <- data_POMET_densities |>
  filter(saison == "automne") |>
  distinct(annee, estuary) |>
  rename(year = annee) |>
  mutate(parameter = "DCE", value = 1)


plan_physico_chem <- data_physchem |>
  filter(!is.na(estuary)) |>
  filter(!is.na(haline_zone)) |>
  filter(PARAMETRE_LIBELLE %in% c("Oxygène dissous", "Salinité", "Phosphate", "Silicate", "Température de l'eau")) |>
  distinct(PARAMETRE_LIBELLE, year, estuary) |>
  rename(parameter = PARAMETRE_LIBELLE) |>
  mutate(value = 1)

plan <- rbind(plan_REBENT, plan_DCE, plan_physico_chem)

complete_plan <- plan |>
  complete(year, estuary, parameter, fill = list(value = 0)) |>
  mutate(value = as.factor(value)) |>
  mutate(fill_estuary = ifelse(value == 0, NA, estuary)) |>
  drop_na()

ggplot(complete_plan, aes(x = year, y = parameter, fill = fill_estuary)) +
  geom_tile(position = position_dodge(width = 0.9),
            color = "grey80") +
    scale_fill_manual(values = c(
      "Gironde" = "#1b9e77",
      "Loire" = "#d95f02",
      "Seine" = "#7570b3"
    )) +
  theme_minimal() +
  labs(x = "Année",
       y = NULL,
       fill = "Estuaire",
       title = "Disponibilité des paramètres par année et estuaire") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

