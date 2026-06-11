library(tidyverse)
library(esteem.overview)

plan_temp <- esteem.overview::data_temp |>
  distinct(year, estuary) |>
  mutate(parameter = "temperature", value = 1)

plan_salin <- esteem.overview::data_salin |>
  distinct(year, estuary) |>
  mutate(parameter = "salinity", value = 1)

plan_nitro <- esteem.overview::data_nitro |>
  distinct(year, estuary) |>
  mutate(parameter = "nitro. index", value = 1)

plan_surface <- esteem.overview::data_intertidal_surface |>
  filter(annee >= 1970) |>
  select(-surface_ha) |>
  rename(year = annee) |>
  rename(estuary = estuaire) |>
  mutate(estuary = str_to_title(estuary)) |>
  mutate(parameter = "surface", value = 1)

plan_flow <- esteem.overview::data_flow_autumn |>
  select(-RESULTAT) |>
  rename(parameter = PARAMETRE_LIBELLE) |>
  mutate(parameter = stringr::str_to_lower(parameter)) |>
  mutate(value = 1)

plan_ROCCHMV <- esteem.overview::data_ROCCHMV_cleaned |>
  distinct(YEAR, estuary) |>
  rename(year = YEAR) |>
  mutate(parameter = "leg. contam.", value = 1)

plan_emergentsea <- tribble(
  ~estuary, ~year, ~parameter, ~value,
  "Loire", 2021, "emerg. contam.", 1,
  "Loire", 2022, "emerg. contam.", 1,
  "Seine", 2021, "emerg. contam.", 1,
  "Seine", 2022, "emerg. contam.", 1,
  "Gironde", 2021, "emerg. contam.", 1,
  "Gironde", 2022, "emerg. contam.", 1
)

plan_REBENT <- esteem.overview::data_REBENT_biota |>
  distinct(YEAR, estuary) |>
  rename(year = YEAR) |>
  mutate(parameter = "benthos", value = 1)

plan_DCE <- esteem.overview::data_POMET_densities |>
  filter(saison == "automne") |>
  distinct(annee, estuary) |>
  rename(year = annee) |>
  mutate(parameter = "fish", value = 1)

plan <- full_join(plan_REBENT, plan_DCE) |>
  full_join(plan_surface) |>
  full_join(plan_flow) |>
  full_join(plan_ROCCHMV) |>
  full_join(plan_emergentsea) |>
  full_join(plan_temp) |>
  full_join(plan_salin) |>
  full_join(plan_nitro)

ordre_param <- c(
  "surface",
  "flow",
  "temperature",
  "salinity",
  "nitro. index",
  "leg. contam.",
  "emerg. contam.",
  "benthos",
  "fish"
)

complete_plan <- plan |>
  complete(year, estuary, parameter, fill = list(value = 0)) |>
  mutate(value = as.factor(value)) |>
  mutate(fill_estuary = ifelse(value == 0, NA, estuary)) |>
  drop_na() |>
  mutate(
    parameter = factor(parameter, levels = rev(ordre_param))
  )

ggplot_data_design <- ggplot(complete_plan, aes(x = year, y = parameter, fill = fill_estuary)) +
  geom_tile(position = position_dodge(width = 0.9),
            color = "grey80") +
    scale_fill_manual(values = c(
      "Gironde" = "#1b9e77",
      "Loire" = "#d95f02",
      "Seine" = "#7570b3"
    )) +
  facet_wrap(vars(estuary), ncol = 1) +
  theme_minimal() +
  labs(x = NULL,
       y = NULL,
       fill = "Estuary") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot = ggplot_data_design, filename = "inst/mat_meth/ggplot_data_design.jpg")
