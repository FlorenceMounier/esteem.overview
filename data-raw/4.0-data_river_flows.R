# =====================================================
# Preparation script
# Datasets:
#  - data_flow.rda
# Author: FM
# Date: 2026-03-12
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(tidyverse, quietly = TRUE)

# =====================================================
# 01. Loire
# =====================================================

# data_flow_loire <- read_csv("../DATABASES/DATA débits/MontjeanSurLoire.csv") |>
#   mutate(zone = "Loire")
# usethis::use_data(data_flow_loire, overwrite = TRUE)

data_flow_loire <- data_flow_loire |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Loire")

# =====================================================
# 02. Seine
# =====================================================

# data_flow_seine <- read_csv("../DATABASES/DATA débits/Vernon_Seine.csv") |>
#     mutate(zone = "Seine")
# usethis::use_data(data_flow_seine, overwrite = TRUE)

data_flow_seine <- data_flow_seine |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Seine")

# =====================================================
# 03. Gironde
# =====================================================

# ---- Dordogne ----

# data_flow_dordogne <- read_csv("../DATABASES/DATA débits/PessacSurDordogne.csv") |>
#   select(`Date (TU)`, `Valeur (en m³/s)`) |>
#   mutate(zone = "Dordogne")
# usethis::use_data(data_flow_dordogne, overwrite = TRUE)

# ---- Garonne ----

fun_correct_csv <- function(path, zone_name){
# lire les lignes
lines <- read_lines(path)
# enlever le guillemet début/fin
lines <- str_remove(lines, '^"')
lines <- str_remove(lines, '"$')
# transformer en table
data <- read_csv(
paste(lines, collapse = "\n"),
show_col_types = FALSE
)
data <- data %>%
# enlever guillemets dans les noms de colonnes
rename_with(~ str_remove_all(.x, '"')) %>%
# enlever guillemets restants dans toutes les colonnes texte
mutate(across(where(is.character), ~ str_remove_all(.x, '"')))
# colonne names
names(data) <- c(
"Date",
"Valeur_m3_per_s",
"Statut",
"Qualification",
"Methode",
"Continuite"
)
return(data)
}
# corrupted csv for garonne

# data_flow_garonne <- fun_correct_csv(path = "../DATABASES/DATA débits/Garonne Tonneins.csv") |>
# select(Date, Valeur_m3_per_s) |>
# mutate(Valeur_m3_per_s = as.numeric(Valeur_m3_per_s)) |>
# mutate(zone = "Garonne")
# names(data_flow_garonne) <- names(data_flow_dordogne)
# usethis::use_data(data_flow_garonne, overwrite = TRUE)

data_flow_gironde <- data_flow_garonne |>
  full_join(data_flow_dordogne) |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year, zone) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  pivot_wider(names_from = zone, values_from = debit) |>
  drop_na() |>
  group_by(year) |>
  summarise(debit = sum(Garonne, Dordogne)) |>
  mutate(zone = "Gironde")

# =====================================================
# 04. All estuaries
# =====================================================

data_flow <- data_flow_gironde |>
full_join(data_flow_loire) |>
full_join(data_flow_seine)
usethis::use_data(data_flow, overwrite = TRUE)
