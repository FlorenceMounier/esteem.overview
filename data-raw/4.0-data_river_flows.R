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

# Daily dataset
data_flow_loire_daily <- read_csv("../DATABASES/DATA débits/Data débits journaliers/MontjeanSurLoire.csv") |>
  mutate(zone = "Loire")
usethis::use_data(data_flow_loire_daily, overwrite = TRUE)

# Monthly dataset from daily dataset
data_flow_loire_monthly_means <- data_flow_loire_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year, month) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Loire") |>
  mutate(source = "monthly_means")
usethis::use_data(data_flow_loire_monthly_means, overwrite = TRUE)

# Sept-Oct mean annual flow
data_flow_loire_autumn <- data_flow_loire_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Loire") |>
  mutate(source = "monthly_means")
usethis::use_data(data_flow_loire_autumn, overwrite = TRUE)


# =====================================================
# 02. Seine
# =====================================================

### DAILY DATASET ###

# Daily dataset
data_flow_seine_daily <- read_csv("../DATABASES/DATA débits/Data débits journaliers/Vernon_Seine.csv") |>
  mutate(zone = "Seine")
usethis::use_data(data_flow_seine_daily, overwrite = TRUE)

# Monthly dataset from daily dataset
data_flow_seine_monthly_means <- data_flow_seine_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year, month) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Loire")
usethis::use_data(data_flow_seine_monthly_means, overwrite = TRUE)

# Sept-Oct mean annual flow
data_flow_seine_autumn_monthly_means <- data_flow_seine_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Seine")
usethis::use_data(data_flow_seine_autumn_monthly_means, overwrite = TRUE)


### MONTHLY DATASET ###

# Monthly dataset
data_flow_seine_monthly <- read_csv("../DATABASES/DATA débits/Data débits mensuels moyens/H320000101_VernonSeine.csv") |>
    mutate(zone = "Seine")
usethis::use_data(data_flow_seine_monthly, overwrite = TRUE)

# Sept-Oct mean annual flow
data_flow_seine_autumn_monthly <- data_flow_seine_monthly |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Seine")
usethis::use_data(data_flow_seine_autumn_monthly, overwrite = TRUE)


### FULL AUTUMN DATASET ###

data_flow_seine_autumn <- data_flow_seine_autumn_monthly |> mutate(source = "monthly") |>
  full_join(data_flow_seine_autumn_monthly_means |> mutate(source = "monthly_means"))
usethis::use_data(data_flow_seine_autumn, overwrite = TRUE)

ggplot(data_flow_seine_autumn) +
  aes(x = year, y = debit, colour = source) +
  geom_point()

# =====================================================
# 03. Gironde
# =====================================================

### DAILY DATASET ###


# ---- Dordogne daily flow ----

data_flow_dordogne_daily <- read_csv("../DATABASES/DATA débits/Data débits journaliers/PessacSurDordogne.csv") |>
  select(`Date (TU)`, `Valeur (en m³/s)`) |>
  mutate(zone = "Dordogne")
usethis::use_data(data_flow_dordogne_daily, overwrite = TRUE)

# Monthly dataset from daily dataset
data_flow_dordogne_monthly_means <- data_flow_dordogne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`, na.rm = TRUE))
usethis::use_data(data_flow_dordogne_monthly_means, overwrite = TRUE)


# Sept-Oct mean annual flow
data_flow_dordogne_autumn_monthly_means <- data_flow_dordogne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Dordogne")
usethis::use_data(data_flow_dordogne_autumn_monthly_means, overwrite = TRUE)


# ---- Garonne daily flow ----

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

data_flow_garonne_daily <- fun_correct_csv(path = "../DATABASES/DATA débits/Data débits journaliers/Garonne Tonneins.csv") |>
select(Date, Valeur_m3_per_s) |>
mutate(Valeur_m3_per_s = as.numeric(Valeur_m3_per_s)) |>
mutate(zone = "Garonne")
names(data_flow_garonne_daily) <- names(data_flow_dordogne_daily)
usethis::use_data(data_flow_garonne_daily, overwrite = TRUE)

# Monthly dataset from daily dataset
data_flow_garonne_monthly_means <- data_flow_garonne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`, na.rm = TRUE))
usethis::use_data(data_flow_garonne_monthly_means, overwrite = TRUE)


# Sept-Oct mean annual flow
data_flow_garonne_autumn_monthly_means <- data_flow_garonne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Garonne")
usethis::use_data(data_flow_garonne_autumn_monthly_means, overwrite = TRUE)


# ---- Gironde daily flow ----

data_flow_gironde_autumn_monthly_means <- data_flow_garonne_autumn_monthly_means |>
  full_join(data_flow_dordogne_autumn_monthly_means) |>
  pivot_wider(names_from = zone, values_from = debit) |>
  mutate(debit = Garonne + Dordogne) |>
  drop_na() |>
  select(-c(Garonne, Dordogne)) |>
  mutate(zone = "Gironde")
usethis::use_data(data_flow_gironde_autumn_monthly_means, overwrite = TRUE)


### MONTHLY DATASET ###

data_flow_gironde_monthly <- read_delim(
  "../DATABASES/DATA débits/Data débits mensuels moyens/DebitGirondeEDF_1963-2013.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)
usethis::use_data(data_flow_gironde_monthly, overwrite = TRUE)

data_flow_gironde_autumn_monthly <- data_flow_gironde_monthly |>
  filter(mois %in% c(9,10)) |>
  group_by(Annee) |>
  summarise(debit = mean(`MoyMensDébit Total`, na.rm = TRUE)) |>
  rename(year = Annee) |>
  mutate(zone = "Gironde")
usethis::use_data(data_flow_gironde_autumn_monthly, overwrite = TRUE)


### FULL AUTUMN DATASET ###

data_flow_gironde_autumn <- data_flow_gironde_autumn_monthly |> mutate(source = "monthly") |>
  full_join(data_flow_gironde_autumn_monthly_means |> mutate(source = "monthly_means"))
usethis::use_data(data_flow_gironde_autumn, overwrite = TRUE)

ggplot(data_flow_gironde_autumn) +
  aes(x = year, y = debit, colour = source) +
  geom_point()

# =====================================================
# 04. All estuaries
# =====================================================

data_flow_autumn <- data_flow_gironde_autumn |>
  full_join(data_flow_loire_autumn) |>
  full_join(data_flow_seine_autumn) |>
  group_by(year, zone) |>
  summarise(debit = mean(debit, na.rm = TRUE)) |>
  pivot_wider(names_from = zone, values_from = debit) |>
  drop_na() |>
  pivot_longer(cols = c(Gironde, Loire, Seine), names_to = "estuary", values_to = "RESULTAT") |>
  mutate(PARAMETRE_LIBELLE = "Flow") |>
  mutate(estuary = as.factor(estuary))
usethis::use_data(data_flow_autumn, overwrite = TRUE)


ggplot(data_flow_autumn) +
  aes(x = year, y = RESULTAT, colour = estuary) +
  geom_line() +
  facet_grid(cols = vars(estuary), scales = "free_y")
