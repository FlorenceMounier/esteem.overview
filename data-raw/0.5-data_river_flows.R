# =====================================================
# Preparation script
# Datasets:
#  - data_flow.rda
# Author: FM
# Date: 2026-06-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(tidyverse, quietly = TRUE)


# =====================================================
# 01. Loire
# =====================================================

# Daily dataset
data_flow_loire_daily <- read_csv("../DATA débits/Data débits journaliers/MontjeanSurLoire.csv")

# Monthly dataset from daily dataset
data_flow_loire <- data_flow_loire_daily |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Loire") |>
  mutate(PARAMETER_LIBELLE = "Flow")

# =====================================================
# 02. Seine
# =====================================================

# --- DAILY DATASET ----

# Load daily dataset
data_flow_seine_daily <- read_csv("../DATA débits/Data débits journaliers/Vernon_Seine.csv")

# Monthly dataset from daily dataset
data_flow_seine_monthly_means <- data_flow_seine_daily |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Loire")


# ---- MONTHLY DATASET ----

# Load monthly dataset
data_flow_seine_monthly_data <- read_csv("../DATA débits/Data débits mensuels moyens/H320000101_VernonSeine.csv")

data_flow_seine_autumn_monthly <- data_flow_seine_monthly |>
  get_info_from_dates(date_variable = `Date (TU)`) |>
  group_by(year, month, year_month, season) |>
  summarise(RESULTAT = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(estuary = "Seine")


# ---- FULL DATASET ----

data_flow_seine <- data_flow_seine_monthly_means |>
  full_join(data_flow_seine_monthly_data)

ggplot(data_flow_seine_autumn) +
  aes(x = year, y = RESULTAT, colour = season) +
  geom_point()

# =====================================================
# 03. Gironde
# =====================================================

### DAILY DATASET ###


# ---- Dordogne daily flow ----

data_flow_dordogne_daily <- read_csv("../DATA débits/Data débits journaliers/PessacSurDordogne.csv") |>
  select(`Date (TU)`, `Valeur (en m³/s)`) |>
  mutate(estuary = "Dordogne")

# Monthly dataset from daily dataset
data_flow_dordogne_monthly_means <- data_flow_dordogne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`, na.rm = TRUE))


# Sept-Oct mean annual flow
data_flow_dordogne_autumn_monthly_means <- data_flow_dordogne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Dordogne")


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

data_flow_garonne_daily <- fun_correct_csv(path = "../DATA débits/Data débits journaliers/Garonne Tonneins.csv") |>
select(Date, Valeur_m3_per_s) |>
mutate(Valeur_m3_per_s = as.numeric(Valeur_m3_per_s)) |>
mutate(zone = "Garonne")
names(data_flow_garonne_daily) <- names(data_flow_dordogne_daily)

# Monthly dataset from daily dataset
data_flow_garonne_monthly_means <- data_flow_garonne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`, na.rm = TRUE))


# Sept-Oct mean annual flow
data_flow_garonne_autumn_monthly_means <- data_flow_garonne_daily |>
  mutate(year = year(`Date (TU)`)) |>
  mutate(month = month(`Date (TU)`)) |>
  filter(month %in% c(9, 10)) |>
  group_by(year) |>
  summarise(debit = mean(`Valeur (en m³/s)`), .groups = "drop") |>
  drop_na() |>
  mutate(zone = "Garonne")


# ---- Gironde daily flow ----

data_flow_gironde_autumn_monthly_means <- data_flow_garonne_autumn_monthly_means |>
  full_join(data_flow_dordogne_autumn_monthly_means) |>
  pivot_wider(names_from = zone, values_from = debit) |>
  mutate(debit = Garonne + Dordogne) |>
  drop_na() |>
  select(-c(Garonne, Dordogne)) |>
  mutate(zone = "Gironde")


### MONTHLY DATASET ###

data_flow_gironde_monthly <- read_delim(
  "../DATA débits/Data débits mensuels moyens/DebitGirondeEDF_1963-2013.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

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

ggplot(data_flow_gironde_autumn) +
  aes(x = year, y = debit, colour = source) +
  geom_point()

# =====================================================
# 04. All estuaries
# =====================================================

data_flow <- data_flow_gironde |>
  full_join(data_flow_loire) |>
  full_join(data_flow_seine) |>
  group_by(year, zone) |>
  summarise(debit = mean(debit, na.rm = TRUE)) |>
  pivot_wider(names_from = zone, values_from = debit) |>
  drop_na() |>
  pivot_longer(cols = c(Gironde, Loire, Seine), names_to = "estuary", values_to = "RESULTAT") |>
  mutate(PARAMETRE_LIBELLE = "Flow") |>
  mutate(estuary = as.factor(estuary))


ggplot_data_flow_autumn <- ggplot(data_flow_autumn) +
  aes(x = year, y = RESULTAT, colour = estuary) +
  geom_line() +
  facet_grid(cols = vars(estuary), scales = "free_y")

ggsave(plot = ggplot_data_flow_autumn, filename = "inst/mat_meth/ggplot_data_flow_autumn.jpg",
       width = 8, height = 4)
