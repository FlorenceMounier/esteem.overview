# =====================================================
# Preparation script from external intertidal areas raw data
# Datasets:
#  - data_intertidal_surface.rda
# Author: FM
# Date: 2026-06-15
# =====================================================


# =====================================================
# 00. Packages, functions and raw data
# =====================================================

library(tidyverse)
library(readxl)


# =====================================================
# 1. Datasets for intertidal surface area
# =====================================================

# ---------------------------------------------------------------------------
# Read raw data from Lecuyer et al. 2024 ("Thriving life beneath...") Loire, Seine ----
# expressed in hectares

raw_data_intertidal_surface_seine_loire <- readxl::read_xlsx("../Intertidal areas/lecuyer2024_loire_seine.xlsx") |>
  filter(secteur %in% c("limnique", "oligohalin", "mesohalin", "polyhalin", "euhalin"))

data_intertidal_surface_area_seine_loire <- raw_data_intertidal_surface_seine_loire |>
  group_by(estuaire, annee) |>
  summarise(
    across(
      .cols = surface_ha,
      .fns = ~ sum(.x, na.rm = TRUE)
    ), .groups = "drop"
  ) |>
  mutate(annee = as.numeric(annee))


# E-mail from R. Lecuyer 04/07/2026
# The **"année"** field sometimes corresponds to time periods rather than specific years, particularly for the oldest records (for example, the *État-Major* maps are not assigned to a precise year).
#
# The **"secteur"** field indicates the area that was quantified (expressed in hectares in my file):
#
# - **"total"** is simply the cumulative area, i.e., the surface area of the overall envelope encompassing all polygons for a given year.
# - By elevation zone (**emerged > intertidal > subtidal**), the **"emerged"** category corresponds to portions that are no longer underwater (mainly due to lateral compression caused by diking, land reclamation, infilling, etc.). This category is particularly useful for quantifying coastal squeeze. Its value is set to zero for the oldest period in each estuary, as I considered that period to represent the initial state. For subsequent years, the **"emerged"** value is simply the difference between the initial area and the remaining area.
#
# I also subdivided the **intertidal** zone according to the salinity domain (**euhalin > polyhalin > mesohalin > oligohalin > limnique**), corresponding to the gradient shown in the figure above (the blue bar beneath each map).
#
# In summary, for a given estuary and year, the **"total"** value can be obtained by summing:
#
#   **exondé + subtidal + intertidal**
#
#   with:
#
#   **intertidal = euhalin + polyhalin + mesohalin + oligohalin + limnique**
#
#   These surface areas were calculated from digitizations that I carried out using various historical cartographic sources.


# ---------------------------------------------------------------------------
# Read raw data from Sottolichio et al. 2013 Gironde estuary ----

# expressed in km2
raw_data_intertidal_surface_gironde_sottolichio <- readr::read_csv2("../Intertidal areas/sottolichio2013_gironde.csv")

data_intertidal_surface_gironde_sottolichio <- raw_data_intertidal_surface_gironde_sottolichio  |>
  summarise(
    across(
      .cols = -section,
      .fns = ~ sum(.x, na.rm = TRUE) * 100
    )
  ) |>
  mutate(estuaire = "gironde") |>
  pivot_longer(cols = -estuaire, names_to = "annee", values_to = "surface_ha") |>
  mutate(annee = as.numeric(annee))


# ---------------------------------------------------------------------------
# Read raw data from Blanchet et al. 2018 Gironde estuary ----

# EUNIS classes selection
# selected from A2 "Intertidal sediment" (A = Marine habitats):
# - A2.2 ("Intertidal sand and muddy sand")
# - A2.3 ("Intertidal mud")
# not selected
# - A1 ("Rock and other hard intertidal substrates") => mouth
# - A3 ("Infralittoral rock and other hard substrates") => mouth
# - A5 ("Subtidal sediment") => subtidal
# - A2.1 ("Intertidal coarse sediments") => not present in the estuary
# - A2.4 ("Intertidal mixed sediments") => not present in the estuary
# - A2.5 ("Coastal salt marshes and saline reedbeds") => uppermost level of sheltered shores that are predominantly closed and only periodically inundated, above the shoreline limit

# expressed in hectares
raw_data_intertidal_surface_gironde_blanchet <- readxl::read_xlsx("../Intertidal areas/blanchet2018_gironde.xlsx")


data_intertidal_surface_gironde_blanchet <- raw_data_intertidal_surface_gironde_blanchet |>
  summarise(
    across(
      .cols = -EUNIS_habitat,
      .fns = ~ sum(.x, na.rm = TRUE)
    )
  ) |>
  mutate(estuaire = "gironde") |>
  pivot_longer(cols = -estuaire, names_to = "annee", values_to = "surface_ha") |>
  mutate(annee = as.numeric(annee))


# ---------------------------------------------------------------------------
# Compile data from all estuaries and renames variables and values ----

# expressed in hectares
data_intertidal_surface <- full_join(
  data_intertidal_surface_area_seine_loire,
  data_intertidal_surface_gironde_sottolichio
) |>
  full_join(data_intertidal_surface_gironde_blanchet) |>
  rename(estuary = estuaire,
         year = annee) |>
  mutate(estuary = case_when(
    estuary == "gironde" ~ "Gironde",
    estuary == "loire" ~ "Loire",
    estuary == "seine" ~ "Seine"
  ))


usethis::use_data(data_intertidal_surface, overwrite = TRUE)
