# =====================================================
# Preparation script for intertidal areas data
# Datasets:
#  - data_intertidal_surface_interp.rda
# Graphs in /inst/mat_meth/intertidal_surface/ :
#  - ggplot_intertidal_surface.jpg
# Author: FM
# Date: 2026-06-30
# =====================================================


# =====================================================
# 00. Packages and raw data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)

# raw_data_int_surf_seine_loire <- readxl::read_xlsx("../Intertidal areas/lecuyer2024_loire_seine.xlsx") |>
#   filter(secteur %in% c("limnique", "oligohalin", "mesohalin", "polyhalin", "euhalin"))
# usethis::use_data(raw_data_int_surf_seine_loire)
data(raw_data_int_surf_seine_loire)

# raw_data_int_surf_gironde <- readr::read_csv2("../Intertidal areas/sottolichio2013_gironde.csv")
# usethis::use_data(raw_data_int_surf_gironde)
data(raw_data_int_surf_gironde)



# =====================================================
# 1. Prepare dataset for Loire & Seine
# =====================================================

# ---- Raw data from Lecuyer et al. 2024 ("Thriving life beneath...") ----
# expressed in hectares

data_int_surf_seine_loire <- raw_data_int_surf_seine_loire  |>
  mutate(haline_zone = case_when(
    secteur %in% c("limnique", "oligohalin", "mesohalin") ~ "inner",
    secteur %in% c("polyhalin", "euhalin") ~ "outer"
  )) |>
  group_by(estuaire, annee, haline_zone) |>
  summarise(
    across(
      .cols = surface_ha,
      .fns = ~ sum(.x, na.rm = TRUE)
    ), .groups = "drop"
  ) |>
  mutate(annee = as.numeric(annee)) |>
  mutate(PROGRAMME = "Lecuyer et al. 2024")  |>
  mutate(PARAMETRE_LIBELLE = "Surface area") |>
  rename(RESULTAT = surface_ha)


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
#   **exondé + subtidal + intertidal**
#   with:
#   **intertidal = euhalin + polyhalin + mesohalin + oligohalin + limnique**
#
#   These surface areas were calculated from digitizations that I carried out using various historical cartographic sources.



# =====================================================
# 2. Prepare dataset for Gironde
# =====================================================

# ---- Raw data from Sottolichio et al. 2013 ----
# expressed in km2

data_int_surf_gironde <- raw_data_int_surf_gironde  |>
  mutate(haline_zone = case_when(
    section %in% c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70") ~ "inner",
    section %in% c("70-80", "80-90", "90-100") ~ "outer"
  )) |>
  group_by(haline_zone) |>
  summarise(
    across(
      .cols = -section,
      .fns = ~ sum(.x, na.rm = TRUE) * 100 # to hectars
    )
  ) |>
  mutate(estuaire = "gironde") |>
  pivot_longer(cols = -c(estuaire, haline_zone), names_to = "annee", values_to = "surface_ha") |>
  mutate(annee = as.numeric(annee)) |>
  mutate(PROGRAMME = "Sottolichio et al. 2013")  |>
  mutate(PARAMETRE_LIBELLE = "Surface area") |>
  rename(RESULTAT = surface_ha)



# =====================================================
# 2. Join datasets from all estuaries
# =====================================================

# Compile data from all estuaries and renames variables and values

# expressed in hectares
data_intertidal_surface <- full_join(
  data_int_surf_seine_loire,
  data_int_surf_gironde
) |>
  rename(estuary = estuaire,
         year = annee) |>
  mutate(estuary = case_when(
    estuary == "gironde" ~ "Gironde",
    estuary == "loire" ~ "Loire",
    estuary == "seine" ~ "Seine"
  ))



# =====================================================
# 3. Interpolations
# =====================================================

# ---- Compute interpolated data ----

data_intertidal_surface_interp <- get_interpolation_spline(
  data = data_intertidal_surface,
  x = year,
  y = RESULTAT,
  estuary, haline_zone) |>
  left_join(data_intertidal_surface)

usethis::use_data(data_intertidal_surface_interp, overwrite = TRUE)



# =====================================================
# 4. M & M graph
# =====================================================

# ---- Graph ----

ggplot_intertidal_surface <- ggplot(
  data = data_intertidal_surface_interp) +
  aes(x = year, y = RESULTAT, colour = estuary, shape = haline_zone, linetype = haline_zone) +
  geom_line() +
  geom_point(data = data_intertidal_surface_interp |> drop_na(),
             aes(x = year, y = RESULTAT, colour = estuary))

ggplot_intertidal_surface

ggsave(plot = ggplot_intertidal_surface,
       filename = "inst/mat_meth/intertidal_surface/ggplot_raw_intertidal_areas.jpg")
