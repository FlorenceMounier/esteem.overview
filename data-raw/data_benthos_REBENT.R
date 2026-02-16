# ---------------------------------------------------------------------------
# Libraries ----

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(writexl)
# Get taxonomy api
library(jsonlite) # install.packages("jsonlite", repos="http://cran.r-project.org")
library(httr) # install.packages("httr")

`%!in%` = Negate(`%in%`)

# ---------------------------------------------------------------------------
# Read raw data from {quadrige.explorer} ----

data_REBENT <- esteem.overview::data_benthos |>
  mutate(YEAR = year(DATE)) |>
  mutate(year_month = paste0(year(DATE), "-", month(DATE))) |>
  mutate(estuary = case_when(
    ZONE_MARINE_QUADRIGE == "085 - Estuaire de la Gironde" ~ "Gironde",
    ZONE_MARINE_QUADRIGE == "070 - Estuaire de la Loire" ~ "Loire",
    ZONE_MARINE_QUADRIGE == "011 - Estuaire de la Seine" ~ "Seine"
  ))


# ---------------------------------------------------------------------------
# Biota

data <- data_REBENT |> filter(PARAMETRE_GROUPE == "Biologie")

# ---------------------------------------------------------------------------
# Campaign period and frequencies ----

# Campains are organized each 3 years, once in autumn, except for 2007:
# april in Seine only, april&may in Gironde as well as october the same year.
data |>
  distinct(estuary, year_month) |>
  arrange(estuary, year_month)

# We keep only autumn:
data <- data |>
  filter(year_month %!in% c("2007-4", "2007-5"))


# ---------------------------------------------------------------------------
# Campaign spatio-temporal measurments ----

data |>
  select(estuary, LIEU_MNEMONIQUE, latitude, longitude) |>
  distinct() |>
  group_by(estuary) |>
  summarise(lat_stat = paste0(min(latitude), " - ", max(latitude)),
            lon_stat = paste0(min(longitude), " - ", max(longitude)))


## => Wrong coordinates for Gironde estuary: latitude = 42.26208 => 45.26208
data <- data |>
  mutate(latitude = case_when(
    latitude == 42.26208 ~ 45.26208,
    TRUE ~ latitude
  ))

## Haline zones
data <- data |>
  mutate(
    haline_zone = case_when(
      estuary == "Gironde" & latitude >= 45.4 ~ "polyhalin",
      estuary == "Gironde" & latitude >= 45.0 ~ "mesohalin",
      estuary == "Loire" & longitude <= -2.0 ~ "polyhalin",
      estuary == "Loire" & longitude <= -1.8 ~ "mesohalin",
      estuary == "Seine" & longitude <= 0.3 ~ "polyhalin",
      estuary == "Seine" & longitude <= 0.5 ~ "mesohalin",
      TRUE ~ NA
    )
  )

# ---------------------------------------------------------------------------
# Types of benthic sampler and their surface ----

# Two types of benthic sampler: intertidal corers & subtidal grab
# - "Benne Smith Mc Intyre": Smith Mc Intyre grab 0.1m²
# - "Benne Van Veen": Van Veen grab 0.1m²
# - "Carottier PVC SIM DCE (0,029 m²)"

# 354 types of PRELEVEMENT_DESCRIPTION
data |> distinct(PRELEVEMENT_DESCRIPTION)

## Extract "Mnémonique", "Engin, "Taille du prélèvement", "Commentaires"

data_full <- data |>
  # sample "TAG"
  mutate(mnemonique = str_extract(PRELEVEMENT_DESCRIPTION, "Mnémonique\\s*:\\s*([^\\-]+)")) |>
  mutate(mnemonique = str_trim(str_remove(mnemonique, "Mnémonique\\s*:\\s*"))) |>
  # sampler type
  mutate(sampler_type = str_extract(PRELEVEMENT_DESCRIPTION, "Engin\\s*:\\s*([^\\-]+)")) |>
  mutate(sampler_type = str_trim(str_remove(sampler_type, "Engin\\s*:\\s*"))) |>
    # sampler surface
  mutate(sampler_surface = str_extract(PRELEVEMENT_DESCRIPTION, "Taille du prélèvement\\s*:\\s*([^\\-]+)")) |>
  mutate(sampler_surface = str_extract(sampler_surface, "\\d+\\.?\\d*") |> as.numeric()) |>
  # comment
  mutate(comment = str_extract(PRELEVEMENT_DESCRIPTION, "Commentaires\\s*:\\s*([^\\-]+)")) |>
  mutate(comment = str_trim(str_remove(comment, "Commentaires\\s*:\\s*")))


## Cleaning of "Engin" and corresponding "Taille du prélèvement"

# Check combination sampler_type & sampler surface
  data_full |>
    count(sampler_type, sampler_surface)
  # => incoherence between sampler type and sampler surface for "011-P-056" in Seine estuary
  # => absence of sampler surface for "Carottier PVC diam. 19 cm (0,028 m²)"

# For which "LIEU_MNEMONIQUE" the sampler type and surface are incoherent?
  data_full |>
    filter(sampler_type == "Carottier PVC diam. 19 cm (0,028 m²)",
           sampler_surface == 0.1) |>
    distinct(ZONE_MARINE_QUADRIGE, LIEU_MNEMONIQUE, DATE)
  # => for "011-P-056" in Seine estuary

# Which distinct sampler_type exist for "011-P-056" in Seine estuary
  data_full |>
    filter(LIEU_MNEMONIQUE == "011-P-056") |>
    distinct(sampler_type)
  # => only "Carottier PVC diam. 19 cm (0,028 m²)" => it is sampler_surface that is wrong

# Correction of sampler surface: "Carottier PVC diam. 19 cm (0,028 m²)" is always "0.028" m²
data_full <- data_full |>
  mutate(sampler_surface = case_when(
    sampler_type == "Carottier PVC diam. 19 cm (0,028 m²)" ~ 0.028,
    TRUE ~ sampler_surface
  )
)


# ---------------------------------------------------------------------------
# Intertidal/Subtidal ----

data_full <- data_full |>
  mutate(tidal = case_when(
    str_detect(sampler_type, pattern = "Carottier") ~ "intertidal",
    str_detect(sampler_type, pattern = "Benne") ~ "subtidal",
    TRUE ~ NA
  ))

# data_full |> count(tidal)


# ---------------------------------------------------------------------------
# Get species taxonomy  ----

## Total: 201 species
# data_full |>  distinct(TAXON_LIBELLE) |> dim()

data_full_taxonomy <- data_full |>
  separate(TAXON_LIBELLE, c("species", "AphiaID"), sep = " \\(") |>
  mutate(AphiaID = AphiaID |> str_remove("AphiaID : "),
         AphiaID = AphiaID |> str_remove("\\)")) |>
  mutate(AphiaID = case_when(
    species == "Nemertea sp2" ~ "152391",
    TRUE ~ AphiaID
  ))

## The vector of AphiaID we wan't to match
AphiaIDToMatch <- data_full_taxonomy |> distinct(AphiaID) |>  pull(AphiaID) |>  as.numeric()
species <- data_full_taxonomy |> distinct(species) |>  pull(species)

## Build the URL to get the data from
url <- ""
for (index in 1:length(AphiaIDToMatch)) {
  url <- rbind(url, sprintf("https://www.marinespecies.org/rest/AphiaClassificationByAphiaID/%d", AphiaIDToMatch[index]))
}
url <- url[-1]

## Get the classification tree from the URL of each AphiaID
classificationTree <- lapply(url, fromJSON)

## Handle the data (each requested name has an list of results)
results_taxonomy <- matrix(data = NA, ncol = 5)
for (matchesindex in 1:length(AphiaIDToMatch)) {
  # Get the classification tree for the current index
  currentResultList = classificationTree[[matchesindex]]

  # Extract the necessary information
  results_taxonomy <- rbind(
    results_taxonomy,
    c(
      "AphiaID" = AphiaIDToMatch[[matchesindex]],
      "phylum" = currentResultList[["child"]][["child"]]$scientificname,
      "class" = ifelse(is.null(currentResultList[["child"]][["child"]][["child"]]$scientificname),
                       yes = NA, no = currentResultList[["child"]][["child"]][["child"]]$scientificname),
      "order" = ifelse(is.null(currentResultList[["child"]][["child"]][["child"]][["child"]]$scientificname),
                       yes = NA, no = currentResultList[["child"]][["child"]][["child"]][["child"]]$scientificname),
      "species" = species[[matchesindex]]
    )
  )
}
results_taxonomy <- results_taxonomy |> as_tibble() |> drop_na()

## Save result taxonomy
usethis::use_data(results_taxonomy, overwrite = TRUE)

## Join taxonomy table with data
data_full_taxonomy_done <- left_join(data_full_taxonomy, results_taxonomy, by = c("AphiaID", "species"))


# ---------------------------------------------------------------------------
# Export cleaned dataset for benthos ----

data_benthos_REBENT_cleaned <- data_full_taxonomy_done |>
  select(-c(ZONE_MARINE_QUADRIGE, SOUS_REGION_MARINE_DCSMM, MASSE_EAU_DCE, LIEU_IDENTIFIANT),
         -c(SUPPORT_NIVEAU_PRELEVEMENT, PARAMETRE_LIBELLE_COMPLET, PARAMETRE_CODE),
         -c(GROUPE_TAXON_LIBELLE, NIVEAU_QUALITE, QUALITE_DESCRIPTION, NUMERO_INDIVIDU_OBSERVATION))

usethis::use_data(data_benthos_REBENT_cleaned, overwrite = TRUE)
write_xlsx(data_benthos_REBENT_cleaned, "inst/results/data_benthos/data_benthos_REBENT_cleaned.xlsx")

map_tidal <- data_benthos_REBENT_cleaned |>
  distinct(LIEU_MNEMONIQUE, tidal)

# ---------------------------------------------------------------------------
# Sediment
data_sediment <- data_REBENT |>
    filter(PARAMETRE_GROUPE != "Biologie") |>
  left_join(map_tidal)

usethis::use_data(data_sediment, overwrite = TRUE)


# ---------------------------------------------------------------------------
# Estuaries maps - Haline zones from REBENT ----


# ---------------------------------------------------------------------------
# Gironde ----

plot_REBENT_map_gironde <- plot_estuary_map(
  data = data_REBENT |> dplyr::filter(estuary == "Gironde"),
  estuary_name = "Gironde",
  colour_var = haline_zone,
  shape_var = tidal
)

# save the image
ggsave(filename = "inst/results/data_maps/REBENT/plot_REBENT_map_gironde.jpg",
       plot = plot_REBENT_map_gironde)

# crop the image
img_gironde <- image_read("inst/results/data_maps/REBENT/plot_REBENT_map_gironde.jpg")
img_trim_gironde <- image_trim(img_gironde)
image_write(img_trim_gironde, "inst/results/data_maps/REBENT/plot_REBENT_map_gironde.jpg")


# ---------------------------------------------------------------------------
# Loire ----

plot_REBENT_map_loire <- plot_estuary_map(
  data = data_REBENT |> dplyr::filter(estuary == "Loire"),
  estuary_name = "Loire",
  colour_var = haline_zone,
  shape_var = tidal
)

# save the image
ggsave(filename = "inst/results/data_maps/REBENT/plot_REBENT_map_loire.jpg",
       plot = plot_REBENT_map_loire)

# crop the image
img_loire <- image_read("inst/results/data_maps/REBENT/plot_REBENT_map_loire.jpg")
img_trim_loire <- image_trim(img_loire)
image_write(img_trim_loire, "inst/results/data_maps/REBENT/plot_REBENT_map_loire.jpg")


# ---------------------------------------------------------------------------
# Seine ----

plot_REBENT_map_seine <- plot_estuary_map(
  data = data_REBENT |> dplyr::filter(estuary == "Seine"),
  estuary_name = "Seine",
  colour_var = haline_zone,
  shape_var = tidal
)

# save the image
ggsave(filename = "inst/results/data_maps/REBENT/plot_REBENT_map_seine.jpg",
       plot = plot_REBENT_map_seine)

# crop the image
img_seine <- image_read("inst/results/data_maps/REBENT/plot_REBENT_map_seine.jpg")
img_trim_seine <- image_trim(img_seine)
image_write(img_trim_seine, "inst/results/data_maps/REBENT/plot_REBENT_map_seine.jpg")

