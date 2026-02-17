# =====================================================
# Datasets:
#  - data_REBENT_biota.rda
#  - data_REBENT_sediment.rda
#  - results_taxonomy.rda
# Preparation script
# Author: FM
# Date: 2026-02-17
# =====================================================

# =====================================================
# 00. Packages
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(writexl)
library(magick) # crop images
# Get taxonomy api
library(jsonlite) # install.packages("jsonlite", repos="http://cran.r-project.org")
library(httr) # install.packages("httr")

`%!in%` = Negate(`%in%`)

# =====================================================
# 01. Create data_REBENT from raw data
# =====================================================

data_REBENT <- esteem.overview::raw_data_benthos |>
  # properly read dates
  mutate(YEAR = year(DATE)) |>
  mutate(year_month = paste0(year(DATE), "-", month(DATE))) |>
  # creat estuary variable
  mutate(estuary = case_when(
    ZONE_MARINE_QUADRIGE == "085 - Estuaire de la Gironde" ~ "Gironde",
    ZONE_MARINE_QUADRIGE == "070 - Estuaire de la Loire" ~ "Loire",
    ZONE_MARINE_QUADRIGE == "011 - Estuaire de la Seine" ~ "Seine"
  )) |>
  # clean unnecessary variables
  select(-c(ZONE_MARINE_QUADRIGE, SOUS_REGION_MARINE_DCSMM, MASSE_EAU_DCE, LIEU_IDENTIFIANT),
         -c(SUPPORT_NIVEAU_PRELEVEMENT, PARAMETRE_LIBELLE_COMPLET, PARAMETRE_CODE),
         -c(GROUPE_TAXON_LIBELLE, NIVEAU_QUALITE, QUALITE_DESCRIPTION, NUMERO_INDIVIDU_OBSERVATION))

# =====================================================
# 02. Select autumn campaigns
# =====================================================

# Campains are organized each 3 years, once in autumn, except for 2007:
# april in Seine only, april&may in Gironde as well as october the same year.
# data_REBENT_biota |>
#   distinct(estuary, year_month) |>
#   arrange(estuary, year_month)

data_REBENT <- data_REBENT |>
  filter(year_month %!in% c("2007-4", "2007-5"))

# =====================================================
# 03. Correct points with wrong coordinates
# =====================================================

# data_REBENT |>
#   select(estuary, LIEU_MNEMONIQUE, latitude, longitude) |>
#   distinct() |>
#   group_by(estuary) |>
#   summarise(lat_stat = paste0(min(latitude), " - ", max(latitude)),
#             lon_stat = paste0(min(longitude), " - ", max(longitude)))


## => Wrong coordinates for Gironde estuary: latitude = 42.26208 => 45.26208
data_REBENT <- data_REBENT |>
  mutate(latitude = case_when(
    latitude == 42.26208 ~ 45.26208,
    TRUE ~ latitude
  ))

# =====================================================
# 04. Create haline zones variable
# =====================================================

data_REBENT <- data_REBENT |>
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


# =====================================================
# 05. String manipulation to extract variables
# =====================================================

# ---- Types of benthic sampler and their surface ----

# Two types of benthic sampler: intertidal corers & subtidal grab
# - "Benne Smith Mc Intyre": Smith Mc Intyre grab 0.1m²
# - "Benne Van Veen": Van Veen grab 0.1m²
# - "Carottier PVC SIM DCE (0,029 m²)"

# 354 types of PRELEVEMENT_DESCRIPTION
# data_REBENT_biota |> distinct(PRELEVEMENT_DESCRIPTION)


# ---- Extract "Mnémonique", "Engin, "Taille du prélèvement", "Commentaires" ----

data_REBENT <- data_REBENT |>
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


# ---- Cleaning of "Engin" and corresponding "Taille du prélèvement" ----

# Check combination sampler_type & sampler surface
data_REBENT |>
    count(sampler_type, sampler_surface)
  # => incoherence between sampler type and sampler surface for "011-P-056" in Seine estuary
  # => absence of sampler surface for "Carottier PVC diam. 19 cm (0,028 m²)"

# For which "LIEU_MNEMONIQUE" the sampler type and surface are incoherent?
data_REBENT |>
    filter(sampler_type == "Carottier PVC diam. 19 cm (0,028 m²)",
           sampler_surface == 0.1) |>
    distinct(estuary, LIEU_MNEMONIQUE, DATE)
  # => for "011-P-056" in Seine estuary

# Which distinct sampler_type exist for "011-P-056" in Seine estuary
data_REBENT |>
    filter(LIEU_MNEMONIQUE == "011-P-056") |>
    distinct(sampler_type)
  # => only "Carottier PVC diam. 19 cm (0,028 m²)" => it is sampler_surface that is wrong

# Correction of sampler surface: "Carottier PVC diam. 19 cm (0,028 m²)" is always "0.028" m²
data_REBENT <- data_REBENT |>
  mutate(sampler_surface = case_when(
    sampler_type == "Carottier PVC diam. 19 cm (0,028 m²)" ~ 0.028,
    TRUE ~ sampler_surface
  )
)


# =====================================================
# 06. Create tidal type variable
# =====================================================

data_REBENT <- data_REBENT |>
  mutate(tidal = case_when(
    str_detect(sampler_type, pattern = "Carottier") ~ "intertidal",
    str_detect(sampler_type, pattern = "Benne") ~ "subtidal",
    TRUE ~ NA
  ))

# =====================================================
# 07. Get species taxonomy
# =====================================================

## Total: 201 species
# data_REBENT_biota |>  distinct(TAXON_LIBELLE) |> dim()

data_REBENT_biota_taxonomy <- data_REBENT  |>
  filter(PARAMETRE_GROUPE == "Biologie") |>
  separate(TAXON_LIBELLE, c("species", "AphiaID"), sep = " \\(") |>
  mutate(AphiaID = AphiaID |> str_remove("AphiaID : "),
         AphiaID = AphiaID |> str_remove("\\)")) |>
  mutate(AphiaID = case_when(
    species == "Nemertea sp2" ~ "152391",
    TRUE ~ AphiaID
  ))

## The vector of AphiaID we wan't to match
AphiaIDToMatch <- data_REBENT_biota_taxonomy |> distinct(AphiaID) |>  pull(AphiaID) |>  as.numeric()
species <- data_REBENT_biota_taxonomy |> distinct(species) |>  pull(species)

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
data_REBENT_biota <- left_join(data_REBENT_biota_taxonomy, results_taxonomy, by = c("AphiaID", "species"))

usethis::use_data(data_REBENT_biota, overwrite = TRUE)
write_xlsx(data_REBENT_biota, "inst/results/data_benthos/data_REBENT_biota.xlsx")


# =====================================================
# 9. Data sediment where biota was sampled
# =====================================================

map_tidal <- data_REBENT_biota |>
  distinct(LIEU_MNEMONIQUE, tidal)

data_REBENT_sediment <- data_REBENT |>
  filter(PARAMETRE_GROUPE != "Biologie") |>
  left_join(map_tidal)

usethis::use_data(data_REBENT_sediment, overwrite = TRUE)
