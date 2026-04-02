# =====================================================
# Preparation script from external `sextant_outputs`
# Datasets:
#  - raw_data_benthos.rda
#  - raw_data_contamination.rda
#  - raw_data_physico_chem.rda
# Author: FM
# Date: 2026-04-02
# =====================================================


library(tidyverse, quietly = TRUE)


#------------------------------------------------------------------------------
# Read raw datasets and combine them

# Import raw sextant output files
sextant_output_Gironde <- read_csv("../SEXTANT/sextant-output-Gironde.csv")
sextant_output_Loire <- read_csv("../SEXTANT/sextant-output-Loire.csv")
sextant_output_Seine <- read_csv("../SEXTANT/sextant-output-Seine.csv")

# Join datasets
sextant_outputs <- sextant_output_Gironde |>
  full_join(sextant_output_Loire) |>
  full_join(sextant_output_Seine)

# Easily opening files in Excel program
write_csv2(sextant_outputs, "../SEXTANT/sextant-output-readr-csv2.csv")


#------------------------------------------------------------------------------
# Data cleaning identified thanks to the exploration with the app

sextant_outputs <- sextant_outputs |>

  # Identify estuaries and delete redundant variables of identification
  dplyr::mutate(estuary = case_when(
    str_starts(string = LIEU_MNEMONIQUE, pattern = "085") ~ "Gironde",
    str_starts(string = LIEU_MNEMONIQUE, pattern = "070") ~ "Loire",
    str_starts(string = LIEU_MNEMONIQUE, pattern = "011") ~ "Seine",
    TRUE ~ NA_character_
  )) |>
  dplyr::select(-c(ZONE_MARINE_QUADRIGE, SOUS_REGION_MARINE_DCSMM, MASSE_EAU_DCE, LIEU_IDENTIFIANT,
            PARAMETRE_GROUPE, PARAMETRE_LIBELLE_COMPLET, PARAMETRE_CODE)) |>

  # Save RESULTAT results as a character value
  dplyr::mutate(RESULTAT_chr = RESULTAT) |>

  # Create a numerical variable for RESULTAT
  dplyr::mutate(RESULTAT = as.numeric(RESULTAT)) |>
  # Replace missing values by 0 for numerical RESULTAT
  dplyr::mutate(RESULTAT = replace(RESULTAT, is.na(RESULTAT), 0)) |>

  # Extract geographic position
  dplyr::mutate(
    longitude = PRELEVEMENT_COORDONNEES |> stringr::str_extract("longitude\\s\\-*[0-9]+\\.[0-9]+")  |> str_remove("longitude ") |>  as.numeric(),
    latitude = PRELEVEMENT_COORDONNEES |> stringr::str_extract("latitude(\\s)[0-9]+\\.[0-9]+") |> str_remove("latitude ") |>  as.numeric()
  ) |>

  # Transform variables as factors
  dplyr::mutate(TAXON_LIBELLE = as.factor(TAXON_LIBELLE)) |>
  dplyr::mutate(estuary = as.factor(estuary)) |>

  # Extract year and month
  dplyr::mutate(year = lubridate::year(DATE)) |>

  # Normalization of contaminant concentration units
  dplyr::mutate(
    RESULTAT = dplyr::case_when(UNITE == "pg.g-1, p.h." ~ RESULTAT / 1000, TRUE ~ RESULTAT),
    UNITE = dplyr::case_when(UNITE == "pg.g-1, p.h." ~ "ng.g-1, p.h.", TRUE ~ UNITE)
  ) |>

  # Simplify complex names of chemicals
  dplyr::mutate(
    PARAMETRE_LIBELLE = dplyr::case_when(
      ## Organochlorinated
      PARAMETRE_LIBELLE == "Alpha-HCH (Hexachlorocyclohexane)" ~ "Alpha-HCH",
      PARAMETRE_LIBELLE == "Lindane ou gamma-HCH (Hexachlorocyclohexane)" ~ "Gamma-HCH",
      PARAMETRE_LIBELLE == "Dichlorodiphényl trichloréthane pp'" ~ "p,p'-DDT",
      PARAMETRE_LIBELLE == "Dichlorodiphényl trichloréthane op'" ~ "o,p'-DDT",
      PARAMETRE_LIBELLE == "Dichlorodiphényl dichloroéthylène pp'" ~ "p,p'-DDE",
      PARAMETRE_LIBELLE == "Dichlorodiphényl dichloréthane pp'" ~ "p,p'-DDD",
      ## Perfluorinated
      PARAMETRE_LIBELLE == "Perfluorodecanoate" ~ "PFDA",
      PARAMETRE_LIBELLE == "Perfluorododecanoate" ~ "PFDoA",
      PARAMETRE_LIBELLE == "Perfluorooctane sulfonate" ~ "PFOS",
      PARAMETRE_LIBELLE == "Perfluorooctanoate" ~ "PFOA",
      PARAMETRE_LIBELLE == "Perfluoroundecanoate" ~ "PFUnA",
      TRUE ~ PARAMETRE_LIBELLE
    )) |>
  ## HBCDD
  dplyr::mutate(PARAMETRE_LIBELLE = PARAMETRE_LIBELLE |> str_replace("Alpha-Hexabromocyclododecane", "Alpha-HBCDD")) |>
  dplyr::mutate(PARAMETRE_LIBELLE = PARAMETRE_LIBELLE |> str_replace("Beta-Hexabromocyclododecane", "Beta-HBCDD")) |>
  dplyr::mutate(PARAMETRE_LIBELLE = PARAMETRE_LIBELLE |> str_replace("Gamma-Hexabromocyclododecane", "Gamma-HBCDD")) |>
  ## PCB
  dplyr::mutate(PARAMETRE_LIBELLE = stringr::str_remove_all(string = PARAMETRE_LIBELLE, pattern = "Congénère de P")) |>
  # PBDE
  dplyr::mutate(PARAMETRE_LIBELLE = PARAMETRE_LIBELLE  |>  str_replace("Polybromodiphényléther congénère", "PBDE")) |>
  dplyr::mutate(PARAMETRE_LIBELLE = PARAMETRE_LIBELLE |> str_replace("^Tétrabromodiphényl\\s+éther\\W+congénère\\W+[:alnum:]+\\W+", "PBDE 66")) |>
  dplyr::mutate(PARAMETRE_LIBELLE = PARAMETRE_LIBELLE |> str_replace("^pentabromodiphényl\\s+éther\\W+congénère\\W+[:alnum:]+\\W+", "PBDE 85"))


#------------------------------------------------------------------------------
# Filter & Save data_benthos.rda

raw_data_benthos <- sextant_outputs |>
  filter(THEME == "Benthos dont récif corallien",
         PROGRAMME == "REBENT_FAU",
         PARAMETRE_LIBELLE == "Nombre d'individus d'état non précisé" |
           PARAMETRE_LIBELLE |> str_starts("Fraction") |
           PARAMETRE_LIBELLE == "Matière organique") |>
  select(- c(THEME, PROGRAMME))

usethis::use_data(raw_data_benthos, overwrite = TRUE)


#------------------------------------------------------------------------------
# Filter & Save "data_contamination.rda"

raw_data_contamination <- sextant_outputs |>
  filter(
    THEME == "Contaminants chimiques et écotoxicologie",
    PROGRAMME == "ROCCHMV",
    PARAMETRE_LIBELLE %in% c(

      # PARAMETRE_GROUPE: Mesures physiques
      "Matière sèche",
      "Taille de l'individu",

      # PARAMETRE_GROUPE: Toxico et Ecotoxicologie/Chimie-biochimie
      "Lipides totaux",

      # PARAMETRE_GROUPE: Contaminants/Hydrocarbures PAH
      "Anthracène", "Benzo(a)anthracène", "Benzo(a)pyrène", "Benzo(g,h,i)pérylène",
      "Fluoranthène", "Naphtalène", "Phénanthrène", "Pyrène",

      # PARAMETRE_GROUPE: Contaminants/Métaux et métalloïdes
      "Mercure", "Cadmium", "Plomb", "Zinc", "Cuivre",
      "Nickel", "Vanadium", "Argent", "Chrome total",

      # PARAMETRE_GROUPE: Contaminants/Organiques autres
      "4-nonylphenols ramifiés", "Diéthylhexylphtalate (DEHP ou DOP)",

      # PARAMETRE_GROUPE: Organohalogénés
      # PCBs
      "CB 28", "CB 52", "CB 101", "CB 118", "CB 138", "CB 153", "CB 180",
      # PBDE
      "PBDE 28", "PBDE 47", "PBDE 99", "PBDE 100", "PBDE 153", "PBDE 154",
      # DL-compounds: 12 DL-PCB
      "CB 77", "CB 81", "CB 105", "CB 114", "CB 118", "CB 123",
      "CB 126", "CB 156", "CB 157", "CB 167", "CB 169", "CB 189",
      # DL-compounds: 7 polychlorinated dibenzo-p-dioxins (PCDDs)
      "2,3,7,8-tetrachlorodibenzo-p-dioxine",
      "1,2,3,7,8-pentachlorodibenzo-p-dioxine",
      "1,2,3,4,7,8-hexachlorodibenzo-p-dioxine",
      "1,2,3,6,7,8-hexachlorodibenzo-p-dioxine",
      "1,2,3,7,8,9-hexachlorodibenzo-p-dioxine",
      "1,2,3,4,6,7,8- heptachlorodibenzo-p-dioxine",
      "octachlorodibenzo-p-dioxine", # OCDD 1,2,3,4,6,7,8,9-O8CDD
      # DL-compounds: 10 polychlorinated dibenzofurans (PCDFs)
      "2,3,7,8-tetrachlorodibenzofuran",
      "1,2,3,7,8-pentachlorodibenzofuran",
      "2,3,4,7,8-pentachlorodibenzofuran",
      "1,2,3,4,7,8-hexachlorodibenzofuran",
      "1,2,3,6,7,8-hexachlorodibenzofuran",
      "1,2,3,7,8,9-hexachlorodibenzofuran",
      "2,3,4,6,7,8-hexachlorodibenzofuran",
      "1,2,3,4,6,7,8-heptachlorodibenzofuran",
      "1,2,3,4,7,8,9-heptachlorodibenzofuran",
      "octachlorodibenzofuranne", # OCDF 1,2,3,4,6,7,8,9-O8CDF
      # HBCDD: 3 isomers
      "Alpha-HBCDD", "Beta-HBCDD", "Gamma-HBCDD",
      # Organochlorine pesticides
      "Gamma-HCH", "p,p'-DDT", "o,p'-DDT", "p,p'-DDE", "p,p'-DDD",

      # PARAMETRE_GROUPE: Contaminants/Organométaux
      "Tributylétain cation",

      # PARAMETRE_GROUPE: Contaminants/Perfluorés (PFC)
      "PFOS"
    )
  ) |>
  select(-c(THEME, PROGRAMME))

usethis::use_data(raw_data_contamination, overwrite = TRUE)


#------------------------------------------------------------------------------
# Filter & Save "data_phyto.rda"

raw_data_physico_chem <- sextant_outputs |>
  filter(
    THEME == "Phytoplancton, hydrologie et phycotoxines",
    PARAMETRE_LIBELLE %in% c(

      # PARAMETRE_GROUPE: Biologie/Phytoplancton
      "Chlorophylle a", "Phéopigments", # > production primaire

      # PARAMETRE_GROUPE: Mesures physiques
      "Température de l'eau",

      # PARAMETRE_GROUPE: Mesures physiques/Matériel particulaire
      "Turbidité",
      "Turbidité FNU",
      "Matière en suspension",

      # PARAMETRE_GROUPE: Nutriments/Nutriments Inorganiques
      "Silicate", # RNOHYD (1975-2016) + REPHY (2007-2024)
      "Ammonium", "Azote nitreux (nitrite)", "Azote nitrique (nitrate)",
      "Nitrate + nitrite", "Phosphate",

      # PARAMETRE_GROUPE: Physicochimie
      "Oxygène dissous", "pH", "Salinité"
    )
  ) |>
  select(-THEME)

usethis::use_data(raw_data_physico_chem, overwrite = TRUE)
