---------------------------------------------------------------------------
# Libraries ----

library(tidyverse, quietly = TRUE)
# Maps
library(sf)

`%!in%` = Negate(`%in%`)

# ---------------------------------------------------------------------------
# Read raw data from sextan ----

data_emergentsea <- read_csv("~/ESTEEM/Phase 1/DATABASES/data-raw-quadrige-emergentsea.csv")

# ---------------------------------------------------------------------------
# Cleaning variables

data_emergentsea <- data_emergentsea |>
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
  # Compute YEAR variable
  dplyr::mutate(YEAR = year(DATE)) |>
  # Create ESTUARY variable
  dplyr::mutate(ESTUARY = case_when(
    ZONE_MARINE_QUADRIGE == "084 - Aval et large de la Gironde" ~ "Gironde",
    ZONE_MARINE_QUADRIGE == "070 - Estuaire de la Loire" ~ "Loire",
    ZONE_MARINE_QUADRIGE == "011 - Estuaire de la Seine" ~ "Seine")) |>
  # Transform variables as factors
  dplyr::mutate(ESTUARY = as.factor(ESTUARY)) |>
  # Change complex names
  dplyr::mutate(
    PARAMETRE_LIBELLE = case_when(
      PARAMETRE_LIBELLE == "N,N-Dimethyl-N'-p-tolylsulphamide" ~ "DMST",
      PARAMETRE_LIBELLE == "Atrazine Déséthyl" ~ "DEA",
      PARAMETRE_LIBELLE == "Aminométhylphosphonique acide" ~ "AMPA",
      PARAMETRE_LIBELLE == "Atrazine Deisopropyl" ~ "DIA",
      PARAMETRE_LIBELLE == "Tralopyril : Biocide antifouling potentiel, biocide antisalissure" ~ "Tralopyril",
      PARAMETRE_LIBELLE == "4,5-dichloro-2-octyl-1,2-thiazol-3(2H)-one" ~ "DCOIT",
      PARAMETRE_LIBELLE == "Médétomidine : composé sédatif présentant des propriétés analgésiques et myorelaxantes vétérinaire" ~ "Médétomidine",
      PARAMETRE_LIBELLE == "Thirame (TMTD)" ~ "Thirame",
      PARAMETRE_LIBELLE == "Dichloroaniline-3,4" ~ "3,4-DCA",
      PARAMETRE_LIBELLE == "1-(3,4-dichlorophenyl)-3-methyl uree (DCPMU)" ~ "DCPMU",
      PARAMETRE_LIBELLE == "3,4-dichlorophenyl uree (DCPU)" ~ "DCPU",
      TRUE ~ PARAMETRE_LIBELLE
    )
  ) |>
  # Delete unused variables
  dplyr::select(-c(THEME, ZONE_MARINE_QUADRIGE, SOUS_REGION_MARINE_DCSMM, MASSE_EAU_DCE,
                   PROGRAMME, CAMPAGNE, GROUPE_TAXON_LIBELLE, TAXON_LIBELLE,
                   LIEU_IDENTIFIANT, LIEU_LIBELLE,
                   PARAMETRE_GROUPE, PARAMETRE_LIBELLE_COMPLET, PARAMETRE_CODE,
                   NUMERO_INDIVIDU_OBSERVATION, RESULTAT_COMMENTAIRE,
                   ECHANTILLON_DESCRIPTION, CITATION,
                   RESULTAT_DESCRIPTION, PRELEVEMENT_DESCRIPTION,
                   SUPPORT_NIVEAU_PRELEVEMENT,
                   PASSAGE_DESCRIPTION, PASSAGE_COORDONNEES, PRELEVEMENT_COORDONNEES,
                   NIVEAU_QUALITE, QUALITE_DESCRIPTION))


# ---------------------------------------------------------------------------
# Compounds selection and classification by family

## Measured in POCIS

POCIS_pharmaceuticals <- c("Carbamazépine", "Oxazepam", "Paracétamol",
                   "Sulfamethoxazole", "Diclofénac",
                   "Acide fenofibrique", "Ibuprofène")

POCIS_fungicides <- c("Métalaxyl", "Tébuconazole", "Propiconazole", "Diméthomorphe", "DMST",
                "Carbendazime", "Azoxystrobine", "Spiroxamine", "Epoxiconazole", "Boscalid")

POCIS_insecticides <- c("Imidaclopride", "Fipronil", "Thiamethoxam", "Diazinon", "Fipronil sulfone")

POCIS_herbicides <- c(
  "2-hydroxy atrazine", "Atrazine", "Métolachlore", "Metolachlore ESA", "Metolachlore OXA",
  "Bentazone", "Chlortoluron", "Métazachlore", "Simazine-hydroxy",
  "Glyphosate", "Diuron", "Propyzamide", "DEA", "Terbuthylazin",
  "2,4-D", "Isoproturon", "Amétryne", "Simazine", "Terbuthylazine désethyl",
  "Acetochlore ESA", "AMPA", "Terbutryne", "DIA",
  "Hexazinone", "Prosulfuron", "Prométryne", "Propazine", "Alachlore"
)

POCIS_antifouling <- c("Irgarol")

POCIS_compound_table <- tibble(
  family = c(
    rep("pharmaceuticals", length(POCIS_pharmaceuticals)),
    rep("fungicides", length(POCIS_fungicides)),
    rep("insecticides", length(POCIS_insecticides)),
    rep("herbicides", length(POCIS_herbicides)),
    rep("antifouling", length(POCIS_antifouling))
  ),
  compound = c(
    POCIS_pharmaceuticals,
    POCIS_fungicides,
    POCIS_insecticides,
    POCIS_herbicides,
    POCIS_antifouling
  ),
  matrix = "POCIS"
)


## Measured in bivalves

bivalve_antifouling <- c("Tolyfluanide", "Cuivre pyrithione", "Dichlofluanide",
                         "Tralopyril", "DCOIT", "Médétomidine", "Zinc pyrithione",
                         "Irgarol", "Zinèbe")

bivalve_fungicides <- c("Azoxystrobine", "Tébuconazole", "Chlorothalonil",
                        "Thirame", "Propiconazole", "Boscalid", "Epoxiconazole")

bivalve_insecticides <- c("Fipronil")

bivalve_herbicides <- c("Diflufenicanil", "Linuron", "Métolachlore", "3,4-DCA",
                        "DCPMU", "Terbuthylazine désethyl", "DCPU",
                        "Chlortoluron", "Propazine", "DEA", "Diuron",
                        "Metoxuron", "Dimethenamide", "Chlorprophame")

bivalve_compound_table <- tibble(
  family = c(
    rep("fungicides", length(bivalve_fungicides)),
    rep("insecticides", length(bivalve_insecticides)),
    rep("herbicides", length(bivalve_herbicides)),
    rep("antifouling", length(bivalve_antifouling))
  ),
  compound = c(
    bivalve_fungicides,
    bivalve_insecticides,
    bivalve_herbicides,
    bivalve_antifouling
  ),
  matrix = "bivalve"
)

compound_table <- full_join(POCIS_compound_table, bivalve_compound_table)

data_emergentsea <- data_emergentsea |>
  filter(PARAMETRE_LIBELLE %in% compound_table$compound)

data_emergentsea <- left_join(data_emergentsea, compound_table,
                              by = c("PARAMETRE_LIBELLE" = "compound"))
#------------------------------------------------------------------------------
# Save data data_emergentsea.rda

usethis::use_data(data_emergentsea, overwrite = TRUE)
