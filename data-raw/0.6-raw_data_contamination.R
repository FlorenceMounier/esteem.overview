# =====================================================
# Prepare contamination data from ROCCHMV program
# Datasets:
#  - data_biometrics.rda
#  - data_ROCCHMV_contamination.rda
# Graphs in /inst/mat_meth/contamination
#  - ggplot_dry_content.jpg
#  - ggplot_lipid_content.jpg
#  - ggmap_ROCCHMV.jpg
# Preparation script
# Author: FM
# Date: 2026-06-30
# =====================================================

# =====================================================
# 00. Packages, functions and raw data from {quadrige.explorer}
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(cowplot)

`%!in%` = Negate(`%in%`)

data(raw_data_contamination)


# =====================================================
# 01. Cleaning raw data
# =====================================================

data_ROCCHMV_clean <- raw_data_contamination |>

  # ---- Get studied species ----
mutate(
  support = str_extract(SUPPORT_NIVEAU_PRELEVEMENT, "Support\\s*:\\s*([^-\n]+)") |>
    str_remove("Support\\s*:\\s*") |>
    str_trim(),
  species = str_extract(SUPPORT_NIVEAU_PRELEVEMENT, "-\\s*([^-\n]+)\\s*-")  |>
    str_remove_all("-") |>
    str_trim(),
  niveau = str_extract(SUPPORT_NIVEAU_PRELEVEMENT, "Niveau\\s*:\\s*(.*)")  |>
    str_remove("Niveau\\s*:\\s*") |>
    str_trim()
) |>

  # ---- Delete fish and clam measurments ----
filter(support != "Poisson", species != "Ruditapes philippinarum (palourde japonaise)") |>

  # ---- Abreviate species names ----

mutate(species = case_when(
  species == "Crassostrea gigas (huître creuse)" ~ "C. gigas",
  species == "Mytilus edulis (moule commune)" ~ "M. edulis",
  species == "Mytilus edulis + galloprovincialis (moule)" ~ "M. edulis & galloprovincialis",
  TRUE ~ species
)) |>

  # ---- Get measurement method description ----

mutate(
  method = str_extract(RESULTAT_DESCRIPTION, "Méthode\\s*:\\s*(.*)\\s*-")  |>
    str_remove("Méthode\\s*:\\s*") |>
    str_remove_all("-(.*)") |>
    str_trim()
) |>

  # ---- Get estuary name from GPS positions ----
get_estuary_from_gps_position(latitude = latitude, longitude = longitude) |>

  # ----- Define information on year, month, season from date -----
get_info_from_dates(date_variable = DATE) |>

  # ---- Clean variables ----
select(
  -c(
    CAMPAGNE,
    GROUPE_TAXON_LIBELLE,
    NUMERO_INDIVIDU_OBSERVATION,
    RESULTAT_COMMENTAIRE,
    TAXON_LIBELLE,
    PASSAGE_DESCRIPTION,
    PASSAGE_COORDONNEES,
    PRELEVEMENT_COORDONNEES,
    PRELEVEMENT_DESCRIPTION,
    NIVEAU_QUALITE,
    QUALITE_DESCRIPTION,
    CITATION,
    SUPPORT_NIVEAU_PRELEVEMENT,
    RESULTAT_DESCRIPTION,
    support
  )
)


# =====================================================
# 02. Biometrics
# =====================================================

# ---- Filter biometrics data ----
data_ROCCHMV_bio <- data_ROCCHMV_clean |>
  filter(PARAMETRE_LIBELLE %in% c("Matière sèche", "Lipides totaux"))


# ---- Dry content ----
data_ROCCHMV_bio_dry_content <- data_ROCCHMV_bio |>
  filter(PARAMETRE_LIBELLE == "Matière sèche")

data_ROCCHMV_bio_dry_content_summarised <- data_ROCCHMV_bio_dry_content |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  group_by(estuary, year, species) |>
  summarise(percent_dw = mean(`Matière sèche`, na.rm = TRUE),
            n = n(),
            .groups = "drop")

ggplot_dry_content <- ggplot(data_ROCCHMV_bio_dry_content_summarised) +
  aes(x = year, y = percent_dw, fill = estuary, colour = estuary) +
  geom_line() +
  facet_grid(vars(species)) +
  labs(y = "Dry content (%)")

ggsave(ggplot_dry_content, filename = "inst/mat_meth/contamination/ggplot_dry_content.jpg",
       height = 15, width = 10, units = "cm")

dry_contents <- data_ROCCHMV_bio_dry_content |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  group_by(estuary, species) |>
  summarise(percent_dw = mean(`Matière sèche`, na.rm = TRUE), .groups = "drop")

# Gironde C. gigas: 15.3 %
# Loire M.edulis: 18.4 %
# Loire M. edulis & galloprovincialis: 18.4 %
# Seine M.edulis: 23.3 %


# ---- Lipid content ----

data_ROCCHMV_bio_lipid <- data_ROCCHMV_bio  |>
  filter(PARAMETRE_LIBELLE == "Lipides totaux") |>
  mutate(percent_ldw = case_when(
    UNITE == "%" ~ RESULTAT,
    UNITE == "%, p.h." & species == "C. gigas" ~ RESULTAT / 15.3 * 100,
    UNITE == "%, p.h." & species == "M.edulis" & estuary == "Loire" ~ RESULTAT / 18.4 * 100,
    UNITE == "%, p.h." & species == "M.edulis" & estuary == "Seine" ~ RESULTAT / 23.3 * 100,
    UNITE == "%, p.h." & species == "M. edulis & galloprovincialis" ~ RESULTAT / 18.4 * 100,
    TRUE ~ NA
  )) |>
  mutate(percent_lww = case_when(
    UNITE == "%, p.h." ~ RESULTAT,
    UNITE == "%" & species == "C. gigas" ~ RESULTAT * 15.3 / 100,
    UNITE == "%" & species == "M.edulis" & estuary == "Loire" ~ RESULTAT * 18.4 / 100,
    UNITE == "%" & species == "M.edulis" & estuary == "Seine" ~ RESULTAT * 23.3 / 100,
    UNITE == "%" & species == "M. edulis & galloprovincialis" ~ RESULTAT * 18.4 / 100,
    TRUE ~ NA
  ))

data_ROCCHMV_bio_lipid_content_summarised <- data_ROCCHMV_bio_lipid |>
  group_by(estuary, year, species) |>
  summarise(percent_ldw = mean(percent_ldw, na.rm = TRUE),
            percent_lww = mean(percent_lww, na.rm = TRUE),
            n = n(),
            .groups = "drop")

ggplot_lipid_content <- ggplot(data_ROCCHMV_bio_lipid_content_summarised) +
  aes(x = year, y = percent_ldw, fill = estuary, colour = estuary) +
  geom_line() +
  facet_grid(vars(species)) +
  labs(y = "Lipid content (%dw)")
ggsave(ggplot_lipid_content, filename = "inst/mat_meth/contamination/ggplot_lipid_content.jpg",
       height = 15, width = 10, units = "cm")

lipid_contents <- data_ROCCHMV_bio_lipid |>
  group_by(estuary, species) |>
  summarise(percent_ldw = mean(percent_ldw, na.rm = TRUE),
            percent_lww = mean(percent_lww, na.rm = TRUE),
             n = n(),
             .groups = "drop")

# Gironde C. gigas: ldw = 6.9 %, lww = 1.06 %
# Loire M.edulis: ldw = 7.72 %, lww = 2.32 %
# Loire M. edulis & galloprovincialis: ldw = 9.09 %, lww = 1.67 %
# Seine M.edulis: ldw = 8.00 %, lww = 2.13 %


# ---- Create data_biometrics ----
data_biometrics <- full_join(dry_contents, lipid_contents)

usethis::use_data(data_biometrics, overwrite = TRUE)

# =====================================================
# 03. Contamination data
# =====================================================

data_ROCCHMV_contamination <- data_ROCCHMV_clean |>

  # ---- Filter not biometrics ----
filter(PARAMETRE_LIBELLE %!in% c("Matière sèche", "Lipides totaux")) |>

  # ---- Add lipid contents %ldw & %lww ----
left_join(data_biometrics) |>
  select(-n) |>

  # ---- Units conversions ----

mutate(RESULTAT = case_when(
  UNITE == "ng.g-1, p.h." ~ RESULTAT,
  UNITE == "µg.kg-1" ~ RESULTAT,
  UNITE == "mg.kg-1" ~ RESULTAT * 1000,
  UNITE == "mg/(kg MS)" ~ RESULTAT * 1000,
  UNITE == "ng.kg-1" ~ RESULTAT / 1000,
  UNITE == "µg.kg-1, p.h." ~ RESULTAT * 1000,
  UNITE == "µg/(kg MS)" ~ RESULTAT * 1000,
  UNITE == "ng.g-1" ~ RESULTAT,
  UNITE == "ng.kg-1, p.h." ~ RESULTAT / 1000,
  TRUE ~ RESULTAT
)) |>

  mutate(UNITE = case_when(
    UNITE == "ng.g-1, p.h." ~ "ng.gww-1",
    UNITE == "µg.kg-1" ~ "ng.gdw-1",
    UNITE == "mg.kg-1" ~ "ng.gdw-1",
    UNITE == "mg/(kg MS)" ~ "ng.gdw-1",
    UNITE == "ng.kg-1" ~ "ng.gdw-1",
    UNITE == "µg.kg-1, p.h." ~ "ng.gww-1",
    UNITE == "µg/(kg MS)" ~ "ng.gdw-1",
    UNITE == "ng.g-1" ~ "ng.gdw-1",
    UNITE == "ng.kg-1, p.h." ~ "ng.gww-1",
    TRUE ~ UNITE
  )) |>

  # Convert to all target units
  mutate(
    RESULTAT_ng_gdw = case_when(
      UNITE == "ng.gdw-1" ~ RESULTAT,
      UNITE == "ng.gww-1" ~ RESULTAT / (percent_dw / 100)
    ),
    RESULTAT_ng_gww = case_when(
      UNITE == "ng.gdw-1" ~ RESULTAT * (percent_dw / 100),
      UNITE == "ng.gww-1" ~ RESULTAT
    ),
    RESULTAT_ng_glw = case_when(
      UNITE == "ng.gdw-1" ~ RESULTAT / (percent_ldw / 100),
      UNITE == "ng.gww-1" ~ RESULTAT / (percent_lww / 100)
    )
  ) |>
  select(-c(RESULTAT, UNITE)) |>

  # ---- Translate compounds if necessary ----

mutate(PARAMETRE_LIBELLE = case_when(
  # -- Metals
  PARAMETRE_LIBELLE == "Mercure" ~ "Mercury",
  PARAMETRE_LIBELLE == "Cadmium" ~ "Cadmium",
  PARAMETRE_LIBELLE == "Plomb" ~ "Lead",
  PARAMETRE_LIBELLE == "Cuivre" ~ "Copper",
  # -- PAH
  PARAMETRE_LIBELLE == "Fluoranthène" ~ "Fluoranthene",
  PARAMETRE_LIBELLE == "Benzo(a)pyrène" ~ "Benzo-pyr.",
  PARAMETRE_LIBELLE == "Pyrène" ~ "Pyrene",
  PARAMETRE_LIBELLE == "Benzo(a)anthracène" ~ "Benzo-anthr.",
  PARAMETRE_LIBELLE == "Benzo(g,h,i)pérylène" ~ "Benzo-peryl.",
  PARAMETRE_LIBELLE == "Anthracène" ~ "Anthracene",
  PARAMETRE_LIBELLE == "Naphtalène" ~ "Naphtalene",
  PARAMETRE_LIBELLE == "Phénanthrène" ~ "Phenanthrene",
  ## PBDE congeners
  PARAMETRE_LIBELLE %in% c("PBDE 28", "PBDE 47", "PBDE 99", "PBDE 100", "PBDE 153", "PBDE 154") ~ PARAMETRE_LIBELLE,
  # -- Organostannic compounds - Tributyltin cation
  PARAMETRE_LIBELLE == "Tributylétain cation" ~ "Tributyltin cation",
  # Others + biometrics parameters
  TRUE ~ PARAMETRE_LIBELLE
)) |>

  # ---- Add sub_family and family variables ----

## Subfamilies
mutate(sub_family = case_when(

  # -- Metals
  PARAMETRE_LIBELLE %in% c("Mercury", "Cadmium", "Lead", "Copper") ~ "Metals",

  # -- Organochlorin pesticides
  ## DDT total
  PARAMETRE_LIBELLE %in% c("p,p'-DDE", "p,p'-DDD", "p,p'-DDT", "o,p'-DDT") ~ "DDT total",
  ## Lindane

  PARAMETRE_LIBELLE == "Gamma-HCH" ~ "Lindane",
  # -- PAH
  PARAMETRE_LIBELLE %in% c("Fluoranthene", "Benzo-pyr.", "Pyrene",
                           "Benzo-anthr.", "Benzo-peryl.",
                           "Anthracene", "Naphtalene", "Phenanthrene") ~ "PAH",

  # -- PCBi
  PARAMETRE_LIBELLE %in% c("CB 28", "CB 52", "CB 101", "CB 118", "CB 138", "CB153", "CB 180") ~ "PCBi",

  # -- Dioxin-Like Compounds (DLC)
  ## 7 polychlorinated dibenzo-p-dioxins (PCDDs)
  PARAMETRE_LIBELLE %in% c(
    "2,3,7,8-tetrachlorodibenzo-p-dioxine",
    "1,2,3,7,8-pentachlorodibenzo-p-dioxine",
    "1,2,3,4,7,8-hexachlorodibenzo-p-dioxine",
    "1,2,3,6,7,8-hexachlorodibenzo-p-dioxine",
    "1,2,3,7,8,9-hexachlorodibenzo-p-dioxine",
    "1,2,3,4,6,7,8- heptachlorodibenzo-p-dioxine",
    "octachlorodibenzo-p-dioxine") ~ "PCDDs",
  ## 10 polychlorinated dibenzofurans (PCDFs)
  PARAMETRE_LIBELLE %in% c(
    "2,3,7,8-tetrachlorodibenzofuran",
    "1,2,3,7,8-pentachlorodibenzofuran",
    "2,3,4,7,8-pentachlorodibenzofuran",
    "1,2,3,4,7,8-hexachlorodibenzofuran",
    "1,2,3,6,7,8-hexachlorodibenzofuran",
    "1,2,3,7,8,9-hexachlorodibenzofuran",
    "2,3,4,6,7,8-hexachlorodibenzofuran",
    "1,2,3,4,6,7,8-heptachlorodibenzofuran",
    "1,2,3,4,7,8,9-heptachlorodibenzofuran",
    "octachlorodibenzofuranne") ~ "PCDFs",
  ## 12 DL-PCB
  PARAMETRE_LIBELLE %in% c(
    "CB 77", "CB 81", "CB 105", "CB 114", "CB 118", "CB 123",
    "CB 126", "CB 156", "CB 157", "CB 167", "CB 169", "CB 189") ~ "DL-PCBs",

  # -- Brominated flame retardants (BFR)
  ## Hexabromocyclododecane isomers (HBCDD isomers)
  PARAMETRE_LIBELLE %in% c("Alpha-HBCDD", "Beta-HBCDD", "Gamma-HBCDD") ~ "HBCDDs",
  ## PBDE congeners
  PARAMETRE_LIBELLE %in% c("PBDE 28", "PBDE 47", "PBDE 99", "PBDE 100", "PBDE 153", "PBDE 154") ~ "PBDE",

  # -- PFAS - PFOS
  PARAMETRE_LIBELLE == "PFOS" ~ "PFAS",

  # -- Organostannic compounds - Tributyltin cation
  PARAMETRE_LIBELLE == "Tributylétain cation" ~ "Organostannic compounds"
)) |>

## Families
mutate(family = case_when(
  sub_family %in% c("HBCDD", "PBDE") ~ "Brominated",
  sub_family == "Metals" ~ "Metals",
  sub_family %in% c("DTT total", "Lindane") ~ "Organochlorin pesticides",
  sub_family ==  "PAH" ~ "PAH",
  sub_family ==  "PCBi" ~ "PCB",
  sub_family %in% c("PCDDs", "PCDFs", "PCB-DL", "DL-PCBs") ~ "DLC",
  sub_family %in% c("HBCDDs", "PBDEs") ~ "BFR",
  sub_family == "PFAS" ~ "Perfluorinated compounds",
  sub_family == "Tributylétain cation" ~ "Organostannic compounds",
  TRUE ~ "Biometrics"
))


# =====================================================
# 02. Case of CB118: PCBi & DL-PCB
# =====================================================

# Subdataset
data_ROCCHMV_CB118 <- data_ROCCHMV_contamination |>
  filter(PARAMETRE_LIBELLE == "CB 118") |>
  mutate(sub_family = "DL-PCBs") |>
  mutate(family = "DLC")

# Duplicate rows with DL-PCB informations
data_ROCCHMV_contamination <- rbind(data_ROCCHMV_contamination, data_ROCCHMV_CB118)

usethis::use_data(data_ROCCHMV_contamination, overwrite = TRUE)


# =====================================================
# 04. Sampling maps
# =====================================================

# ---- Gironde ----
ggmap_ROCCHMV_gironde <- plot_estuary_map(
  data = data_ROCCHMV |> filter(estuary == "Gironde"),
  estuary_name = "Gironde", colour_var = ggplot2_colors(3)[3],
  size_var = 3
) + theme_esteem() +
  theme(legend.position = "none", axis.title = ggplot2::element_blank())

# ---- Loire ----
ggmap_ROCCHMV_loire <- plot_estuary_map(
  data = data_ROCCHMV |> filter(estuary == "Loire"),
  estuary_name = "Loire", colour_var = ggplot2_colors(3)[2],
  size_var = 3
) + theme_esteem() +
  theme(legend.position = "none", axis.title = ggplot2::element_blank())


# ---- Seine ----
ggmap_ROCCHMV_seine <- plot_estuary_map(
  data = data_ROCCHMV |> filter(estuary == "Seine"),
  estuary_name = "Seine", colour_var = ggplot2_colors(3)[1],
  size_var = 3
) + theme_esteem() +
  theme(legend.position = "none", axis.title = ggplot2::element_blank())

# ---- Cowplot ----
ggmap_ROCCHMV <- plot_grid(ggmap_ROCCHMV_gironde,
                           ggmap_ROCCHMV_loire,
                           ggmap_ROCCHMV_seine,
                           nrow = 3, ncol = 1, rel_heights = c(1.4,1,1))

ggsave(plot = ggmap_ROCCHMV, filename = "inst/mat_meth/contamination/ggmap_ROCCHMV.jpg",
       height = 10, width = 7, units = "cm")
