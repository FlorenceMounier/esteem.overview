# =====================================================
# Prepare contamination data from ROCCHMV program
# Datasets:
#  - data_contamination_biometrics.rda
#  - data_contamination_TEF_DLC.rda
#  - data_contamination_no_correction.rda
#  - data_contamination_full_corr.rda
#  - data_contamination_convert_factors.rda
#  - data_contamination.rda
# Graphs in /inst/mat_meth/contamination
#  - ggplot_dry_content.jpg
#  - ggplot_lipid_content.jpg
#  - ggplot_contamination_loire_parallel_sampling.jpg
#  - ggmap_ROCCHMV.jpg
# Preparation script
# Author: FM
# Date: 2026-07-01
# =====================================================



# =====================================================
# 00. Packages, functions and raw data from {quadrige.explorer}
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
library(cowplot)

`%!in%` = Negate(`%in%`)

# From /data-raw/0.2-RAW-data_surval.R
data(raw_data_surval_contamination)

# =====================================================
# 01. Cleaning raw data
# =====================================================

data_ROCCHMV_clean <- raw_data_surval_contamination |>

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
# 04. Sampling maps
# =====================================================

# ---- Gironde ----
ggmap_ROCCHMV_gironde <- plot_estuary_map(
  data = data_ROCCHMV_clean |> filter(estuary == "Gironde"),
  estuary_name = "Gironde", colour_var = ggplot2_colors(3)[3],
  size_var = 3
) + theme_esteem() +
  theme(legend.position = "none", axis.title = ggplot2::element_blank())

# ---- Loire ----
ggmap_ROCCHMV_loire <- plot_estuary_map(
  data = data_ROCCHMV_clean |> filter(estuary == "Loire"),
  estuary_name = "Loire", colour_var = ggplot2_colors(3)[2],
  size_var = 3
) + theme_esteem() +
  theme(legend.position = "none", axis.title = ggplot2::element_blank())


# ---- Seine ----
ggmap_ROCCHMV_seine <- plot_estuary_map(
  data = data_ROCCHMV_clean |> filter(estuary == "Seine"),
  estuary_name = "Seine", colour_var = ggplot2_colors(3)[1],
  size_var = 3
) + theme_esteem() +
  theme(legend.position = "none", axis.title = ggplot2::element_blank())

# ---- Cowplot ----
ggmap_ROCCHMV <- plot_grid(ggmap_ROCCHMV_gironde,
                           ggmap_ROCCHMV_loire,
                           ggmap_ROCCHMV_seine,
                           nrow = 3, ncol = 1, rel_heights = c(1.4,1,1))

ggsave(plot = ggmap_ROCCHMV,
       filename = "inst/mat_meth/contamination/ggmap_ROCCHMV.jpg",
       height = 10, width = 7, units = "cm")



# =====================================================
# 02. Biometrics
# =====================================================


### DRY CONTENT ###

# ---- Filter dry content data ----

data_ROCCHMV_bio_dry_content <- data_ROCCHMV_clean |>
  filter(PARAMETRE_LIBELLE == "Matière sèche")

# ---- Summarise by estuary, year & species ----

data_ROCCHMV_bio_dry_content_summarised <- data_ROCCHMV_bio_dry_content  |>
  filter(PARAMETRE_LIBELLE == "Matière sèche") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  group_by(estuary, year, species) |>
  summarise(percent_dw = mean(`Matière sèche`, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# ---- Graph of time evolution ----

ggplot_dry_content <- ggplot(data_ROCCHMV_bio_dry_content_summarised) +
  aes(x = year, y = percent_dw, fill = estuary, colour = estuary) +
  geom_line() +
  facet_grid(vars(species)) +
  labs(y = "Dry content (%)")
ggsave(ggplot_dry_content, filename = "inst/mat_meth/contamination/ggplot_dry_content.jpg",
       height = 15, width = 10, units = "cm")

# ---- Summarise by estuary & species ----

dry_contents <- data_ROCCHMV_bio_dry_content |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT) |>
  group_by(estuary, species) |>
  summarise(percent_dw = mean(`Matière sèche`, na.rm = TRUE), .groups = "drop")

# Gironde C. gigas: 15.3 %
# Loire M.edulis: 18.4 %
# Loire M. edulis & galloprovincialis: 18.4 %
# Seine M.edulis: 23.3 %


### LIPID CONTENT ###

# ---- Compute lipid content per glw and gww from dry contents ----

data_ROCCHMV_bio_lipid <- data_ROCCHMV_clean  |>
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

# ---- Summarise by estuary, year & species ----

data_ROCCHMV_bio_lipid_content_summarised <- data_ROCCHMV_bio_lipid |>
  group_by(estuary, year, species) |>
  summarise(percent_ldw = mean(percent_ldw, na.rm = TRUE),
            percent_lww = mean(percent_lww, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# ---- Graph of time evolution ----

ggplot_lipid_content <- ggplot(data_ROCCHMV_bio_lipid_content_summarised) +
  aes(x = year, y = percent_ldw, fill = estuary, colour = estuary) +
  geom_line() +
  facet_grid(vars(species)) +
  labs(y = "Lipid content (%dw)")
ggsave(ggplot_lipid_content, filename = "inst/mat_meth/contamination/ggplot_lipid_content.jpg",
       height = 15, width = 10, units = "cm")

# ---- Summarise by estuary & species ----

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


### SAVE ALL BIOMETRICS ###

data_contamination_biometrics <- full_join(dry_contents, lipid_contents)
usethis::use_data(data_contamination_biometrics, overwrite = TRUE)



# =====================================================
# 03. Contamination data cleaning
# =====================================================

data_ROCCHMV_contam_completed <- data_ROCCHMV_clean |>

# ---- Filter not biometrics ----

filter(PARAMETRE_LIBELLE %!in% c("Matière sèche", "Lipides totaux")) |>


# ---- Add lipid contents %ldw & %lww ----

left_join(data_contamination_biometrics) |>
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
  PARAMETRE_LIBELLE == "Tributylétain cation" ~ "TBT",
  # Others + biometrics parameters
  TRUE ~ PARAMETRE_LIBELLE
)) |>


# ---- Add sub families ----

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
  PARAMETRE_LIBELLE %in% c("CB 28", "CB 52", "CB 101", "CB 118", "CB 138", "CB 153", "CB 180") ~ "PCBi",

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
  PARAMETRE_LIBELLE %in% c("PBDE 28", "PBDE 47", "PBDE 99", "PBDE 100", "PBDE 153", "PBDE 154") ~ "PBDEs",

  # -- PFAS - PFOS
  PARAMETRE_LIBELLE == "PFOS" ~ "PFAS",

  # -- Organostannic compounds - Tributyltin cation
  PARAMETRE_LIBELLE == "TBT" ~ "Organotin compounds"
)) |>


# ---- Add families ----

mutate(family = case_when(
  sub_family %in% c("HBCDD", "PBDE") ~ "Brominated",
  sub_family == "Metals" ~ "Metals",
  sub_family %in% c("DDT total", "Lindane") ~ "Organochlorin pesticides",
  sub_family ==  "PAH" ~ "PAH",
  sub_family ==  "PCBi" ~ "PCB",
  sub_family %in% c("PCDDs", "PCDFs", "PCB-DL", "DL-PCBs") ~ "DLC",
  sub_family %in% c("HBCDDs", "PBDEs") ~ "BFR",
  sub_family == "PFAS" ~ "Perfluorinated compounds",
  sub_family == "Organotin compounds" ~ "Organometallic compounds",
  TRUE ~ NA
))



# =====================================================
# 02. Duplicate case of CB118: PCBi & DL-PCB
# =====================================================

# ---- Extract subdataset ----
data_ROCCHMV_CB118 <- data_ROCCHMV_contam_completed |>
  filter(PARAMETRE_LIBELLE == "CB 118") |>
  mutate(sub_family = "DL-PCBs") |>
  mutate(family = "DLC")

# ---- Duplicate rows with DL-PCB data_ROCCHMV_contam_completed ----
data_ROCCHMV_contam_completed <- rbind(data_ROCCHMV_contam_completed, data_ROCCHMV_CB118)


# =====================================================
# 03. Summarize contaminant concentrations by family
#     or sub-family where the threshold applies to them
# =====================================================

# ---- DDT total ----

data_ROCCHMV_contam_DDT_total_ng_gdw <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "DDT total") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_gdw) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_gdw = sum(`p,p'-DDD`, `p,p'-DDE`, `p,p'-DDT`,`o,p'-DDT`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "DDT total")

data_ROCCHMV_contam_DDT_total_ng_gww <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "DDT total") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_gww) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_gww = sum(`p,p'-DDD`, `p,p'-DDE`, `p,p'-DDT`,`o,p'-DDT`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "DDT total")

data_ROCCHMV_contam_DDT_total_ng_glw <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "DDT total") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_glw) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_glw = sum(`p,p'-DDD`, `p,p'-DDE`, `p,p'-DDT`,`o,p'-DDT`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "DDT total")

data_ROCCHMV_contam_DDT_total <- data_ROCCHMV_contam_DDT_total_ng_gdw |>
  full_join(data_ROCCHMV_contam_DDT_total_ng_gww) |>
  full_join(data_ROCCHMV_contam_DDT_total_ng_glw)


# ---- HBCDDs ----

data_ROCCHMV_contam_sum_HBCDDs_ng_gdw <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "HBCDDs") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_gdw) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_gdw = sum(`Alpha-HBCDD`, `Beta-HBCDD`, `Gamma-HBCDD`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sum_HBCDDs")

data_ROCCHMV_contam_sum_HBCDDs_ng_gww <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "HBCDDs") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_gww) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_gww = sum(`Alpha-HBCDD`, `Beta-HBCDD`, `Gamma-HBCDD`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sum_HBCDDs")

data_ROCCHMV_contam_sum_HBCDDs_ng_glw <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "HBCDDs") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_glw) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_glw = sum(`Alpha-HBCDD`, `Beta-HBCDD`, `Gamma-HBCDD`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sum_HBCDDs")

data_ROCCHMV_contam_sum_HBCDDs <- data_ROCCHMV_contam_sum_HBCDDs_ng_gdw |>
  full_join(data_ROCCHMV_contam_sum_HBCDDs_ng_gww) |>
  full_join(data_ROCCHMV_contam_sum_HBCDDs_ng_glw)



# ---- PBDEs ----

## sum_PBDEs_ng_glw
data_ROCCHMV_contam_sum_PBDEs_ng_gdw <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "PBDEs") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_gdw) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_gdw = sum(`PBDE 28`, `PBDE 47`, `PBDE 99`, `PBDE 100`, `PBDE 153`, `PBDE 154`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sum_PBDEs")

## sum_PBDEs_ng_gww
data_ROCCHMV_contam_sum_PBDEs_ng_gww <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "PBDEs") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_gww) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_gww = sum(`PBDE 28`, `PBDE 47`, `PBDE 99`, `PBDE 100`, `PBDE 153`, `PBDE 154`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sum_PBDEs")

## sum_PBDEs_ng_glw
data_ROCCHMV_contam_sum_PBDEs_ng_glw <- data_ROCCHMV_contam_completed |>
  filter(sub_family == "PBDEs") |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_ng_glw) |>
  group_by(estuary, year, species, family, sub_family) |>
  summarise(RESULTAT_ng_glw = sum(`PBDE 28`, `PBDE 47`, `PBDE 99`, `PBDE 100`, `PBDE 153`, `PBDE 154`, na.rm = TRUE),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "sum_PBDEs")

## join units
data_ROCCHMV_contam_sum_PBDEs <- data_ROCCHMV_contam_sum_PBDEs_ng_gdw |>
  full_join(data_ROCCHMV_contam_sum_PBDEs_ng_gww) |>
  full_join(data_ROCCHMV_contam_sum_PBDEs_ng_glw)


# ---- Weighted sum of DLC (PCDD, PCDF, DL-PCB) ----

## Weight factors dataset
data_contamination_TEF_DLC <- data_ROCCHMV_contam_completed |>
  filter(family == "DLC") |>
  distinct(PARAMETRE_LIBELLE, sub_family) |>
  mutate(TEF_DLC = case_when(
    PARAMETRE_LIBELLE %in% c("2,3,7,8-tetrachlorodibenzo-p-dioxine",
                             "1,2,3,7,8-pentachlorodibenzo-p-dioxine") ~ 1,
    PARAMETRE_LIBELLE %in% c("2,3,4,7,8-pentachlorodibenzofuran") ~ 0.3,
    PARAMETRE_LIBELLE %in% c("1,2,3,4,7,8-hexachlorodibenzo-p-dioxine",
                             "1,2,3,6,7,8-hexachlorodibenzo-p-dioxine",
                             "1,2,3,7,8,9-hexachlorodibenzo-p-dioxine",
                             "2,3,7,8-tetrachlorodibenzofuran",
                             "1,2,3,4,7,8-hexachlorodibenzofuran",
                             "1,2,3,6,7,8-hexachlorodibenzofuran",
                             "1,2,3,7,8,9-hexachlorodibenzofuran",
                             "2,3,4,6,7,8-hexachlorodibenzofuran",
                             "1,2,3,4,6,7,8-heptachlorodibenzofuran",
                             "1,2,3,4,7,8,9-heptachlorodibenzofuran",
                             "CB 126"
                             ) ~ 0.1,
    PARAMETRE_LIBELLE == "1,2,3,4,6,7,8- heptachlorodibenzo-p-dioxine" ~ 0.01,
    PARAMETRE_LIBELLE %in% c("1,2,3,7,8-pentachlorodibenzofuran", "CB 169") ~ 0.03,
    PARAMETRE_LIBELLE %in% c("octachlorodibenzo-p-dioxine",
                             "octachlorodibenzofuranne",
                             "CB 81") ~ 0.0003,
    PARAMETRE_LIBELLE %in% c("CB 77") ~ 0.0001,
    PARAMETRE_LIBELLE %in% c("CB 105", "CB 114", "CB 118", "CB 123", "CB 156", "CB 157", "CB 167", "CB 189") ~ 0.00003
  )) |>
  arrange(desc(TEF_DLC), sub_family, PARAMETRE_LIBELLE)

usethis::use_data(data_contamination_TEF_DLC, overwrite = TRUE)

## DLC names vector
DLC_names <- data_contamination_TEF_DLC |>  pull(PARAMETRE_LIBELLE)

## Weighted contamination levels
data_ROCCHMV_contam_W_ng_g <- data_ROCCHMV_contam_completed |>
  filter(family == "DLC") |>
  left_join(data_contamination_TEF_DLC) |>
  mutate(RESULTAT_W_ng_gdw = RESULTAT_ng_gdw * TEF_DLC,
         RESULTAT_W_ng_gww = RESULTAT_ng_gww * TEF_DLC,
         RESULTAT_W_ng_glw = RESULTAT_ng_glw * TEF_DLC)

## Wsum_DLC_ng_gdw
data_ROCCHMV_contam_Wsum_DLC_ng_gdw <- data_ROCCHMV_contam_W_ng_g |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_W_ng_gdw) |>
  group_by(estuary, year, species, family) |>
  summarise(RESULTAT_ng_gdw = sum(rowSums(across(all_of(DLC_names)), na.rm = TRUE)),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "Wsum_DLC")

## Wsum_DLC_ng_gww
data_ROCCHMV_contam_Wsum_DLC_ng_gww <- data_ROCCHMV_contam_W_ng_g |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_W_ng_gww) |>
  group_by(estuary, year, species, family) |>
  summarise(RESULTAT_ng_gww = sum(rowSums(across(all_of(DLC_names)), na.rm = TRUE)),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "Wsum_DLC")

## Wsum_DLC_ng_glw
data_ROCCHMV_contam_Wsum_DLC_ng_glw <- data_ROCCHMV_contam_W_ng_g |>
  pivot_wider(names_from = PARAMETRE_LIBELLE, values_from = RESULTAT_W_ng_glw) |>
  group_by(estuary, year, species, family) |>
  summarise(RESULTAT_ng_glw = sum(rowSums(across(all_of(DLC_names)), na.rm = TRUE)),
            .groups = "drop") |>
  mutate(PARAMETRE_LIBELLE = "Wsum_DLC")


## join units
data_ROCCHMV_contam_Wsum_DLC <- data_ROCCHMV_contam_Wsum_DLC_ng_gdw |>
  full_join(data_ROCCHMV_contam_Wsum_DLC_ng_gww) |>
  full_join(data_ROCCHMV_contam_Wsum_DLC_ng_glw)


# ---- Join new variables ----

## DLC names vector without CB118
DLC_names_non_CB118 <- data_contamination_TEF_DLC |> filter(PARAMETRE_LIBELLE != "CB 118") |>  pull(PARAMETRE_LIBELLE)

data_ROCCHMV_contam <- data_ROCCHMV_contam_completed |>
  full_join(data_ROCCHMV_contam_DDT_total) |>
  filter(PARAMETRE_LIBELLE %!in% c("p,p'-DDE", "p,p'-DDD", "p,p'-DDT", "o,p'-DDT")) |>
  full_join(data_ROCCHMV_contam_sum_HBCDDs) |>
  filter(PARAMETRE_LIBELLE %!in% c("Alpha-HBCDD", "Beta-HBCDD", "Gamma-HBCDD")) |>
  full_join(data_ROCCHMV_contam_sum_PBDEs) |>
  filter(PARAMETRE_LIBELLE %!in% c("PBDE 28", "PBDE 47", "PBDE 99", "PBDE 100", "PBDE 153", "PBDE 154")) |>
  full_join(data_ROCCHMV_contam_Wsum_DLC) |>
  filter(PARAMETRE_LIBELLE %!in% DLC_names_non_CB118) |>
  filter(!(PARAMETRE_LIBELLE == "CB 118" & family == "DLC"))

# # Check resulting list of contaminants
# data_ROCCHMV_contam |>
# distinct(PARAMETRE_LIBELLE, family) |>  View()


# =====================================================
# 04. Summarise by estuary, year & group
# =====================================================

data_contamination_no_correction <- data_ROCCHMV_contam |>
  mutate(group = case_when(
    species == "C. gigas" ~ "Oyster",
    species %in% c("M. edulis", "M. edulis & galloprovincialis") ~ "Mussels"
  )) |>
  group_by(estuary, year, group, PARAMETRE_LIBELLE, family, sub_family) |>
  summarise(
    RESULTAT_ng_gdw = median(RESULTAT_ng_gdw, na.rm = TRUE),
    RESULTAT_ng_gww = median(RESULTAT_ng_gww, na.rm = TRUE),
    RESULTAT_ng_glw = median(RESULTAT_ng_glw, na.rm = TRUE),
    .groups = "drop"
  )

usethis::use_data(data_contamination_no_correction, overwrite = TRUE)

# =====================================================
# 05. Check species matrix influence and correct if necessary
# =====================================================

# ---- Identify parallel sampling ----
data_2_groups <- data_ROCCHMV_contam_summarized |>
  distinct(PARAMETRE_LIBELLE, year, estuary, group) |>
  group_by(PARAMETRE_LIBELLE, year, estuary) |>
  summarise(n_group = n_distinct(group), .groups = "drop") |>
  filter(n_group > 1)

# ---- Estuary and period of parallel sampling ----
data_2_groups |> distinct(estuary, year) |> arrange(year)

# ---- Graphical representation of contamination level differences ----
ggplot_contamination_loire_parallel_sampling <- ggplot(data_ROCCHMV_contam_summarized |> filter(estuary == "Loire")) +
  aes(x = year, y = RESULTAT_ng_gdw, colour = group) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(PARAMETRE_LIBELLE), ncol = 4, scale = "free_y") +
  labs(y = "Concentration (ng/gdw)") +
  theme(axis.title.x = element_blank(),
        legend.position="bottom",
        axis.text.x = element_text(angle = 60, vjust = 0.9, hjust=1))
# only copper seems to have really different contamination levels
# despite the natural variability of contamination
ggsave(ggplot_contamination_loire_parallel_sampling,
       filename = "inst/mat_meth/contamination/ggplot_contamination_loire_parallel_sampling.jpg",
       height = 22, width = 18, units = "cm")


# ---- Estimate correction factors for all contaminants ----

# 1. Identify the years in which both groups are available

df_overlap <- data_ROCCHMV_contam_summarized |>
  select(PARAMETRE_LIBELLE, family, sub_family, year, group,
         RESULTAT_ng_gdw, RESULTAT_ng_gww, RESULTAT_ng_glw) |>
  group_by(PARAMETRE_LIBELLE, family, sub_family, year, group) |>
  summarise(
    RESULTAT_ng_gdw = median(RESULTAT_ng_gdw, na.rm = TRUE),
    RESULTAT_ng_gww = median(RESULTAT_ng_gww, na.rm = TRUE),
    RESULTAT_ng_glw = median(RESULTAT_ng_glw, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = group,
    values_from = c(RESULTAT_ng_gdw, RESULTAT_ng_gww, RESULTAT_ng_glw),
    names_sep = "_"
  ) |>
  filter(
    !is.na(RESULTAT_ng_gdw_Oyster), !is.na(RESULTAT_ng_gdw_Mussels),
    !is.na(RESULTAT_ng_gww_Oyster), !is.na(RESULTAT_ng_gww_Mussels),
    !is.na(RESULTAT_ng_glw_Oyster), !is.na(RESULTAT_ng_glw_Mussels)
  ) |>
  mutate(
    log_ratio_ng_gdw = log10((RESULTAT_ng_gdw_Oyster + 1) / (RESULTAT_ng_gdw_Mussels + 1)),
    log_ratio_ng_gww = log10((RESULTAT_ng_gww_Oyster + 1) / (RESULTAT_ng_gww_Mussels + 1)),
    log_ratio_ng_glw = log10((RESULTAT_ng_glw_Oyster + 1) / (RESULTAT_ng_glw_Mussels + 1)),
    ratio_Oyster_Mussels_year_ng_gdw = (RESULTAT_ng_gdw_Oyster + 1) / (RESULTAT_ng_gdw_Mussels + 1),
    ratio_Oyster_Mussels_year_ng_gww = (RESULTAT_ng_gww_Oyster + 1) / (RESULTAT_ng_gww_Mussels + 1),
    ratio_Oyster_Mussels_year_ng_glw = (RESULTAT_ng_glw_Oyster + 1) / (RESULTAT_ng_glw_Mussels + 1)
  )

# 2. Estimate a correction factor for each contaminant
# when n = 3 points
# when diff +/- 50% (ratio != 1)

estimate_correction_factor <- function(dat,
                                       min_pairs = 3,
                                       factor_threshold = 1.5,
                                       unit) {

  n_pairs <- nrow(dat)

  if (n_pairs < min_pairs) {
    return(tibble(
      n_pairs = n_pairs,
      mean_log_ratio = NA_real_,
      conf_low_log10 = NA_real_,
      conf_high_log10 = NA_real_,
      ratio_Mussels_Oyster = NA_real_,
      ratio_low = NA_real_,
      ratio_high = NA_real_,
      p_value = NA_real_,
      correction_factor = 1,
      correction_applied = FALSE,
      reason = "Not enough paired years"
    ))
  }

  log_ratio_col_name <- paste("log_ratio", unit, sep = "_")
  tt <- t.test(dat[log_ratio_col_name], mu = 0)

  mean_log_ratio <- unname(tt$estimate)
  ratio <- 10^mean_log_ratio
  ratio_low <- 10^tt$conf.int[1]
  ratio_high <- 10^tt$conf.int[2]

  marked_effect <- ratio >= factor_threshold | ratio <= 1 / factor_threshold
  significant_raw <- tt$p.value < 0.05

  apply_correction <- marked_effect & significant_raw

  tibble(
    n_pairs = n_pairs,
    mean_log_ratio = mean_log_ratio,
    conf_low_log10 = tt$conf.int[1],
    conf_high_log10 = tt$conf.int[2],
    ratio_Oyster_Mussels = ratio,
    ratio_low = ratio_low,
    ratio_high = ratio_high,
    p_value = tt$p.value,
    correction_factor = if_else(apply_correction, ratio, 1),
    correction_applied = apply_correction,
    reason = case_when(
      apply_correction ~ "Correction applied",
      !marked_effect ~ "Effect size below threshold",
      !significant_raw ~ "Difference not statistically clear",
      TRUE ~ "No correction"
    )
  )
}

# 3. Table of correction factors

fct_correction_table <- function(unit){
  correction_table <- df_overlap |>
  group_by(PARAMETRE_LIBELLE, family, sub_family) |>
  nest() |>
  mutate(
    correction = map(data, .f = estimate_correction_factor, unit = unit)
  ) |>
  select(-data) |>
  unnest(correction) |>
  ungroup() |>
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH"),
    direction = case_when(
      is.na(ratio_Oyster_Mussels) ~ "Not tested",
      ratio_Oyster_Mussels > 1 ~ "Mussels > Oyster",
      ratio_Oyster_Mussels < 1 ~ "Mussels < Oyster",
      TRUE ~ "No difference"
    ),
    ratio_txt = case_when(
      is.na(ratio_Oyster_Mussels) ~ NA_character_,
      TRUE ~ sprintf(
        "%.2f [%.2f–%.2f]",
        ratio_Oyster_Mussels, ratio_low, ratio_high
      )
    )
  ) |>
  arrange(desc(correction_applied), PARAMETRE_LIBELLE)

  return(correction_table)
}

correction_table_ng_gdw <- fct_correction_table(unit = "ng_gdw")
correction_table_ng_gww <- fct_correction_table(unit = "ng_gww")
correction_table_ng_glw <- fct_correction_table(unit = "ng_glw")

# 4. Apply the correction to the entire game

data_contamination_full_corr <- data_ROCCHMV_contam_summarized |>
  left_join(
    correction_table_ng_gdw |>
      select(PARAMETRE_LIBELLE, correction_factor, correction_applied) |>
      rename(correction_factor_ng_gdw = correction_factor,
             correction_applied_ng_gdw = correction_applied),
    by = "PARAMETRE_LIBELLE"
  ) |>
  left_join(
    correction_table_ng_gww |>
      select(PARAMETRE_LIBELLE, correction_factor, correction_applied) |>
      rename(correction_factor_ng_gww = correction_factor,
             correction_applied_ng_gww = correction_applied),
    by = "PARAMETRE_LIBELLE"
  ) |>
  left_join(
    correction_table_ng_glw |>
      select(PARAMETRE_LIBELLE, correction_factor, correction_applied) |>
      rename(correction_factor_ng_glw = correction_factor,
             correction_applied_ng_glw = correction_applied),
    by = "PARAMETRE_LIBELLE"
  ) |>
  mutate(
    correction_factor_ng_gdw = replace_na(correction_factor_ng_gdw, 1),
    correction_factor_ng_gww = replace_na(correction_factor_ng_gww, 1),
    correction_factor_ng_glw = replace_na(correction_factor_ng_glw, 1),
    correction_applied_ng_gdw = replace_na(correction_applied_ng_gdw, FALSE),
    correction_applied_ng_gww = replace_na(correction_applied_ng_gww, FALSE),
    correction_applied_ng_glw = replace_na(correction_applied_ng_glw, FALSE),

    RESULTAT_ng_gdw = case_when(
      group == "Mussels" ~ RESULTAT_ng_gdw,
      group == "Oyster" ~ RESULTAT_ng_gdw / correction_factor_ng_gdw,
      TRUE ~ RESULTAT_ng_gdw
    ),
    RESULTAT_ng_gww = case_when(
      group == "Mussels" ~ RESULTAT_ng_gww,
      group == "Oyster" ~ RESULTAT_ng_gww / correction_factor_ng_gww,
      TRUE ~ RESULTAT_ng_gww
    ),
    RESULTAT_ng_glw = case_when(
      group == "Mussels" ~ RESULTAT_ng_glw,
      group == "Oyster" ~ RESULTAT_ng_glw / correction_factor_ng_glw,
      TRUE ~ RESULTAT_ng_glw
    )
  )

usethis::use_data(data_contamination_full_corr, overwrite = TRUE)

# 5. Graphical check before/after correction

df_mussel_equiv_ng_gdw <- data_ROCCHMV_contam_summarized |>
  left_join(
    correction_table_ng_gdw |>
      select(PARAMETRE_LIBELLE, correction_factor, correction_applied),
    by = "PARAMETRE_LIBELLE"
  ) |>
  mutate(
    correction_factor = replace_na(correction_factor, 1),
    correction_applied = replace_na(correction_applied, FALSE),

    RESULTAT_ng_gdw_mussel_equiv = case_when(
      group == "Mussels" ~ RESULTAT_ng_gdw,
      group == "Oyster" ~ RESULTAT_ng_gdw / correction_factor,
      TRUE ~ RESULTAT_ng_gdw
    )
  )

df_plot_before_after_ng_gdw <- df_mussel_equiv_ng_gdw |>
  select(
    PARAMETRE_LIBELLE, family, sub_family, year, group,
    RESULTAT_ng_gdw,
    RESULTAT_ng_gdw_mussel_equiv,
    correction_applied
  ) |>
  filter(correction_applied == TRUE) |>
  pivot_longer(
    cols = c(RESULTAT_ng_gdw, RESULTAT_ng_gdw_mussel_equiv),
    names_to = "type",
    values_to = "value"
  ) |>
  mutate(
    type = recode(
      type,
      RESULTAT_ng_gdw = "Observed",
      RESULTAT_ng_gdw_mussel_equiv = "Mussel-equivalent"
    )
  )

ggplot(df_plot_before_after_ng_gdw, aes(year, value, colour = group)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  facet_grid(type ~ PARAMETRE_LIBELLE, scales = "free_y") +
  labs(
    x = "Year",
    y = "Concentration ng/g dw",
    colour = "Group"
  ) +
  theme_bw()


# ---- Estimate correction factors for Copper, Cadmium & Mercury ----
data_ROCCHMV_contam_summarized_2_groups <- left_join(data_2_groups, data_ROCCHMV_contam_summarized)

ggplot(data_ROCCHMV_contam_summarized_2_groups |> filter(estuary == "Loire")) +
  aes(x = year, y = RESULTAT_ng_glw, colour = group) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(PARAMETRE_LIBELLE), scale = "free_y")

data_contamination_convert_factors <- data_ROCCHMV_contam_summarized_2_groups |>
  filter(PARAMETRE_LIBELLE %in% c("Copper", "Cadmium", "Mercury")) |>
  select(PARAMETRE_LIBELLE, year, group,
         RESULTAT_ng_glw, RESULTAT_ng_gww, RESULTAT_ng_glw) |> pivot_longer(
           cols = starts_with("RESULTAT_ng_"),
           names_to = "metric",
           values_to = "value"
         ) |>
  pivot_wider(
    names_from = group,
    values_from = value
  ) |>
  mutate(
    ratio_Oyster_Mussels = Oyster / Mussels,
    ratio_Mussels_Oyster = Mussels / Oyster
  ) |>
  group_by(PARAMETRE_LIBELLE, metric) |>
  summarise(
    median_ratio_Oyster_Mussels = median(ratio_Oyster_Mussels, na.rm = TRUE),
    median_ratio_Mussels_Oyster = median(ratio_Mussels_Oyster, na.rm = TRUE),
    .groups = "drop"
  ) |>
  select(
    PARAMETRE_LIBELLE,
    metric,
    median_ratio_Oyster_Mussels
  ) |>
  pivot_wider(
    names_from = metric,
    values_from = median_ratio_Oyster_Mussels
  )

usethis::use_data(data_contamination_convert_factors, overwrite = TRUE)

# ---- Transform raw data ----

data_contamination <- data_ROCCHMV_contam_summarized |>
  mutate(RESULTAT_ng_gdw = case_when(
    PARAMETRE_LIBELLE == "Cadmium" & group == "Oyster" ~ RESULTAT_ng_gdw *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Cadmium") |> pull("RESULTAT_ng_gdw"),
    PARAMETRE_LIBELLE == "Copper" & group == "Oyster" ~ RESULTAT_ng_gdw *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Copper") |> pull("RESULTAT_ng_gdw"),
    PARAMETRE_LIBELLE == "Mercury" & group == "Oyster" ~ RESULTAT_ng_gdw *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Mercury") |> pull("RESULTAT_ng_gdw"),
    TRUE ~ RESULTAT_ng_gdw
  )) |>
  mutate(RESULTAT_ng_gww = case_when(
    PARAMETRE_LIBELLE == "Cadmium" & group == "Oyster" ~ RESULTAT_ng_gww *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Cadmium") |> pull("RESULTAT_ng_gww"),
    PARAMETRE_LIBELLE == "Copper" & group == "Oyster" ~ RESULTAT_ng_gww *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Copper") |> pull("RESULTAT_ng_gww"),
    PARAMETRE_LIBELLE == "Mercury" & group == "Oyster" ~ RESULTAT_ng_gww *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Mercury") |> pull("RESULTAT_ng_gww"),
    TRUE ~ RESULTAT_ng_gww
  )) |>
  mutate(RESULTAT_ng_glw = case_when(
    PARAMETRE_LIBELLE == "Cadmium" & group == "Oyster" ~ RESULTAT_ng_glw *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Cadmium") |> pull("RESULTAT_ng_glw"),
    PARAMETRE_LIBELLE == "Copper" & group == "Oyster" ~ RESULTAT_ng_glw *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Copper") |> pull("RESULTAT_ng_glw"),
    PARAMETRE_LIBELLE == "Mercury" & group == "Oyster" ~ RESULTAT_ng_glw *
      data_contamination_convert_factors |> filter(PARAMETRE_LIBELLE == "Mercury") |> pull("RESULTAT_ng_glw"),
    TRUE ~ RESULTAT_ng_glw
  ))

usethis::use_data(data_contamination, overwrite = TRUE)
