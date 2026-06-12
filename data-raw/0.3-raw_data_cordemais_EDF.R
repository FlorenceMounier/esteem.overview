# =====================================================
# Preparation script
# Datasets:
#  -
# Author: FM
# Date: 2026-06-12
# =====================================================

#------------------------------------------------------------------------------
# Packages

library(tidyverse, quietly = TRUE)


#------------------------------------------------------------------------------
# Read raw datasets

# Import raw sextant output files
EDF_cordemais_loire <- read_csv2("../BDD_Poisson_Ab_All_2013_AnneeN_AvecCrust.csv")
