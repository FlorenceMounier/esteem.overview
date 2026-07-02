# =====================================================
# Preparation script
# Datasets:
#  -
# Plots:
#  -
# Author: FM
# Date: 2026-07-02
# =====================================================


# =====================================================
# 00. Packages and data
# =====================================================

library(esteem.overview)
library(tidyverse, quietly = TRUE)
`%!in%` = Negate(`%in%`)


# =====================================================
# 01. Correlation between annual average values
# =====================================================

cor_yearly <- cor_parameters_yearly(
  data = data_abiotic_yearly,
  group_vars = c("estuary", "haline_zone", "season"),
  method = "spearman",
  min_n = 6
)

# voir les corrélations les plus fortes
cor_yearly |>
  filter(!is.na(correlation)) |>
  arrange(desc(abs(correlation)))

# corrélations significatives après correction
cor_yearly |>
  filter(p_adjusted < 0.05) |>
  arrange(p_adjusted)
