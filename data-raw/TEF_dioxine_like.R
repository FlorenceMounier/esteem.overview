# ---------------------------------------------------------------------------
# Libraries ----

library(tidyverse, quietly = TRUE)
library(readxl)

TEF_dioxine_like <- read_xlsx(path = "data-raw/WHO-TEF-2005.xlsx")

usethis::use_data(TEF_dioxine_like, overwrite = TRUE)
