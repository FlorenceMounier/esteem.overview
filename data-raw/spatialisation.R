## code to prepare `fr_map` ----

# Libraries
library(tidyverse)
library(rnaturalearth)

#  Loading the background map for France
fr_map <- ne_countries(scale = "large", country = "France", returnclass = "sf")

usethis::use_data(fr_map, overwrite = TRUE)
