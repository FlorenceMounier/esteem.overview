library(tidyverse)

seine_estuary <- tribble(
  ~year, ~zone, ~surface_ha,

  1840, "Subtidal",     11500,
  1840, "Oligohaline",  10500,
  1840, "Mesohaline",    1500,
  1840, "Polyhaline",     800,
  1840, "Euhaline",       300,

  1930, "Subtidal",      9500,
  1930, "Oligohaline",   6500,
  1930, "Mesohaline",    1500,
  1930, "Polyhaline",     500,
  1930, "Euhaline",       200,

  1960, "Subtidal",     10000,
  1960, "Oligohaline",   5000,
  1960, "Mesohaline",    1200,
  1960, "Polyhaline",     400,
  1960, "Euhaline",       150,

  1970, "Subtidal",     10200,
  1970, "Oligohaline",   4500,
  1970, "Mesohaline",    1200,
  1970, "Polyhaline",     350,
  1970, "Euhaline",       150,

  1990, "Subtidal",      9800,
  1990, "Oligohaline",   3500,
  1990, "Mesohaline",    1000,
  1990, "Polyhaline",     300,
  1990, "Euhaline",       150,

  2020, "Subtidal",     10500,
  2020, "Oligohaline",   2500,
  2020, "Mesohaline",     900,
  2020, "Polyhaline",     250,
  2020, "Euhaline",       100
)

ggplot(seine_estuary) +
  aes(x = year, y = surface_ha, fill = zone) +
  geom_col()
