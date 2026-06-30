# =====================================================
# Preparation script
# Datasets:
#  -
# Graphs in /int/
#  -
# Author: FM
# Date: 2026-06-30
# =====================================================



# =====================================================
# 03. Campaign frequency
# =====================================================

gantt_data <- data_ROCCHMV_contamination |>
  group_by(estuary, PARAMETRE_LIBELLE) |>
  mutate(
    grp = cumsum(c(TRUE, diff(year) > 1))
  ) |>
  group_by(
    estuary,
    PARAMETRE_LIBELLE,
    grp
  ) |>
  summarise(
    start_year = min(year),
    end_year   = max(year),
    .groups = "drop"
  )

gantt_data <- ggplot(
  gantt_data,
  aes(
    x = start_year,
    xend = end_year,
    y = PARAMETRE_LIBELLE,
    yend = PARAMETRE_LIBELLE
  )
) +
  geom_segment(
    linewidth = 1,
    lineend = "round"
  ) +
  facet_grid(vars(estuary)) +
  theme_esteem() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

gantt_data

