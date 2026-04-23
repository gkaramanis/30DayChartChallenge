library(tidyverse)
library(legendry)
library(camcorder)

gg_record(here::here("2026/temp/"), device = "png", height = 11.08, width = 8, units = "in", dpi = 320)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg      <- "#FAFAF6"
clr_text    <- "#1A1A18"
clr_text2   <- "#6B6B65"
clr_accent  <- "#E8A020"
clr_accent2 <- "#3D4F9F"
clr_grid    <- "#E0E0D8"

# City bus punctuality by month, Uppsala 2023–2025
# Source: UL Statistik

ul_raw <- read_csv(here::here("2026/data/ul_ridership_punctuality_2024_2025.csv"), show_col_types = FALSE)

ul_punct <- ul_raw |>
  select(year, month, month_name, ul_city_punctuality_pct) |>
  mutate(year = factor(year))

season_key <- key_range_manual(
  start = c(0.5, 2.5, 5.5, 8.5, 11.5),
  end   = c(2.45, 5.45, 8.45, 11.45, 12.5),
  name  = c("Winter", "Spring", "Summer", "Autumn", "")
)

ggplot(ul_punct, aes(x = month, y = year, fill = ul_city_punctuality_pct)) +
  geom_tile(color = clr_bg, linewidth = 0.8) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_fill_steps(low = clr_accent, high = clr_accent2, limits = c(86, 98), breaks = seq(86, 98, 3), labels = scales::label_number(suffix = "%"), guide = guide_colorsteps(title = NULL, barwidth = 20, barheight = 0.75)) +
  coord_radial(expand = FALSE, inner.radius = 0.2) +
  guides(theta = guide_axis_nested(key = season_key, angle = 0, type = "bracket")) +
  labs(
    title = "Uppsala city bus punctuality",
    subtitle = "2023–2025, inner to outer ring",
    caption = "Source: UL · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2, base_size = 24) +
  theme_guide(
    bracket = element_line(linewidth = 3, color = clr_grid)
    ) +
  theme(
    legend.position = "top",
    legend.location = "plot",
    panel.grid = element_blank(),
    axis.text.x = element_text(color = clr_text2),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 38)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 48.5)),
    plot.margin = margin(t = 20, r = 40, b = 16, l = 40)
  )

record_polaroid()
