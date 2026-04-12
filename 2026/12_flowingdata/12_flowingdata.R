library(tidyverse)
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


# Monthly UL ridership (Uppsala city + Enköping + regional) 2024–2025
# Source: UL Statistik — https://www.ul.se/sidfot/om-ul/statistik-for-ul/
ul_raw <- read_csv(here::here("2026/data/ul_ridership_punctuality_2024_2025.csv"), show_col_types = FALSE)

ul <- ul_raw |>
  select(year, month, ul_city_trips) |>
  mutate(month_lab = factor(month.abb[month], levels = month.abb))

ul_slope <- ul |>
  select(month, month_lab, year, ul_city_trips) |>
  pivot_wider(names_from = year, values_from = ul_city_trips, names_prefix = "y") |>
  mutate(direction = if_else(y2025 >= y2024, "up", "down")) |> 
  pivot_longer(y2024:y2025, names_to = "year", values_to = "total") |>
  mutate(year = str_remove(year, "y"))

ggplot(ul_slope, aes(x = year, y = total / 1e6)) +
  geom_line(data = ul_slope, aes(group = month, color = direction), linewidth = 1.8, lineend = "round", arrow = arrow()) +
  geom_point(data = . %>% filter(year == 2024), aes(color = direction), size = 4, stroke = 0) +
  scale_color_manual(values = c("up" = clr_accent2, "down" = clr_accent)) +
  scale_x_discrete(expand = expansion(mult = 0.25)) +
  scale_y_continuous(
    labels = scales::label_number(suffix = "M", accuracy = 0.1),
    expand = expansion(mult = 0.1)
  ) +
  facet_wrap(vars(month_lab), ncol = 3) +
  labs(
    title = "Riding Uppsala",
    subtitle = "Bus journeys declined in 2025",
    caption = "Source: UL · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    panel.grid = element_line(color = clr_grid, linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(2, "lines"),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold", margin = margin(t = 20, b = 5)),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, margin = margin(t = 12, b = 0), hjust = 0.5),
    plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(t = 10, b = 50)),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    plot.margin = margin(t = 20, l = 20, r = 20, b = 18)
  )


record_polaroid()
