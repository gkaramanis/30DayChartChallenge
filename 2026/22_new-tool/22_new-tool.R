library(tidyverse)
library(ggdiceplot)
library(camcorder)

gg_record(here::here("2026/temp/"), device = "png", height = 11.08, width = 8, units = "in", dpi = 320)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg <- "#FAFAF6"
clr_text <- "#1A1A18"
clr_text2 <- "#6B6B65"
clr_accent <- "#E8A020"
clr_accent2 <- "#3D4F9F"
clr_grid <- "#E0E0D8"

# New car registrations by fuel type, Uppsala municipality
# Which seasons drive purchases — and how has that shifted?
# New tool: ggdiceplot — dice-style categorical co-occurrence charts
# Source: SCB Statistikdatabasen, PersBilarDrivMedel
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/

cars_raw <- read_csv(here::here("2026/data/scb_nyreg_bilar_drivmedel_Uppsala.csv"),
                     locale = locale(encoding = "latin1"))

cars_seasonal <- cars_raw |>
  pivot_longer(starts_with("Antal"), names_to = "ym", values_to = "n") |>
  mutate(
    ym = str_remove(ym, "^Antal "),
    year = as.integer(str_sub(ym, 1, 4)),
    month = as.integer(str_sub(ym, 6, 7)),
    season = case_when(
      month %in% 3:5         ~ "Spring",
      month %in% 6:8         ~ "Summer",
      month %in% 9:11        ~ "Autumn",
      month %in% c(12, 1, 2) ~ "Winter"
    ),
    fuel_group = case_when(
      drivmedel == "el"                    ~ "Battery Electric",
      drivmedel == "laddhybrid"            ~ "Plug-in Hybrid",
      drivmedel == "elhybrid"              ~ "Hybrid",
      drivmedel %in% c("bensin", "diesel") ~ "Petrol / Diesel",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(fuel_group), year %in% c(2015, 2018, 2021, 2024)) |>
  group_by(year, fuel_group, season) |>
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  mutate(
    season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter")),
    fuel_group = factor(fuel_group, levels = c("Battery Electric", "Plug-in Hybrid", "Hybrid", "Petrol / Diesel")),
    year = factor(year)
  )

season_colors <- c(
  "Spring" = "#6BAE75",
  "Summer" = clr_accent,
  "Autumn" = "#C46E2A",
  "Winter" = clr_accent2
)

ggplot(cars_seasonal, aes(x = year, y = fuel_group)) +
  geom_dice(aes(dots = season, fill = season, size = share), ndots = 4, pip_scale = 0.97, show.legend = TRUE) +
  scale_fill_manual(values = season_colors, guide = "none") +
  scale_size_area(guide = "none") +
  labs(
    title = "Uppsala's new cars by season",
    subtitle = "Pip size = share of all yearly registrations",
    caption = "Source: SCB Statistikdatabasen · Graphic: Georgios Karamanis"
  ) +
  theme_dice(base_family = f1, base_size = 24) +
  theme(
    legend.position = "top",
    legend.location = "plot",
    legend.text = element_text(size = 14, margin = margin(l = 5, r = 20)),
    legend.key.size = unit(3, "lines"),
    legend.title = element_text(size = 0),
    axis.text = element_text(color = clr_text2),
    plot.background = element_rect(fill = clr_bg, color = NA),
    panel.background = element_rect(fill = clr_bg, color = NA),
    axis.title = element_blank(),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid = element_line(linewidth = 0.5, color = clr_grid),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 41)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 42)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
