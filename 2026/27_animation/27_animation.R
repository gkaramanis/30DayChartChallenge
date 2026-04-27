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

# Monthly new car registrations by fuel type, Uppsala municipality, 2013–2024
# Source: SCB Statistikdatabasen, PersBilarDrivMedel
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/

raw <- read_csv(
  here::here("2026/data/scb_nyreg_bilar_drivmedel_Uppsala.csv"),
  show_col_types = FALSE
)

bubbles <- raw |>
  pivot_longer(starts_with("Antal"), names_to = "ym", values_to = "n") |>
  mutate(
    ym = str_remove(ym, "^Antal "),
    year = as.integer(str_sub(ym, 1, 4))
  ) |>
  filter(year >= 2013, year <= 2024) |>
  mutate(
    fuel = case_when(
      drivmedel %in% c("el", "laddhybrid") ~ "Electric / PHEV",
      drivmedel == "elhybrid"              ~ "Hybrid",
      drivmedel %in% c("bensin", "diesel") ~ "Petrol / Diesel",
      TRUE                                 ~ "Other"
    )
  ) |>
  group_by(year, fuel) |>
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = n / sum(n) * 100) |>
  ungroup() |>
  filter(fuel != "Other") |>
  mutate(fuel = factor(fuel, levels = c("Petrol / Diesel", "Hybrid", "Electric / PHEV")))

fuel_colors <- c(
  "Petrol / Diesel" = "#A0A098",
  "Hybrid" = clr_accent2,
  "Electric / PHEV" = clr_accent
)

ggplot(bubbles, aes(x = year, y = share, color = fuel, group = fuel, alpha = year)) +
  geom_line(linewidth = 0.8, show.legend = FALSE) +
  geom_point(aes(size = n)) +
  scale_color_manual(values = fuel_colors, name = NULL) +
  scale_size_area(max_size = 18, guide = "none") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 3)) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  scale_alpha_continuous(range = c(0.2, 1), guide = "none") +
  labs(
    title = "The great Uppsala car swap",
    subtitle = "Bubble size = annual registrations",
    caption = "Source: SCB · Graphic: Georgios Karamanis",
    x = NULL,
    y = "Share of new registrations"
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    legend.position = "top",
    legend.location = "plot",
    legend.text = element_text(margin = margin(l = 3, r = 16)),
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.text.x = element_text(),
    axis.title.y = element_text(color = clr_text2),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 16)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 16, b = 16, l = 16)
  )

record_polaroid()
