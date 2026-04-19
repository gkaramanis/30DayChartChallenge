library(tidyverse)
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


# New car registrations by fuel group, Uppsala municipality 2013–2025
# Source: SCB Statistikdatabasen, PersBilarDrivMedel
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/

cars_annual <- read_csv(here::here("2026/data/scb_nyreg_bilar_drivmedel_Uppsala.csv"), show_col_types = FALSE) |>
  pivot_longer(cols = starts_with("Antal "), names_to = "ym", values_to = "n") |>
  mutate(
    year = as.integer(str_sub(ym, 7, 10)),
    fuel_group = case_when(
      drivmedel == "el" ~ "Electric (BEV)",
      drivmedel == "laddhybrid" ~ "Plug-in hybrid",
      drivmedel == "elhybrid" ~ "Hybrid (non-plug-in)",
      TRUE ~ "Petrol, diesel & other"
    ),
    fuel_group = factor(fuel_group, levels = c(
      "Petrol, diesel & other", "Hybrid (non-plug-in)", "Plug-in hybrid", "Electric (BEV)"
    ))
  ) |>
  filter(year >= 2013, year <= 2025) |>
  replace_na(list(n = 0)) |>
  group_by(year, fuel_group) |>
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop")

rechargeable_share <- cars_annual |>
  filter(year == 2025) |>
  mutate(share = n / sum(n)) |>
  filter(fuel_group %in% c("Electric (BEV)", "Plug-in hybrid")) |>
  summarise(share = sum(share)) |>
  pull(share)

fuel_colors <- c(
  "Petrol, diesel & other" = clr_grid,
  "Hybrid (non-plug-in)" = clr_text2,
  "Plug-in hybrid" = clr_accent2,
  "Electric (BEV)" = clr_accent
)

ggplot(cars_annual, aes(x = year, y = n, fill = fuel_group)) +
  geom_area(position = "stack", color = clr_bg, linewidth = 0.3) +
  scale_fill_manual(values = fuel_colors, guide = guide_legend(reverse = TRUE, nrow = 2)) +
  scale_x_continuous(breaks = seq(2013, 2025, 3), expand = expansion(c(0.01, 0.08))) +
  scale_y_continuous(breaks = c(2e3, 4e3, 6e3), labels = scales::label_number(), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Electric revolution in Uppsala",
    subtitle = paste0(round(rechargeable_share * 100), "% of new cars were rechargeable in 2025"),
    caption = "Source: SCB, new car registrations in Uppsala municipality · Graphic: Georgios Karamanis",
    fill = NULL
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    legend.position = "top",
    legend.key.width = unit(1.2, "lines"),
    legend.key.height = unit(0.6, "lines"),
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.title = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 20)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
