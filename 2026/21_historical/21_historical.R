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

# Total cycling path km in Uppsala municipality, 2015–2024
# Source: Uppsala Miljöbarometern
# https://uppsala.miljobarometern.se/trafik/cykel/cykelvagar/table/

cycling_raw <- read_csv(here::here("2026/data/miljobarometern_cykelvagar_km_2015_2024.csv"))

# Use the two all-maintainers series for municipality and urban area
cycling <- cycling_raw |>
  filter(series %in% c(
    "Uppsala kommun (alla väghållare)",
    "Uppsala tätort (alla väghållare)"
  )) |>
  mutate(series = if_else(series == "Uppsala kommun (alla väghållare)", "Municipality", "Urban area"))

growth <- cycling |> 
  group_by(series) |> 
  summarise(diff(range(km)), .groups = "drop") |> 
  pull()

ggplot(cycling, aes(x = year, y = km, color = series)) +
  geom_line(linewidth = 1.8, lineend = "round") +
  geom_point(size = 3.5, stroke = 0) +
  annotate("text", x = 2024, y = 545, label = paste0("+", growth[1], " km\nin 10 years"), family = f2, fontface = "bold", size = 8, color = clr_accent, lineheight = 0.9, hjust = 1) +
  annotate("text", x = 2024, y = 475, label = paste0("+", growth[2], " km"), family = f2, fontface = "bold", size = 8, color = clr_accent2, lineheight = 0.9, hjust = 1) +
  scale_color_manual(values = c("Municipality" = clr_accent, "Urban area" = clr_accent2), name = NULL) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 3), expand = expansion(mult = c(0.1, 0.2))) +
  scale_y_continuous(breaks = seq(450, 550, 50), labels = scales::label_comma(suffix = " km"), expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Uppsala keeps cycling",
    subtitle = "Total cycling network length",
    caption = "Source: Uppsala Miljöbarometern · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2, base_size = 24) +
  theme(
    legend.position = "top",
    legend.location = "plot",
    legend.text = element_text(margin = margin(l = 5, r = 20)),
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
