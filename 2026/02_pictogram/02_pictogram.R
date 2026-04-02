library(tidyverse)
library(waffle)
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

# https://mobilitysweden.se/statistik/databas-nyregistreringar
# New car registrations, Uppsala län, 2025

new_cars_raw <- read_csv(here::here("2026/data/new car registrations 2025.csv"))

new_cars <- new_cars_raw |> 
  select(make = 1, n = 2) |> 
  mutate(
    n = parse_number(str_replace_all(n, " ", "")),
    label = paste0("**", make, "** ", scales::number(n))
    ) |> 
  mutate(label = fct_reorder(label, n, .desc = TRUE)) |> 
  head(5)

ggplot(new_cars) +
  geom_pictogram(aes(values = n / 10, label = make, color = make), family = "Font Awesome 5 Free Solid", size = 4.8, n_rows = 5) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_label_pictogram(values = c("car-side")) +
  scale_color_manual(values = c(
    "Volkswagen" = clr_accent2,
    "Toyota" = clr_accent,
    "Volvo" = clr_accent,
    "Kia" = "#A0A098",
    "Skoda" = "#A0A098"
  )) +
  facet_wrap(vars(label), ncol = 1) +
  labs(
    title = "New cars in Uppsala County",
    subtitle = "Top 5 makes by number of registrations",
    caption = "Source: Mobility Sweden · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 24) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    strip.text = marquee::element_marquee(hjust = 0, margin = margin(t = 10, b = 2, l = 50), size = 20, family = f2),
    plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5),
    plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(t = 10, b = 30)),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    plot.margin = margin(t = 20, b = 18)
  )

record_polaroid()
