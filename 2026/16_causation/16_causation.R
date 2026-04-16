library(tidyverse)
library(pxweb)
library(marquee)
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

# New EV + plug-in hybrid registrations, Uppsala municipality, 2010–2024
# Source: SCB Statistikdatabasen, PersBilarDrivMedel
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/

cars_annual <- read_csv(
  here::here("2026/data/scb_nyreg_bilar_drivmedel_Uppsala.csv"),
  show_col_types = FALSE
) |>
  pivot_longer(starts_with("Antal"), names_to = "ym", values_to = "n") |>
  mutate(
    ym    = str_remove(ym, "^Antal "),
    year  = as.integer(str_sub(ym, 1, 4)),
    month = as.integer(str_sub(ym, 6, 7))
  ) |>
  filter(year >= 2010, year <= 2024, drivmedel %in% c("el", "laddhybrid")) |>
  group_by(year) |>
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop")

# Uppsala municipality population, 2010–2024
# Source: SCB, Folkmängd november (BE0101A/FolkmangdNov)

pop_annual <- pxweb_get(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/FolkmangdNov",
  query = list(
    Region = "0380",
    Alder = "tot",
    Kon = c("1", "2"),
    ContentsCode = "BE0101A9",
    Tid = as.character(2010:2024)
  )
) |>
  as.data.frame() |>
  group_by(år) |>
  summarise(pop = sum(Antal), .groups = "drop") |>
  rename(year = år) |>
  mutate(year = as.integer(year))

cars_pc <- cars_annual |>
  left_join(pop_annual, by = "year") |>
  mutate(ev_per_10k = n / pop * 10000)

ggplot(cars_pc, aes(x = year, y = ev_per_10k)) +
  annotate("rect", xmin = 2018, xmax = 2022 + 10/12, ymin = -Inf, ymax = Inf, fill = clr_accent2, alpha = 0.12) +
  annotate("rect", xmin = 2022 + 10/12, xmax = 2024.5, ymin = -Inf, ymax = Inf, fill = clr_accent, alpha = 0.08) +
  geom_marquee(data = data.frame(x = 2020.5, y = 4, label = "**Bonus & malus**  \nrebate for EVs  \n and tax on polluters"), aes(x = x, y = y, label = label), hjust = 0.5, vjust = 0, size = 5, color = clr_text2, family = f2, style = modify_style(classic_style(), "body", align = "center"), inherit.aes = FALSE, lineheight = 0.9) +
  geom_marquee(data = data.frame(x = 2023.75, y = 20, label = "**Malus only**  \ntax on  \nhigh-emission cars"), aes(x = x, y = y, label = label), hjust = 0.5, vjust = 0, size = 5, color = clr_text2, family = f2, style = modify_style(classic_style(), "body", align = "center"), inherit.aes = FALSE, lineheight = 0.9) +
  geom_line(color = clr_accent, linewidth = 2, lineend = "round") +
  geom_point(color = clr_accent, size = 4, stroke = 0) +
  annotate("text", x = 2024, y = 80, label = "EVs &\nplug-ins", family = f2, fontface = "bold", size = 8, color = clr_accent, lineheight = 0.9) +
  scale_x_continuous() +
  scale_y_continuous(labels = scales::label_number(suffix = ""), breaks = seq(0, 120, 20)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Bonus-malus effect?",
    subtitle = "New car registrations in Uppsala municipality",
    caption = "Source: SCB · Graphic: Georgios Karamanis",
    x = NULL,
    y = "per 10 000 residents"
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.title.y = element_text(color = clr_text2),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20, b = 6)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(b = 16)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 16)),
    plot.margin = margin(t = 20, r = 60, b = 16, l = 16)
  )

record_polaroid()
