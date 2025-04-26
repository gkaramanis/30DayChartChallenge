library(tidyverse)
library(patchwork)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 11, height = 9.5, dpi = 320)

# https://www.forsakringskassan.se/statistik-och-analys/statistikdatabas#!/sjuk/sjp-pagaende-sjukfall-diagnos-f43
# Andel pågående sjukfall på grund av stress-related diagnoses
sjukfall <- readxl::read_xlsx(here::here("2025/data/SJPPagSjukfallDiagnosF43-counties.xlsx"), skip = 2) %>% 
  janitor::clean_names() %>% 
  rename(
    year = ar,
    month = manad,
    sex = kon,
    county = lan,
    pct = andel_pagaende_sjukfall_percent
  ) %>% 
  mutate(
    county = str_remove_all(county, "\\d+ |s* län"),
    across(year:month, as.numeric),
    sex = if_else(sex == "Kvinnor", "Women", "Men")
    )

f1 <- "Asap Semi Condensed"
f2 <- "Input Serif Compressed"

p <- function(data) {
  ggplot(data, aes(x = month, y = year, fill = pct)) +
    geom_tile() +
    scale_x_continuous(breaks = c(1, 12), label = c("Jan", "Dec")) +
    scale_y_reverse(breaks = seq(2005, 2025, 10)) +
    MetBrewer::scale_fill_met_c("Greek", direction = -1) +
    coord_fixed(clip = "off") +
    labs(
      title = "The human cost of stress: Rising sick leave in Sweden",
      subtitle = str_wrap("Stress-related sick leave continues rising, peaking in 2024 with 43 500 ongoing cases. Women's rates are often more than double men's, with mothers aged 30–39 particularly affected, often linked to high-risk jobs and greater domestic responsibilities. Värmland and Jämtland show particularly high rates among women.", 135),
      caption = "Monthly data from Jan 2005 on ongoing sick leave with stress diagnosis (F43) by sex and county\nSource: Swedish Social Insurance Agency (Försäkringskassan) · Graphic: Georgios Karamanis",
      fill = "Percentage of ongoing sick leave (%)"
    ) +
    facet_grid(vars(sex), vars(county)) +
    theme_void(base_family = f1) +
    theme(
      legend.position = "top",
      legend.key.height = unit(0.6, "line"),
      legend.key.width = unit(2.5, "line"),
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),
      legend.margin = margin(15, 0, 5, 0),
      plot.background = element_rect(fill = "grey99", color = NA),
      axis.text = element_text(family = f2, size = 8, color = "grey30"),
      axis.text.x = element_text(hjust = c(0, 1)),
      axis.text.y = element_text(color = c("grey70", "grey50", "grey30")),
      strip.text.y = element_text(angle = -90),
      strip.text = element_text(face = "bold", margin = margin(0, 0, 1, 0)),
      strip.text.y.right = element_text(size = 12),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 10, hjust = 0, margin = margin(10, 0, 0, 0))
    )
}

p1 <-  p(sjukfall %>% filter(as.numeric(as.factor(county)) <= 11)) +
  labs(caption = NULL) +
  theme(axis.text.x = element_blank())

p2 <-  p(sjukfall %>% filter(as.numeric(as.factor(county)) > 11)) +
  labs(
    title = NULL,
    subtitle = NULL
    ) +
  theme(legend.position = "none")

layout <- "
AAAAAAAAAAAAAAA
BBBBBBBBBBBBBB#
"

p1 / p2 +
  plot_layout(design = layout) &
  theme(
    plot.margin = margin(5, 5, 5, 5)
  )

