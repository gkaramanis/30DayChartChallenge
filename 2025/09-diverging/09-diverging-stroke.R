library(tidyverse)
library(ggfx)
library(ggmagnify)
library(marquee)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

# Table 1. Persons with at least one incident case of stroke by age and sex, 1987−2023
stroke_raw <- readxl::read_xlsx(here::here("2025/data/downloaded/2024-11-9295-tables.xlsx"), sheet = "Tabell 1", skip = 3, n_max = 39) %>% 
  janitor::clean_names()

stroke <- stroke_raw %>% 
  fill(alder, .direction = "down") %>% 
  select(alder, kon, x2003, x2023) %>% 
  pivot_longer(x2003:x2023, names_to = "year", values_to = "n") %>% 
  mutate(
    year = parse_number(year),
    n = if_else(kon == "Kvinnor", n, -n)
    ) %>% 
  filter(kon != "Totalt")

lab_f <- function(x) {
  abs(x) %>% 
    scales::number()
}

f1 <- "Outfit"

p <- ggplot(stroke) +
  as_reference(
    geom_bar(data = . %>% filter(year == 2003), aes(n, alder, fill = kon), stat = "identity", orientation = "y", alpha = 0.4, width = 0.91),
    id = "2003"
  ) +
  with_blend(
    geom_bar(data = . %>% filter(year == 2023), aes(n, alder, fill = kon, color = after_scale(colorspace::darken(fill, 0.5))), stat = "identity", orientation = "y", alpha = 0.7, linewidth = 0.5),
    bg_layer = "2003",
    blend_type = 'multiply'
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = lab_f, limits = c(-4500, 4500), breaks = seq(-4000, 4000, 1000)) +
  scale_fill_manual(values = c("#9d5987", "#dd822f")) +
  labs(
    title = "Stroke cases drop overall in Sweden, but rising among younger people",
    subtitle = "Persons with at least one new case of stroke, 2003-2023. Data from 2003 {.grey60 **is shown with lower transparency.**} There is a decrease in the older age groups, but a concerning rise among 30-34 year olds.",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
  ) +
  theme_minimal(base_size = 14, base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 22, face = "bold", margin = margin(0, 0, 0, 0)),
    plot.subtitle = marquee::element_marquee(width = 1, size = 15, margin = margin(3, 0, 20, 0), family = f1),
    plot.caption = element_text(size = 10, margin = margin(20, 0, 0, 0), color = "grey40"),
    plot.margin = margin(10, 10, 10, 10)
  )

p + 
  geom_magnify(from = c(xmin = -120, xmax = 100, ymin = 10.55, ymax = 11.45), to = c(xmax = -2000, xmin = -3500, ymin = 8.7, ymax = 12.7)) +
  # Men 30-34
  annotate(GeomMarquee, x = -3900, y = 7.8, label = "Cases among {.#dd822f **men**} aged 30-34 more than doubled since 2003", family = f1, size = 5, hjust = 0, vjust = 1, lineheight = 0.95, width = 0.25) +
  annotate("segment", x = -3050, xend = -3050, y = 8, yend = 9) +
  annotate("point", x = -3050, y = 9, size = 2) +
  # Women 80+
  annotate(GeomMarquee, x = 1400, y = 5.4, label = "Cases among {.#9d5987 **women**} aged 80+ halved", family = f1, size = 5, hjust = 0, vjust = 1, lineheight = 0.95, width = 0.25) +
  annotate("segment", x = 2000, xend = 2000, y = 4.1, yend = 1) +
  annotate("point", x = 2000, y = 1, size = 2)

