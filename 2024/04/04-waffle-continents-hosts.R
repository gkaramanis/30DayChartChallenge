library(tidyverse)
library(waffle)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-04/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

hosts <- read_csv(here::here("2024/data/olympic_hosts.csv")) %>% 
  filter(game_season == "Summer")

hosts_wf <- hosts %>% 
  mutate(
    game_location = if_else(game_location == "Australia, Sweden", "Australia", game_location),
    continent = countrycode::countrycode(game_location, origin = "country.name.en", destination = "continent")
    ) %>% 
  count(continent) %>% 
  add_row(continent = "Africa", n = 0.1) %>% 
  mutate(continent = fct_reorder(continent, -n))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

bg_col <- "#FF60A8"
txt_col <- "#D4F662"

ggplot(hosts_wf) +
  geom_waffle(aes(values = n, fill = continent), n_rows = 4, flip = TRUE, color = bg_col, size = 2) +
  geom_text(aes(0.5, 0, label = continent), hjust = 0, family = f1, fontface = "bold", size = 10, color = txt_col) +
  coord_fixed(clip = "off") +
  MetBrewer::scale_fill_met_d("Egypt", direction = -1) +
  facet_wrap(vars(continent), nrow = 2) +
  labs(
    title = str_wrap(toupper("How many times has each continent hosted the Olympics?"), 20),
    caption = "Source: Olympics.com · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2b) +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing.y = unit(0, "cm"),
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.title = element_text(family = f2b, hjust = 0.5, size = 34, margin = margin(20, 0, 60, 0), color = txt_col),
    plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(50, 0, 10, 0), color = txt_col, family = f2b),
    plot.margin = margin(0, 20, 0, 20)
  )
  
