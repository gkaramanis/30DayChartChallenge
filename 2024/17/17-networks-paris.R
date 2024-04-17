library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-17/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://overpass-turbo.eu/?Q=%5Bout%3Ajson%5D%5Btimeout%3A60%5D%3B%0A%2F%2FFrance+métrop+%3A+1403916%0Arel%281403916%29%3B%0Amap_to_area+-%3E+.searchArea%3B%0A%28%0Arelation%5B%22route%22%3D%22subway%22%5D%5B%22network%22%3D%22Métro+de+Paris%22%5D%28area.searchArea%29%3B%0A%29%3B%0Aout+body%3B%0A%0A%7B%7Bstyle%3A%0A%0A++relation%5Broute%3Dsubway%5D%0A++++%7B+color%3Aeval%28%22tag%28%27colour%27%29%22%29%3B+fill-color%3Ared%3B+%7D%0A%0A%7D%7D%0A%0A%0A%3E%3B%0Aout+skel+qt%3B&R=

metro <- read_sf(here::here("2024/data/paris/paris-metro.geojson")) %>% 
  # Filter out metro stations
  filter(st_geometry_type(.) != "POINT")

venues <- read_sf("https://data.paris2024.org/api/explore/v2.1/catalog/datasets/paris-2024-sites-de-competition/exports/geojson?lang=en&timezone=Europe%2FParis")

f2 <- "Produkt"

ggplot(metro) +
  geom_sf(data = . %>% filter(!is.na(colour)), aes(color = colour), linewidth = 1.5, lineend = "round") +
  geom_sf(data = venues, size = 3, shape = 21, stroke = 2, color = "black", fill = "#ffe33d") +
  scale_color_identity() +
  coord_sf(xlim = c(st_bbox(metro)[1], st_bbox(metro)[3]), ylim = c(st_bbox(metro)[2] + 0.05, st_bbox(metro)[4])) +
  labs(
    title = toupper("Métro de Paris"),
    subtitle = toupper("& Sites de compétition"),
    caption = toupper("Source: OpenStreetMap & Paris 2024 Data · Graphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f2) +
  theme(
    plot.background = element_rect(fill = "#ffe33d", color = NA),
    plot.title = element_text(size = 40, hjust = 0.5, margin = margin(40, 0, 0, 0)),
    plot.subtitle = element_text(size = 24, hjust = 0.5, margin = margin(7, 0, 0, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 20, 0))
  )
  


