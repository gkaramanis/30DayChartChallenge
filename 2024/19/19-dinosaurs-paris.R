library(tidyverse)
library(sf)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-19"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

venues <- read_sf("https://data.paris2024.org/api/explore/v2.1/catalog/datasets/paris-2024-sites-de-competition/exports/geojson?lang=en&timezone=Europe%2FParis") %>% 
  distinct(geometry)

nat_hist <- data.frame(y = 48.8443, x = 2.3562) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

paris <- read_sf("https://raw.githubusercontent.com/blackmad/neighborhoods/master/paris.geojson")

# <a href="https://www.flaticon.com/free-icons/dinosaur" title="dinosaur icons">Dinosaur icons created by Freepik - Flaticon</a>
dino <- here::here("2024/data/img/dinosaur.png")

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

ggplot() +
  geom_sf(data = paris, fill = NA, color = "#c2dbda", linewidth = 1) + 
  geom_point(aes(y = 48.8443, x = 2.3562), color = "#f4c28c", size = 18) +
  ggimage::geom_image(aes(y = 48.8443, x = 2.3562, image = dino), color = "black", size = 0.115) +
  geom_sf(data = venues, color = "#f6eab3", size = 5) +
  coord_sf(xlim = c(st_bbox(paris)[1], st_bbox(paris)[3]), ylim = c(st_bbox(paris)[2], st_bbox(paris)[4])) +
  labs(
    title = toupper("Paris 2024 venues"),
    subtitle = toupper("National Museum of\nNatural History"),
    caption = toupper("Source: Paris 2024 Data\nGraphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#7495b3", color = NA),
    plot.title = element_text(hjust = 0.5, size = 33, face = "bold", color = "#f6eab3"),
    plot.subtitle = element_text(hjust = 0.5, size = 34, face = "bold", color = "#f4c28c", margin = margin(5, 0, 50, 0), family = f1b),
    plot.caption = element_text(hjust = 0.5, color = "#d3e4b0", face = "bold", margin = margin(50, 0, 0, 0), size = 14, family = f1b)
  )
  

