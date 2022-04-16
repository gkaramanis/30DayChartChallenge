library(tidyverse)
library(tabulizer)
library(rgeoboundaries)
library(sf)
library(camcorder)
library(ggtext)
library(ggrepel)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 10, height = 8.5, units = "in", dpi = 320)

# Remake of 
# https://www.economist.com/graphic-detail/2018/06/05/malta-leads-the-way-for-gay-and-transgender-rights-in-europe

# Read in 2021 scores
rainbow21 <- extract_tables("https://ilga-europe.org/sites/default/files/Attachments/Rainbow%20Europe%20Index%202021.pdf", area = list(c(230, 1104, 840, 1220)), guess = FALSE) %>% 
  .[[1]] %>% 
  as.data.frame() %>% 
  rename(country = 1, score = 2) %>% 
  mutate(
    score = as.numeric(score),
    country = str_to_title(str_remove(country, "\\*")),
    country_label = case_when(
      country == "Bosnia Herz" ~ "Bosnia and Herzegovina",
      country == "Czechia" ~ "Czech Republic",
      country == "North  Macedonia" ~ "Republic of Macedonia",
      country == "Russia" ~ "Russian Federation",
      country == "Uk" ~ "United Kingdom",
      TRUE ~ country
    )
    )

target_proj <- "+proj=laea +lat_0=17 +lon_0=32"

world_simpl <- gb_adm0() %>% 
  st_transform(crs = target_proj) %>% 
  rmapshaper::ms_simplify() %>% 
  mutate(
    shapeName = case_when(
      shapeName == "None" ~ "Switzerland",
      TRUE ~ shapeName
    )
    )
  
world <- left_join(world_simpl, rainbow21, by = c("shapeName" = "country_label"))

# https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
zoom_to <- c(17, 57)

zoom_level <- 2.8

zoom_to_xy <- st_transform(st_sfc(st_point(zoom_to), crs = 4326), crs = target_proj)

C <- 40075016.686   # ~ circumference of Earth in meters

x_span <- C / 2^zoom_level

y_span <- C / 2^(zoom_level + 0.18)

disp_window <- st_sfc(
  st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
  st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 3))),
  crs = target_proj
)


pal <- rev(c("#2a86ba", "#61acaf", "#90cfa6", "#bcdeab", "#e2ecb5", "#fbeaa4", "#fec480", "#f79b5f", "#ec6344", "#dd262b"))

f1 <- "Fira Sans Compressed"
f2 <- "Charter"

ggplot(world) +
  geom_sf(aes(fill = score), size = 0.4, color = "white") +
  geom_text_repel(aes(label = toupper(country), geometry = geometry), color = "black", family = f1, stat = "sf_coordinates", point.padding = 0.5, segment.size = 0.1, seed = 99) +
  annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, size = 2, color = "#E3120B") +
  annotate("segment", x = -Inf, xend = -Inf, y = Inf, yend = 5.85*10^6, size = 10, color = "#E3120B") +
  scale_fill_stepsn(colors = pal, na.value = "#E0E2DB", n.breaks = 8, show.limits = TRUE, labels = function(x) {paste("———", x)}) +
  coord_sf(xlim = st_coordinates(disp_window)[,'X'],
           ylim = st_coordinates(disp_window)[,'Y'],
           clip = "on", expand = FALSE) +
  labs(
    fill = "<span style='font-size:13pt'>**LGBTI rights,<br>2021**</span><br><span style='font-size:11pt'>100 = full equality</span>",
    title = "Malta still leads the way for gay and transgender\nrights in Europe",
    caption = "Source: ILGA-Europe · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_colorsteps(byrow = TRUE)) +
  theme_minimal() +
  theme(
    legend.position = c(0.935, 0.778),
    legend.background = element_rect(fill = "white", color = "#76848C", size = 0.2),
    legend.margin = margin(5, 5, 12, 5),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1.1, "cm"),
    legend.title = element_markdown(family = f1, margin = margin(0, 0, 10, 0)),
    legend.text = element_text(size = 12, family = f1, margin = margin(0, 0, 0, -5)),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(family = f2, size = 27, face = "bold", hjust = 0, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(family = f1, size = 8, hjust = 0, margin = margin(2, 0, 10, 0), color = "grey50"),
    plot.margin = margin(40, 0, 0, 0)
  )
