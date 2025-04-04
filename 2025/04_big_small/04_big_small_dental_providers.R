library(tidyverse)
library(geofacet)
library(ggtext)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 5, height = 8.9, dpi = 320, bg = "grey99")

dental_provider <- readxl::read_xlsx(here::here("2025/data/2024-5-9102-tables.xlsx"), sheet = "Tabell 4 A–C", skip = 26, n_max = 22) %>% 
  select(county = 1, region = 6, private = 7) %>% 
  pivot_longer(region:private, names_to = "type")

f1 <- "Signika"
f2 <- "DIN Condensed"

ggplot(dental_provider, aes(x = 0, y = value, fill = type)) +
  geom_col(position = "fill", color = "white") +
  geom_text(data = . %>% filter(type == "private"), aes(label = paste0(round(value), "%")), position = position_fill(vjust = 0.6), size = 3.5, family = f2, color = "white") +
  scale_fill_manual(values = c("private" = "red3", region = "grey88")) +
  coord_radial(expand = FALSE, theta = "y") +
  facet_geo(vars(county), grid = "se_counties_grid1") +
  labs(
    title = "BIG PRIVATE vs small public dental care",
    subtitle = "In 2023, <span style='color:red3'>**private dental providers**</span> had the biggest share<br>of the market in every Swedish county",
    caption = "Source: Socialstyrelsen · Graphic: Georgios Karamanis",
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.spacing.x = unit(2, "lines"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_markdown(margin = margin(5, 0, 20, 0), size = 13, hjust = 0.5, lineheight = 1.1),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), color = "grey40", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  )
  
record_polaroid()
