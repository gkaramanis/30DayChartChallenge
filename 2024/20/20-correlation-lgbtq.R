library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-20/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_LGBTI_Olympians_and_Paralympians"

lgbtqi_raw <- read_html(url) %>% 
  html_table() %>% 
  .[[4]] %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names()

lgbtqi <- lgbtqi_raw %>% 
  mutate(
    across(f:total, parse_number),
    year = parse_number(games)
    ) %>% 
  pivot_longer(f:nb, names_to = "gender", values_to = "n")

pal <- c("#AC85E9", "#FF6C9D", "#FFA3C8", "#FDE164", "#01E1F2")

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

lgbtqi %>% 
  filter(str_detect(games, "Summer")) %>% 
  filter(year > 1970) %>% 
  ggplot() +
  geom_col(aes(year, n, fill = gender, color = gender), width = 4) +
  scale_color_manual(values = pal, labels = c("Women", "Men", "Nonbinary")) +
  scale_fill_manual(values = pal, labels = c("Women", "Men", "Nonbinary")) +
  scale_x_continuous(breaks = seq(1972, 2020, 12)) +
  scale_y_continuous(limits = c(0, 300), position = "right") +
  labs(
    title = toupper("LGBTI Olympians"),
    caption = toupper("Source: Wikipedia · Graphic: Georgios Karamanis"),
  ) +
  theme_void(base_family = f2b, base_size = 16) +
  theme(
    legend.position = c(0.45, 0.95),
    legend.title = element_blank(),
    legend.text.position = "left",
    legend.text = element_text(size = 20),
    plot.background = element_rect(fill = pal[4], color = NA),
    axis.text = element_text(size = 20, face = "bold", family = f1b, color = pal1[5]),
    axis.text.y = element_text(size = 12, hjust = 0),
    plot.title = element_text(hjust = 0.5, size = 40, margin = margin(40, 0, 40, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(40, 0, 40, 0)),
    plot.margin = margin(0, 10, 0, 10)
  )
