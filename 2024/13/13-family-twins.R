library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-13"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

url <- "https://docs.google.com/spreadsheets/d/1h9YplYoT1rVkmSHgtGJJmSoKTt9Wm1-LRzV7kWlFNNI/edit#gid=0"

twins_triplets <- googlesheets4::sheet_names(url)

tt <- map_df(twins_triplets, function(x) googlesheets4::read_sheet(url, sheet = x) %>% mutate(relationship = x)) %>% 
  janitor::clean_names()

tt_unique <- tt %>% 
  filter(!duplicated(paste0(pmax(athlete, relation), pmin(athlete, relation))))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

tt_labels <- tt_unique %>% 
  count(relationship) %>% 
  mutate(
    label = paste0(n, " pairs of\n", relationship),
    label = fct_reorder(label, n)
    )

pal <-c(
  "#008080",
  "#a4debf",
  "#ef0041",
  "#ffe33d",
  "#ff5bd7"
  )

ggplot(tt_labels) +
  geom_col(aes(y = n, x = as.numeric(label)), fill = pal[2]) +
  geom_text(aes(y = n, x = as.numeric(label), label = label), family = f1b, fontface = "bold", lineheight = 0.9, hjust = c(0, 0, 0, 1, 1), nudge_y = c(2, 2, 2, -2, -2), size = 10, color = c(pal[4], pal[4], pal[4], pal[3], pal[3])) +
  coord_flip() +
  labs(
    title = "TWINS AND TRIPLETS\nIN THE SAME GAMES",
    caption = "SOURCE: OLYMPEDIA · GRAPHIC: GEORGIOS KARAMANIS"
  ) +
  theme_void(base_family = f2b) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[5], color = NA),
    plot.title = element_text(size = 50, hjust = 0.5, family = f1b, color = "white"),
    plot.caption = element_text(size = 11, hjust = 0.5, color = "white", family = f1b),
    plot.margin = margin(30, 0, 30, 0)
  )
  
