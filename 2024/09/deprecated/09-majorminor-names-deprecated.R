library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

athletes <- read_csv(here::here("2024/data/olympedia/Olympic_Athlete_Bio.csv"))

mm_athletes <- athletes %>% 
  mutate(
    yob = year(as.Date(born)),
    mm = case_when(
      str_detect(tolower(name), "\\b(major)\\b") ~ "major",
      str_detect(tolower(name), "\\b(minor)\\b") ~ "minor"
    )) %>% 
  filter(!is.na(mm))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

pal <- c("#00FFFF", "#FFFF00", "#800080")

ggplot(mm_athletes) +
  geom_text(aes(mm, yob, label = paste0(name, " (", country_noc, ")"), hjust = if_else(mm == "major", 1, 0), color = mm), family = f1b, size = 7) +
  # geom_tile(aes(1.5, yob, fill = mm), height = 0.5, width = 0.9) +
  geom_text(aes(1.5, yob, label = yob, color = mm), family = f1, size = 10, fontface = "bold") +
  scale_color_manual(values = pal) +
  scale_x_discrete(expand = expansion(mult = c(3, 3))) +
  scale_y_reverse() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[3], color = NA)
  )
  
