library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-02/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://www.paris2024.org/en/olympic-sports/

# The IOC is keen to set a new standard for inclusive, gender-balanced and youth-centred games. 

# Paris 2024 submitted its proposal to the IOC to integrate four new sports that are closely associated with youth and reward creativity and athletic performance. These sports are breaking, sport climbing, skateboarding, surfing

# All four are easy to take up and participants form communities that are very active on social media.

# 41 sports + 4 new


sports <- expand.grid(x = 1:9, y = 5:1) %>% 
  mutate(color = if_else(row_number() > 41, "#4deeea", "#ffe700"))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

library(ggtext)

ggplot(sports) +
  geom_point(aes(x, y, color = I(color)), size = 20, ) +
  annotate("richtext", 5, 8, label = "<span style='color:#4deeea'>FOUR*</span> NEW SPORTS<br>ARE INTRODUCED<br>IN PARIS 2024", family = f1b, fontface = "bold", color = "#ffe700", size = 18, lineheight = 0.8, fill = NA, label.size = 0) +
  annotate("text", 5, 6.3, label = toupper("*breaking, sport climbing, skateboarding & surfing"), family = f1b, fontface = "bold", color = "#4deeea", size = 4.5, lineheight = 0.8) +
  annotate("text", 5, -0.5, label = "Source: IOC · Graphic: Georgios Karamanis", family = f2b, color = "#ffe700") +
  coord_fixed(clip = "off", xlim = c(0.5, 9.5), ylim = c(-1, 10)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f000ff", color = NA)
  )
  
  
