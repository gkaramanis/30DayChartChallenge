library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-06/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://oecd.ai/en/incidents/41242
# See google results
# Artificial intelligence is using gallons upon gallons of water. Microsoft alone used more than 2,500 Olympic-sized swimming pools of water in its data centres last year (last year = 2022)
# 2500 olympic-sized pools x 500 000 gallons = 1.25 billion gallons
# or 4.73 billion liters

pools <- expand.grid(x = 1:50, y = 1:50)

f1 <- "Graphik"
f1b <- "Graphik Compact"

ggplot(pools) +
  geom_tile(aes(x, y), fill = "cyan", color = "blue3", linewidth = 0.3, width = 0.4, height = 0.8) +
  annotate("text", 25.5, 25.5, label = str_wrap(toupper("Artificial intelligence is using gallons upon gallons of water. Microsoft alone used more than 2,500 Olympic-sized swimming pools of water in its data centers in 2022."), 20), color = "purple4", hjust = 0.5, family = f1, fontface = "bold", size = 12, lineheight = 0.9) +
  annotate("text", 3, 51.2, label = "1 Olympic-sized swimming pool is about 0.5 million gallons or 1.9 billion liters", hjust = 0, vjust = 0, family = f1b, color = "blue3") +
  annotate("curve", x = 2.5, y = 51.7, xend = 1, yend = 50.8, arrow = arrow(length = unit(0.01, "npc")), color = "blue3", curvature = 0.3) +
  coord_fixed(clip = "off") +
  labs(
    caption = "Source: OECD.AI · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1b) +
  theme(
    plot.background = element_rect(fill = "#DDEB14", color = NA),
    plot.caption = element_text(hjust = 0.5, size = 11, color = "blue4")
  )
  
