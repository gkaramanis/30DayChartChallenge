library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

txt = "In rowing, there are no world records due to the variability that weather conditions can impose on times"

f1b <- "Graphik Compact"
f2 <- "Iosevka Term"

ggplot() +
  geom_text(aes(0, 0, label = 0), size = 320, family = f2) +
  geom_text(aes(-0.2, 0.3, label = "////"), size = 30, angle = 35) +
  geom_text(aes(0.2, -0.35, label = "\\\\\\\\"), size = 30, angle = 35) +
  geom_text(aes(0, 0, label = str_wrap(toupper(txt), 9)), size = 12, family = f1b, fontface = "bold", color = "white", lineheight = 0.9, alpha = 0.95) +
  coord_fixed(clip = "off", ylim = c(-2, 2), xlim = c(-1.1, 1)) +
  labs(
    caption = toupper("Graphic: Georgios Karamanis")
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#00CCFF", color = NA),
    plot.caption = element_text(hjust = 0.5, color = "#00CCFF", family = f1b, margin = margin(0, 0, 10, 0))
  )
  
