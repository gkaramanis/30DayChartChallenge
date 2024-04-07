library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-07/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://www.sciencedirect.com/science/article/pii/S2666337624000039

injuries <- tribble(
  ~sport, ~n, ~ir,
  "Track Cycling", 6, 30.8,
  "Soccer", 17, 27.6,
  "Boxing", 6, 20.8
  ) %>% 
  mutate(sport = fct_inorder(sport))

f1b <- "Graphik Compact"
f2b <- "Produkt Medium"

pal <- c("#FF398C", "#804DFF", "#1FC0FF", "#553355")

ggplot(injuries, aes(sport, ir / 10, label = ir/10, fill = sport)) +
  geom_col() +
  geom_text(nudge_y = -0.15, color = "white", family = f1b, fontface = "bold", size = 10) +
  geom_text(aes(y = 0.1, label = sport), color = "white", family = f1b, fontface = "bold", size = 7.5) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Sports with the most injuries\nin Tokyo 2020",
    subtitle = "Injuries per 100 athletes",
    caption = "Source: Sakanashi et al. (2024) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2b) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[4], color = NA),
    plot.title = element_text(hjust = 0.5, size = 32, color = "white"),
    plot.subtitle = element_text(hjust = 0.5, size = 18, color = "white"),
    plot.caption = element_text(hjust = 0.5, size = 11, color = "white"),
    plot.margin = margin(10, 0, 10, 0)
  )
  
