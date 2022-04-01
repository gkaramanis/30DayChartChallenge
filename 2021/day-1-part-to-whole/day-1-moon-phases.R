library(dplyr)
library(stringr)
library(gggibbous)

phases <- tibble(
  ratio = c(seq(0, 1, 0.25), 0.75, 0.5, 0.25),
  a = seq(0, 315, 45),
  x = cos(a * pi / 180),
  y = sin(a * pi / 180),
  right = rep(c(TRUE, FALSE), each = 4),
  phase = c("new", "waxing crescent", "first quarter", "waxing gibbous", "full", "waning gibbous", "third quarter", "waning crescent")
)

ggplot(phases) +
  geom_moon(aes(x, y, ratio = 1), fill = "grey10", size = 30) +
  geom_moon(aes(x, y, ratio = ratio, right = right), fill = "grey85", color = NA, size = 29.5) +
  geom_text(aes(x * 1.6, y * 1.6, label = str_to_title(phase)), colour = "grey98", family = "Publico Headline Bold") +
  annotate("text", 0, 0, label = "Phases of\nthe Moon", color = "grey97", family = "Publico Headline Bold", size = 9) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA),
    plot.margin = margin(20, 20, 20, 20)
    ) 

ggsave(here::here("2021", "day-1-part-to-whole", "day-1-moon-phases.png"), dpi = 320, height = 7, width = 7)
