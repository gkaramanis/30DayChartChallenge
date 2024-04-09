library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

timeline <- tribble(
  ~year, ~event,
  1940, "Born in Saint Bethlehem, Tennessee, US",
  1944, "Diagnosed with polio",
  1952, "Regained ability to walk by age 12",
  1956, "Wins the bronze medal in the 4x100 meter relay at the Olympics",
  1960, "Becomes the first American woman to win three gold medals at the Olympics"
)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

ggplot(timeline, aes(0, year, label = str_wrap(event, 26))) +
  geom_point(size = 6) +
  geom_path(linewidth = 1.5) +
  geom_text(aes(label = year), hjust = 1, nudge_x = -1, size = 10, family = f1b, fontface = "bold") +
  geom_text(hjust = 0, nudge_x = 1, lineheight = 0.9, family = f1, size = 5) +
  annotate("text", 3.5, 1935, label = "Wilma Rudolph", size = 12, family = f2b) +
  annotate("text", 3.5, 1965, label = "Graphic: Georgios Karamanis", family = f1b) +
  scale_x_continuous(limits = c(-1, 9)) +
  scale_y_reverse(limits = c(1970, 1930)) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )
  
  
  