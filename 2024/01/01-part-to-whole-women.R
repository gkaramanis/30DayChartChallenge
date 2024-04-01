library(tidyverse)
library(ggforce)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-01/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://stillmed.olympics.com/media/Documents/Olympic-Movement/Factsheets/Women-in-the-Olympic-Movement.pdf

r <- 1.3

# 1900, 2.2
# 1960, 11.4
# 2020, 47.8
# 1932, 9
# 1992, 28.8

women <- tribble(
  ~year, ~pct, ~x0, ~y0, ~col,
  1900, 2.2, 2*r - r/2, 3*r, "#0081C8",
  1960, 11.4, 4*r, 3*r, "black",
  2020, 47.8, 6*r + r/2, 3*r, "#EE334E",
  1932, 9, 3*r - r/4, 2*r, "#FCB131",
  1992, 28.8, 5*r + r/4, 2*r, "#00A651"
)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

ggplot(women) +
  # geom_circle(aes(x0 = x0, y0 = y0, r = 1.3, colour = col), linewidth = 8) +
  geom_vline(aes(xintercept = x0), alpha = 0.1, linetype = "dashed") +
  geom_arc_bar(aes(x0 = x0, y0 = y0, r = 1.5, r0 = 1.1, start = 0, end = 2 * pi), fill = "grey99", color = NA) +
  geom_arc_bar(aes(x0 = x0, y0 = y0, r = 1.5, r0 = 1.1, fill = col, start = 0, end = pct * 2 * pi / 100), color = NA) +
  geom_text(aes(x0, y0 + if_else(year %in% c(1932, 1992), -2.5, 2.5), label = paste0(year, "\n", pct, "%"), color = col), lineheight = 0.9, family = f1, size = 9, fontface = "bold") +
  annotate("text", 5.25, 9, label = "SHARE OF WOMEN\nPARTICIPANTS", family = f2b, color = "purple4", size = 14, lineheight = 0.8) +
  annotate("text", 5.25, -2, label = "Source: IOC · Graphic: Georgios Karamanis", family = f2, color = "purple4") +
  scale_color_identity() +
  scale_fill_identity() +
  coord_fixed(xlim = c(0.5, 10), ylim = c(-2, 10)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#C9E4DE", color = NA)
  )
  
