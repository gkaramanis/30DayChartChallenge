library(tidyverse)
library(ggtext)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-05/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

athletes <- tribble(
  ~year, ~city, ~para, ~no_para,
  1960, "Rome", 400, 5338,
  1976, "Toronto", 1657, 6084,
  1992, "Barcelona", 3001, 9356,
  2004, "Athens", 3808, 10625,
  2020, "Tokyo", 4403, 11656
)

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

bg_col <- "#D4F662"

ggplot(athletes) +
  # Paralympics
  geom_col(aes(para, as.factor(year)), fill = "#EA337E") +
  geom_text(data = . %>% filter(year == 2020), aes(para, as.factor(year), label = scales::number(para)), color = "white", hjust = 1, size = 10, family = f1, fontface = "bold", nudge_x = -500) +
  # Not Paralympics
  geom_col(aes(-no_para, as.factor(year)), fill = "#059E98") +
  geom_text(data = . %>% filter(year == 2020), aes(-no_para, as.factor(year), label = scales::number(no_para)), color = "white", hjust = 0, size = 10, family = f1, fontface = "bold", nudge_x = 500) +
  # V line
  geom_vline(xintercept = 0, color = bg_col, size = 3) +
  # Games
  geom_text(aes(0, as.factor(year), label = paste0(city, "\n", year)), hjust = 1, family = f2b, nudge_x = -500, size = 10, color = bg_col) +
  scale_y_discrete(limits = rev) +
  labs(
    caption = "NUMBER OF ATHLETES<br>IN THE SUMMER <span style='color:#059E98'>OLYMPICS</span><br>AND <span style='color:#EA337E'>PARALYMPICS</span>"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.caption = element_markdown(size = 40, family = f1b, face = "bold", hjust = 0.5)
  )
