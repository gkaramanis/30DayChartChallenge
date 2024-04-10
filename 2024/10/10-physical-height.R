library(tidyverse)
library(ggtext)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-10/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

wh <- olympics %>% 
  group_by(season, year, sex) %>% 
  summarise(
    h = mean(height, na.rm = TRUE),
    w = mean(weight, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(year, season, sex) %>% 
  filter(season == "Summer") %>% 
  group_by(sex) %>% 
  mutate(
    dh = round(h - first(na.omit(h)), 1),
    dw = round(w - first(na.omit(w)), 1)
  ) %>% 
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

pal <- c(
  "#FF5C77",
  "#4DD091",
  "#FFEC59",
  "black"
  )


ggplot(wh, aes(year, dh, color = sex)) +
  geom_line(linewidth = 2) +
  geom_point(size = 5) +
  geom_text(data = . %>% group_by(sex) %>% filter(dh == 0), aes(x = year, y = -1, label = year), family = f1, fontface = "bold", size = 5, color = c(pal[1], pal[2])) +
  geom_text(data = . %>% group_by(sex) %>% filter(year == max(year)), aes(x = year, y = -1, label = year), family = f1, fontface = "bold", size = 5, stat = "unique", color = pal[4]) +
  scale_y_continuous(breaks = seq(0, 9, 3), position = "right") +
  scale_color_manual(values = c(pal[2], pal[1])) +
  coord_fixed(ratio = 11) +
  labs(
    title = toupper("Olympic athletes\nare getting taller*"),
    subtitle = "<span style='color:#43A273'>WOMEN</span> HAVE GAINED 7.6 CM AND <span style='color:#FF5C77'>MEN</span> 8.8 CM ON AVERAGE<br>SINCE THEY STARTED COMPETING. *THE INCREASE ALIGNS<br>WITH THE RISE IN GLOBAL AVERAGE HEIGHT",
    caption = "Source: Kaggle · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2b) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[3], color = NA),
    plot.title = element_text(size = 34, hjust = 0.5),
    plot.subtitle = element_markdown(size = 13, hjust = 0.5, lineheight = 1.2),
    plot.caption = element_text(size = 11, hjust = 0.5, family = f1b, face = "bold"),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    plot.margin = margin(0, 20, 0, 20)
  )
  
