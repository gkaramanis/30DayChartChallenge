library(tidyverse)
library(lubridate)
library(wesanderson)
library(cowplot)

tweets_w1_raw <- read_csv("2021/data/tweets_w1.csv")

tweet_stats <- tweets_w1_raw %>% 
  mutate(
    day = day(created_at),
    hour = as.numeric(hour(created_at)),
    dn = if_else(between(hour, 6, 17), "day", "night"),
    hour_dn = if_else(hour > 11, hour - 12, hour)
  ) %>% 
  filter(day != 31) %>% 
  # filter(!is_retweet) %>% 
  add_count(day, hour)
  
pal <- wes_palette("Zissou1")


f1 = "Fira Sans"
f1cb = "Fira Sans Condensed Bold"
f1c = "Fira Sans Condensed"
f2 = "JetBrains Mono"
f3 = "Humor Sans"

p1 <- ggplot(tweet_stats, aes(x = hour_dn, y = day, fill = n)) +
  geom_tile(stat = "unique", color = NA) +
  # hours
  geom_text(aes(y = 11, label = hour), stat = "unique", family = f3, size = 4.5, color = "#b57e65") +
  # arrows
  annotate("segment", x = 5.5, y = 0, xend = 5.5, yend = 8,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.75, color = "grey30") +
  annotate("segment", x = 6.3, y = 8.6, xend = 7.1, yend = 8.6,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.75, color = "grey30") +
  # arrows "labels"
  annotate("text", x = 5.25, y = 4, label = "days", angle = -73, family = f1, size = 2.5) +
  annotate("text", x = 6, y = 8.6, label = "hours", angle = -0, family = f1, size = 2.5) +
  # day-night
  scale_x_continuous(breaks = 0:11) +
  scale_y_continuous(limits = c(-4, 11)) +
  scale_fill_stepsn(colors = pal) +
  coord_polar(start = -pi/12, clip = "off") +
  facet_wrap(vars(dn)) +
  labs(
    title = "Number of tweets and retweers tagged\nwith #30DayChartChallenge",
    subtitle = "by day and hour of day (UTC), days 1 to 7",
    caption = "data: Twitter · graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = "Fira Sans") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.height = unit(0.75, "line"),
    strip.text = element_blank(),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, size = 20, family = f1cb, color = "#9865b5"),
    plot.subtitle = element_text(hjust = 0.5, family = f1c, color = "#b57e65", margin = margin(10, 0, 0, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), family = f1c, color = "#b57e65"),
    plot.margin = margin(20, 10, 20, 10)
  )

ggdraw(p1) +
  draw_image(here::here("2021", "day-11-circular", "icons", "sun.png"), x = -0.243, y = -0.005, scale = 0.055) +
  draw_image(here::here("2021", "day-11-circular", "icons/moon.png"), x = 0.245, y = -0.005, scale = 0.05) +
  ggsave(here::here("2021/day-11-circular/day-11-circular.png"), dpi = 320, width = 7, height = 5.5)

