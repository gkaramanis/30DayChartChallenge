library(tidyverse)
library(lubridate)
library(wesanderson)

tweets_w1_raw <- read_csv("2021/data/tweets_w1.csv")

tweets <- tweets_w1_raw %>% 
  mutate(
    h = hour(created_at),
    m = minute(created_at)
    # c = if_else(is_retweet, "green", "purple")
    ) %>% 
  count(h, m, source)

cn = 1.5
pal <- wes_palette("GrandBudapest2", n = 40, type = "continuous")

ggplot(tweets) +
  geom_tile(aes(m, h, width = n/cn, height = n/cn, fill = factor(source)), alpha = 0.25) +
  scale_x_continuous(breaks = seq(0, 60, by = 10), minor_breaks = 0:60) +
  scale_y_reverse(limits = c(23, 0), breaks = seq(20, 0, by = -4), minor_breaks = 0:23) +
  scale_fill_manual(values = rev(pal), na.value = "grey50") +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Distribution of tweets tagged with #30DayChartChallenge\nby hour of day and minute of hour (days 1 to 7)",
    caption = "data: Twitter · graphic: Georgios Karamanis",
    fill = "Twitter client",
    x = "minute",
    y = "hour"
  ) +
  theme_minimal(base_family = "Fira Sans Compressed") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9, family = "Fira Sans Compressed Light"),
    legend.key.size = unit(0.75, "line"),
    legend.title = element_text(angle = 90, hjust = 0.5, family = "Fira Sans Light"),
    axis.title = element_text(family = "Fira Sans Light"),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.caption = element_text(size = 10, family = "Fira Sans Compressed Light", color = "grey40"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  ggsave(here::here("2021/day-10-abstract/day-10-abstract.png"), dpi = 320, width = 9, height = 6)

