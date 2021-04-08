library(rtweet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggbump)
library(ggtext)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

tweets_hour <- tweets_30 %>% 
  select(created_at) %>% 
  mutate(
    hour = hour(created_at),
    date = date(created_at),
    label = stamp("March 1")(date),
    is_today = if_else(date == date(Sys.Date()), TRUE, FALSE)
  ) %>% 
  filter(date > as.Date("2021-03-31")) %>% 
  group_by(date, hour) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(max_hour = max(hour)) %>% 
  ungroup()

ggplot(tweets_hour) +
  geom_bump(aes(hour, n, group = date,
                # color = if_else(is_today, "purple", "grey50"),
                color = factor(date),
                size = if_else(is_today, 1.25, 0.25)
                ), lineend = "round", smooth = 7) +
  geom_text(aes(max_hour, n, label = if_else(max_hour == hour, label, NULL), color = factor(date)), stat = "unique", hjust = 0, nudge_x = 0.5, family = "Futura Medium", size = 2) +
  scale_size_identity() +
  scale_color_manual(values = c("grey80", "grey40", "purple")) +
  labs(
    title = "Hourly tweets tagged with #30DayChartChallenge<br><span style='color:purple'>**Today**</span> vs <span style='color:darkgrey'>previous days</span>",
    x = "hour of day",
    y = NULL,
    caption = "Source: Twitter · Graphic: Georgios Karamanis"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Futura Medium") +
  theme(
    legend.position = "none",
    plot.title = element_markdown(hjust = 0.5, lineheight = 1.2),
    plot.caption = element_text(color = "grey50", size = 6),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(color = "grey50"),
    plot.margin = margin(10, 40, 10, 20)
  ) +
  ggsave(here::here("2021/day-3-historical/day-3-historical.png"), dpi = 320, width = 6, height = 4)
    
