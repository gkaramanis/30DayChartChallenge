library(ggplot2)
library(dplyr)
library(rtweet)
library(lubridate)
library(tidyr)
library(wesanderson)
library(colorspace)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

tweets_hashtags <- tweets_30 %>% 
  filter(date(created_at) != "2021-03-31") %>% 
  mutate(day = day(created_at)) %>% 
  select(day, hashtags) %>%
  unnest(hashtags) %>% 
  mutate(hashtags = tolower(hashtags)) %>% 
  count(day, hashtags) %>% 
  filter(n > 4) %>% 
  filter(hashtags != "30daychartchallenge" & !is.na(hashtags)) %>% 
  filter(day == 1 | day == max(day) - 1) %>% 
  add_count(hashtags) %>% 
  arrange(nn) 

pal <- wes_palette("Royal2")

ggplot(tweets_hashtags, aes(x = day, y = n, group = hashtags)) +
  # vertical lines
  annotate("segment", x = 1, xend = 1, y = 0, yend = 80, size = 2, color = pal[5]) +
  annotate("segment", x = max(tweets_hashtags$day), xend = max(tweets_hashtags$day), y = 0, yend = 80, size = 2, color = pal[5]) +
  # "y axis" text
  annotate("text", x = 2.5, y = seq(0, 80, by = 10), label = seq(0, 80, by = 10), family = "JetBrains Mono", size = 3, color = pal[5]) +
  # day labels
  annotate("text", x = c(0.75, 4.25), y = 65, label = c("day 1", "day 4"), angle = c(90, -90), family = "JetBrains Mono", color = pal[5]) +
  # slope lines
  geom_line(size = 1, color = pal[1]) +
  geom_text(aes(
    x = day + if_else(day == 1, -0.3, 0.3),
    label = hashtags,
    color = if_else(nn == 2, darken(pal[1]), darken(pal[4])),
    size = if_else(nn == 2, 4.5, 3),
    hjust = if_else(day == 1, 1, 0)
    ), family = "Graphik Medium") +
  geom_point(aes(
    color = if_else(nn == 2, pal[1], pal[4]),
    size = if_else(nn == 2, 4, 2),
    ), shape = 21, fill = "grey97", stroke = 2) +
  scale_color_identity() +
  scale_size_identity() +
  coord_cartesian(clip = "off") +
  xlim(-0.5, 5.5) +
  labs(
    title = "Hashtags used on day 1 and day 4\nof the #30DayChartChallenge",
    subtitle = "showing hashtags used 5 or more times",
    caption = "data: Twitter · graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(15, 20, 10, 20),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.5, color = pal[3], family = "Futura Condensed ExtraBold", size = 18, lineheight = 1, margin = margin(0, 0, 3, 0)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(0, 0, 5, 0), color = pal[5], family = "Futura Bold", size = 11),
    plot.caption = element_text(hjust = 0.5, color = pal[3], family = "Futura Condensed Medium", size = 8)
  ) +
  ggsave(here::here("2021/day-5-slope/day-5-slope.png"), dpi = 320, height = 7, width = 5)
