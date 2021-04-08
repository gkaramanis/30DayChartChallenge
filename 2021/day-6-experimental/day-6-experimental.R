library(ggplot2)
library(dplyr)
library(rtweet)
library(lubridate)
library(sonify)
library(tuneR)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

tweets_music <- tweets_30 %>% 
  mutate(
    day = day(created_at),
    hour = hour(created_at)
    ) %>% 
  filter(day != 31) %>% 
  distinct(favorite_count, retweet_count, day, hour) %>% 
  arrange(day, hour) %>% 
  complete(day, hour) %>% 
  replace(is.na(.), 0)

r <- sonify(x = tweets_music$day + tweets_music$hour/25, y = tweets_music$retweet_count, interpolation = "constant", duration = max(tweets_music$day) * 2)

f <- sonify(x = tweets_music$day + tweets_music$hour/25, y = tweets_music$favorite_count, interpolation = "constant", duration = max(tweets_music$day) * 2)

writeWave(r, here::here("2021", "day-6-experimental", "r.wav"))
writeWave(f, here::here("2021", "day-6-experimental", "f.wav"))


ggplot() +
  geom_text(aes(0, 0, label = "retweets"), color = "grey20", size = 20, family = "Publico Headline Black Italic") +
  theme_void() +
  theme(plot.background = element_rect(fill = "orange", color = NA)) +
  ggsave(here::here("2021", "day-6-experimental", "r.png"))

ggplot() +
  geom_text(aes(0, 0, label = "favorites"), color = "grey97", size = 20, family = "Publico Headline Black Italic") +
  theme_void() +
  theme(plot.background = element_rect(fill = "purple", color = NA)) +
  ggsave(here::here("2021", "day-6-experimental", "f.png"))
