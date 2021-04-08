library(tidyverse)
library(lubridate)
library(rtweet)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

phys <- tweets_30 %>% 
  mutate(day = day(created_at)) %>% 
  filter(day != 31 & is_retweet == FALSE) %>% 
  select(status_id, day, hashtags) %>% 
  unnest(hashtags) %>% 
  group_by(status_id) %>% 
  mutate(
    crosspost = sum(if_else(
      tolower(hashtags) == "30daychartchallenge" | tolower(hashtags) == "tidytuesday" , 1, 0))
  ) %>% 
  ungroup() %>% 
  filter(crosspost == 2) %>% 
  distinct(status_id, day) %>% 
  count(day)
