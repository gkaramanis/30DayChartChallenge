library(rtweet)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

tweets_w2 <- tweets_30 %>% 
  mutate(day = day(created_at)) %>% 
  filter(day > 7 & day < 15)

write_as_csv(tweets_w2, "tweets_w2.csv")
