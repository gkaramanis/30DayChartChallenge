library(ggplot2)
library(dplyr)
library(cowplot)
library(rtweet)
library(ggimage)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

retweets <- tweets_30 %>% 
  group_by(is_retweet) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

f1b <- "Canela Deck Bold"
f2 <- "Baskerville"

card <- ggplot() +
  # bg
  geom_tile(aes(x = 0, y = 0, width = 1, height = 1.4), fill = "#90733B") +
  # bg 2
  geom_tile(aes(x = 0, y = 0.01, width = 0.95, height = 1.33), fill = "white", color = "grey10", size = 0.7) +
  # 1
  geom_tile(aes(x = 0, y = 0.62, width = 0.92, height = 0.08), fill = "#F7F4EB", color = "grey10", size = 0.9) +
  # 2
  geom_tile(aes(x = 0, y = 0.232, width = 0.92, height = 0.67), fill = "#F2D09D", color = "grey10", size = 0.7) +
  # 3
  geom_tile(aes(x = 0, y = -0.157, width = 0.92, height = 0.08), fill = "#F7F4EB", color = "grey10", size = 0.9) +
  # 4
  geom_tile(aes(x = 0, y = -0.425, width = 0.92, height = 0.43), fill = "#F0F1EA", color = "grey10", size = 0.7) +
  # name
  annotate("text", x = -0.44, y = 0.62, label = "#30DayChartChallenge", hjust = 0, family = f1b, size = 4.5) +
  # type line
  annotate("text", x = -0.44, y = -0.157, label = "Tweets vs Retweets", hjust = 0, family = f1b, size = 4.5) +
  # text
  annotate("text", x = -0.43, y = -0.3, label = paste0(round(retweets$freq[2] * 100),  "% of all tweets tagged with\n#30DayChartChallenge are retweets"), hjust = 0, family = f2, size = 4, lineheight = 0.9) +
  annotate("text", x = -0.43, y = -0.425, label = paste0(retweets$n[1], " tweets and ", retweets$n[2], " retweets\nbetween 2021-04-01 and ", Sys.Date()), hjust = 0, family = f2, size = 4, lineheight = 0.9) +
  annotate("text", x = -0.43, y = -0.55, label = "Source: Twitter\nGraphic: Georgios Karamanis", hjust = 0, family = f2, size = 4, lineheight = 0.9) +
  # symbols
  annotate("point", x = 0.413, y = 0.62, shape = 21, size = 7, fill = "pink") +
  geom_image(data = NULL, aes(x = 0.415, y = 0.62, image = here::here("2021", "day-4-magical", "icons", "magic.png")), asp = 0.8, size = 0.03) +
  geom_image(data = NULL, aes(x = 0.36, y = -0.157, image = here::here("2021", "day-4-magical", "icons", "twitter.png")), asp = 0.8, size = 0.045) +
  geom_image(data = NULL, aes(x = 0.42, y = -0.157, image = here::here("2021", "day-4-magical", "icons", "retweet.png")), asp = 0.8, size = 0.045) +
  coord_fixed(expand = FALSE) +
  theme_void() 

p <- ggplot(retweets) +
  annotate("linerange", x = seq(0, 1, by = 0.25), ymin = -0.75, ymax = 0.75, color = "#5F5E6C") +
  geom_bar(aes(x = n, y = 0, fill = factor(-n)), position = "fill", stat = "identity", orientation = "y", color = "grey20") +
  scale_fill_manual(values = c("purple", "#7DC4EC")) +
  ylim(-3, 2) +
  coord_polar() +
  theme_void() +
  theme(legend.position = "none")

ggdraw() +
  draw_plot(card) +
  draw_plot(p, y = 0.16, scale = 1) 

ggsave(here::here("2021/day-4-magical/day-4-magical.png"), dpi = 320, width = 4, height = 5.6)

