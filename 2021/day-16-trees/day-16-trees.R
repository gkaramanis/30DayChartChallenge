library(tidyverse)
library(here)
library(ggraph)
library(igraph)
library(patchwork)
library(lubridate)
library(wesanderson)

tweets_w1w2 <- list.files(path = here("2021", "data"), pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.)) %>% 
  mutate(hour = hour(created_at))

tweet_tree0 <- data.frame(from = "day", to = 0:23)

# I make two plots for days 13 and 14 because I don't know how to facet
# day 13
tweet_tree1 <- tweets_w1w2 %>%
  filter(day == 13) %>% 
  filter(is_retweet) %>%
  summarise(from = hour, to = retweet_status_id)

tweet_tree2 <- tweets_w1w2 %>% 
  filter(day == 13) %>% 
  filter(is_retweet) %>% 
  summarise(from = retweet_status_id, to = status_id)

edges <- rbind(tweet_tree0, tweet_tree1, tweet_tree2)
mygraph <- graph_from_data_frame(edges)

pal <- wes_palette("Zissou1")
depth_lbl <- c("day", "hour", "tweet", "retweets")

p1 <- ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = factor(depth)), color = "grey30", size = 0.15) +
  scale_fill_manual(values = pal, labels = depth_lbl) +
  labs(fill = NULL, title = "Day 13") +
  coord_fixed() +
  theme_void(base_family = "Fira Sans") +
  theme(plot.title = element_text(hjust = 0.5))

# day 14
tweet_tree3 <- tweets_w1w2 %>%
  filter(day == 14) %>% 
  filter(is_retweet) %>% 
  summarise(from = hour, to = retweet_status_id)

tweet_tree4 <- tweets_w1w2 %>% 
  filter(day == 14) %>% 
  filter(is_retweet) %>% 
  summarise(from = retweet_status_id, to = status_id)

edges <- rbind(tweet_tree0, tweet_tree3, tweet_tree4)
mygraph <- graph_from_data_frame(edges)

p2 <- ggraph(mygraph, layout = 'circlepack') + 
  geom_node_circle(aes(fill = factor(depth)), color = "grey30", size = 0.15) +
  scale_fill_manual(values = pal) +
  labs(title = "Day 14") +
  coord_fixed() +
  theme_void(base_family = "Fira Sans") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# join plots and save
p1 + p2 +
  plot_annotation("Retweets tagged with #30DayChartChallenge",
                  caption = "source: Twitter · graphic: Georgios Karamanis",
                  theme = theme(
                    plot.title = element_text(size = 16, hjust = 0.5, family = "Fira Sans Bold"),
                    plot.caption = element_text(size = 11, hjust = 0.5, family = "Fira Sans")
                    )
  ) 

ggsave(here("2021", "day-16-trees", "day-16-trees.png"), dpi = 320)

