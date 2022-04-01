library(tidyverse)
library(here)
library(RColorBrewer)

tweets_w1w2 <- list.files(path = here("2021", "data"), pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))

worldwide <- tweets_w1w2 %>% 
  filter(str_detect(tolower(location), "world")) %>% 
  group_by(day) %>% 
  summarise(day, fav_sum = sum(favorite_count), re_sum = sum(retweet_count)) %>% 
  ungroup() %>% 
  distinct()

f1b = "JetBrains Mono Bold"
f2 = "Fira Sans"

pal <- brewer.pal(n = 8, name = "Purples")[4:8]

ggplot(worldwide, aes(x = fav_sum, y = re_sum, label = day)) +
  geom_path(aes(color = day), size = 1.5) +
  geom_point(aes(fill = day), size = 6, shape = 21, color = "grey90") +
  geom_text(color = "grey95", size = 2.8, family = f1b, check_overlap = TRUE) +
  annotate("text", worldwide$fav_sum[1] - 7, worldwide$re_sum[1], hjust = 1, label = "Challenge\nstarts here", family = f2, size = 3, color = "darkorange3") +
  annotate("text", worldwide$fav_sum[14] + 7, worldwide$re_sum[14], hjust = 0, label = "Second week\nends here", family = f2, size = 3, color = "darkorange3") +
  scale_color_stepsn(colors = pal) +
  scale_fill_stepsn(colors = pal) +
  # coord_fixed(ratio = 0.33) +
  labs(
    x = "Favorites",
    y = "Retweets",
    title = str_wrap("Total favorites and retweets of tweets tagged with #30DayChartChallenge during the first two weeks of the challenge by people that have the word 'world' in their location", 52),
    caption = "Source: Twitter · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0), size = 17, face = "bold", color = "darkorange2", lineheight = 1),
    plot.caption = element_text(hjust = 1, margin = margin(30, 0, 0, 0), size = 8, color = "grey50"),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "grey97", color = NA)
    ) 

ggsave(here::here("2021/day-19-global-change/day-19-global-change.png"), dpi = 320, width = 7, height = 7)
