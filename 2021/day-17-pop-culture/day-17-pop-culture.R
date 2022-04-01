library(tidyverse)
library(here)
library(ggrepel)
library(wesanderson)

tweets_w1w2 <- list.files(path = here("2021", "data"), pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))

pop <- tweets_w1w2 %>% 
  filter(!is_retweet) %>% 
  filter(str_detect(text, "movie|film|music|lyric|song|book|comic|game|tv|serie")) %>% 
  select(favorite_count, retweet_count, profile_image_url, name, text)

f1p <- "Fira Sans Compressed"
f1pm <- "Fira Sans Compressed Medium"
f2 <- "Charter"

pal <- wes_palette("Zissou1", n = 60, type = "continuous")

ggplot(pop, aes(favorite_count, retweet_count, color = name),) +
  geom_point() +
  geom_text_repel(aes(label = if_else(favorite_count > 30 | retweet_count > 5, name, NULL), size = favorite_count), family = f1pm) +
  annotate("text", 4, 17, hjust = 0, vjust = 1, label = str_wrap("Users that have tweeted about pop culture* during the first two weeks of #30DayChartChallenge", 34), size = 7, family = f2, fontface = "bold", lineheight = 0.95, color = "grey90") +
  annotate("text", 4, 14.25, hjust = 0, vjust = 1, label = "*Tweets containing\n  movie|film|music|lyric|song|book|comic|game|tv|serie", size = 4, family = f2, lineheight = 0.95, color = "grey80") +
  scale_color_manual(values = pal) +
  coord_cartesian() +
  labs(
    x = "Favorites",
    y = "Retweets"
  ) +
  theme_minimal(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#5F6479", color = NA),
    panel.grid = element_line(color = "grey20", size = 0.1),
    axis.text = element_text(color = "grey70", size = 12),
    axis.title = element_text(face = "bold", color = "grey85", size = 12),
    plot.margin = margin(20, 20, 20, 20)
  ) 

ggsave(here::here("2021/day-17-pop-culture/day-17-pop-culture.png"), dpi = 320, width = 8, height = 8)
