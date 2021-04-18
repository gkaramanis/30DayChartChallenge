library(tidyverse)
library(here)
library(ggraph)
library(tidygraph)
library(wesanderson)
library(colorspace)

tweets_w1w2 <- list.files(path = here("2021", "data"), pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))

tweets_mentions <- tweets_w1w2 %>% 
  filter(!is_retweet) %>% 
  select(screen_name, mentions_screen_name, day) %>% 
  mutate(mentions_screen_name = strsplit(mentions_screen_name, " ")) %>% 
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name))
  
net30 <- tweets_mentions %>% 
  filter(day == 14) %>%
  as_tbl_graph() %>% 
  mutate(popularity = centrality_degree(mode = 'in'))

pal <- wes_palette("Zissou1", 75, type = "continuous")

ggraph(net30, layout = "kk") +
  annotate("text", 2.5, -0.5,
           label = str_wrap("Tweets with mentions on day 14 of #30DayChartChallenge Source: Twitter Graphic: Georgios Karamanis", 15),
           hjust = 0.5, size = 18, alpha = 0.1, family = "Fira Sans Condensed Heavy", lineheight = 0.9) +
  geom_edge_fan(aes(alpha = stat(index)), color = "grey20") +
  # geom_node_point(aes(size = popularity, color = name)) +
  geom_node_label(aes(label = name, fill = name),
                  size = 2.5, family = "Fira Sans Compressed",
                  label.padding = unit(0.2, "lines"),
                  label.size = 0, color = "grey98") +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_manual(values = darken(pal, 0)) +
  scale_fill_manual(values = darken(pal, 0.1)) +
  # coord_fixed() +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  ggsave(here::here("2021/day-18-connections/day-18-connections.png"), dpi = 320, width = 8.25, height = 8)
  # 
