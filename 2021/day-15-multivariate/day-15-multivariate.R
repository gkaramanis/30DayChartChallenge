library(tidyverse)
library(here)
library(lubridate)

tweets_w1w2 <- list.files(path = here("2021", "data"), pattern = "*.csv", full.names = TRUE) %>% 
  map_df(~read_csv(.))

plot_tw <- tweets_w1w2 %>% 
  mutate(hour = hour(created_at)) %>% 
  count(day, hour, is_retweet) %>% 
  pivot_wider(names_from = is_retweet, values_from = n, names_prefix = "retweet_") %>% 
  mutate(
    a = retweet_FALSE / max(retweet_FALSE, na.rm = TRUE) * 2 * pi,
    s = retweet_TRUE / max(retweet_TRUE, na.rm = TRUE) / 2
  )

ggplot(plot_tw, aes(x = hour, y = day, angle = a, radius = 0.4)) +
  geom_segment(aes(x = hour, y = day, xend = hour + 0.17, yend = day), color = "#6EC5B8") +
  geom_point(size = 5, shape = 21, color = "#6EC5B8") +
  geom_spoke(aes(size = s), arrow=arrow(length = unit(0.2,"cm")), color = "#FF9000") +
  scale_size_continuous(range = c(0.2, 3)) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(breaks = 1:14) +
  coord_fixed(expand = FALSE, clip = "off") +
  labs(
    title = str_wrap("Tweets tagged with #30DayChartChallenge, by hour for days 1 to 14. The arrows indicate tweets (size) and retweets (angle) as percentage of their max values", 80), 
    caption = "source: Twitter · graphic: Georgios Karamanis"
    ) +
  theme_minimal(base_family = "Spot Mono Bold") +
  theme(
    legend.position = "none",
    plot.background = element_rect(color = NA),
    axis.text = element_text(color = "#978B7D"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, family = "Spot Mono", size = 10, margin = margin(20, 0, 0, 0))
  ) 

ggsave(here("2021", "day-15-multivariate", "day-15-multivariate.png"), dpi = 320, width = 10, height = 7)
