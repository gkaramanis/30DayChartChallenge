library(tidyverse)
library(wesanderson)

tweets_w1_raw <- read_csv("2021/data/tweets_w1.csv")

spaces <- tweets_w1_raw %>% 
  filter(!is_retweet) %>% 
  select(day, status_id, text, display_text_width) %>% 
  rowwise() %>% 
  mutate(space_i = str_locate(text, "\\s")[[1]][1]) %>% 
  add_count(display_text_width, space_i)

pal <- wes_palette("Zissou1", type = "continuous")

f1c <- "Fira Sans Compressed"
f1d <- "Fira Sans Condensed"


ggplot(spaces) +
  geom_tile(aes(x = display_text_width, y = space_i, height = 1, width = 1, fill = n)) +
  annotate("text", x = 20, y = 40, label = "Tweets that start with\n'#30DayChartChallenge'", hjust = 0, family = f1d) +
  annotate("curve", x = 15, y = 39, xend = 16, yend = 22, arrow = arrow(length = unit(0.2, "cm")), curvature = 0.4, size = 0.3, color = "grey40") +
  annotate("text", x = 280, y = 36, label = "Tweets that start with 'day'\nand other 3-letter words", hjust = 1, family = f1d) +
  annotate("curve", x = 284, y = 35, xend = 292, yend = 5, arrow = arrow(length = unit(0.2, "cm")), curvature = -0.3, size = 0.3, color = "grey40") +
  scale_fill_gradientn(colors = pal) +
  scale_x_continuous(limits = c(0, 300), minor_breaks = seq(0, 300, by = 10)) +
  scale_y_continuous(limits = c(0, 50), minor_breaks = seq(0, 50, by = 1)) +
  labs(
    x = "tweet length",
    y = "position of first whitespace character",
    title = "Position of first whitespace character and text length of tweets tagged with #30DayChartChallenge",
    caption = "source: Twitter · graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_legend(title = "number of tweets", title.position = "bottom", title.hjust = 0.5, label.position = "top")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_minimal(base_family = f1c) +
  theme(
    legend.position = "top",
    legend.key.height = unit(0.5, "line"),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(color = "grey40", size = 8)
  ) 

ggsave(here::here("2021/day-14-space/day-14-space.png"), dpi = 320, width = 8, height = 6)
