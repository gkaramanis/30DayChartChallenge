library(tidyverse)
library(scales)
library(ggtext)

tweets_w1_raw <- read_csv("2021/data/tweets_w1.csv")

tweets_counts <- tweets_w1_raw %>% 
  add_count(screen_name) %>% 
  distinct(name, screen_name, n, followers_count, friends_count) %>% 
  select(n, everything()) %>% 
  mutate(rich_name = paste0("**", name, "**<br>", screen_name))

pal <- colorspace::darken(c('#e6194B', '#3cb44b', '#9A6324', '#4363d8', '#800000', '#42d4f4', '#f032e6', '#000075', '#469990', '#dcbeff'), 0.2)

ggplot(tweets_counts, aes(x = followers_count, y = friends_count, size = n)) +
  geom_point(alpha = 0.4, color = "grey30", fill = "#aaffc3", shape = 21) +
  geom_richtext(data = subset(tweets_counts, followers_count > 100000), aes(label = rich_name, color = rich_name), size = 3, hjust = 0, nudge_x = 0.01, family = "Fira Sans Compressed", lineheight = 1, fill = NA, label.color = NA) +
  geom_richtext(data = subset(tweets_counts, n > 50), aes(label = rich_name, color = rich_name), size = 3, hjust = 1, nudge_x = -0.01, family = "Fira Sans Compressed", lineheight = 1, fill = NA, label.color = NA) +
  annotate("text", x = 0.45, y = 20000, label = str_wrap("Size of bubble indicates number of tweets. Friends is the number of accounts a user follows. Highlighted are the users with more than 100K followers or more than 50 tweets.", 32), hjust = 0, vjust = 1, family = "American Typewriter", size = 3.5) +
  scale_size_continuous(range = c(1, 12)) +
  scale_x_log10(labels = label_number_si(), breaks = seq(0, 1000000, by = 50000)) +
  scale_y_log10(labels = label_number_si(), breaks = seq(0, 1000000, by = 50000)) +
  scale_color_manual(values = pal) +
  guides(color = FALSE) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Users that have tweeted during the first week of #30DayChartChallenge", 
    x = "Followers (log)",
    y = "Friends (log)",
    size = "Number of tweets",
    caption = "source: Twitter · graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = "American Typewriter Bold") +
  theme(
    legend.position = c(0.15, 0.07),
    legend.direction = "vertical",
    plot.background = element_rect(fill = "#ffd8b1", color = NA),
    panel.grid = element_line(color = "#9A6324", size = 0.1),
    axis.text = element_text(color = "black", size = 8, family = "American Typewriter"),
    axis.text.x = element_text(angle = -90, vjust = 0.5),
    plot.margin = margin(40, 50, 40, 20),
    plot.title = element_text(margin = margin(0, 0, 20, 0)),
    plot.title.position = "panel",
    plot.caption = element_text(family = "American Typewriter Light", hjust = 0.5, margin = margin(20, 0, 0, 0))
  ) +
  ggsave(here::here("2021/day-13-correlation/day-13-correlation.png"), dpi = 320, width = 8, height = 8)

