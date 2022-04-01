library(tidyverse)
library(lubridate)

tweets_w1_raw <- read_csv("2021/data/tweets_w1.csv")
  
tweets_w1 <- tweets_w1_raw %>% 
  filter(is_retweet == FALSE) %>%
  select(status_id, text, hashtags, created_at, lang, day)

tweet_stats <- tweets_w1 %>% 
  mutate(
    hour = hour(created_at),
    lang = fct_rev(fct_other(lang, keep = c("en", "fr", "es"))),
    lang = fct_recode(lang, English = "en", French = "fr", Spanish = "es")
  ) %>% 
  count(hour, lang)

ggplot(tweet_stats, aes(x = hour, y = lang, size = n, label = n)) +
  geom_violin(color = NA, fill = "#CBC3E3", scale = "count") +
  geom_point(shape = 21, fill = "orange") +
  geom_text(nudge_y = -0.125, family = "Fira Sans Bold", color = colorspace::darken("#CBC3E3", 0.7)) +
  scale_size_continuous(range = c(1.8, 3.5)) +
  scale_x_continuous(breaks = seq(0, 22, by = 2), limits = c(0, 23), sec.axis = dup_axis()) +
  labs(
    title = "Distribution of tweets tagged with #30DayChartChallenge\nby language and hour of day",
    caption = "data: Twitter · graphic: Georgios Karamanis"
       ) +
  theme_minimal(base_family = "Fira Sans Bold") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(color = "grey56"),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.margin = margin(20, 20, 10, 20),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0), lineheight = 1, size = 16),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), size = 8, color = "grey70")
  ) 

ggsave(here::here("2021/day-9-statistics/day-9-statistics-violinscale.png"), dpi = 320, width = 7, height = 7)

