library(tidyverse)
library(lubridate)
library(biscale)
library(cowplot)

tweets_w1_raw <- read_csv("2021/data/tweets_w1.csv")

tweets_media <- tweets_w1_raw %>% 
  mutate(
    day = paste0("day ", day),
    h = hour(created_at),
    m = minute(created_at),
    hm = h + m/60,
    media_t = if_else(!is.na(media_type), 2, 1),
    retweet_t = if_else(is_retweet, 2, 1)
    ) %>% 
  select(day, hm, retweet_t, media_t)

bi_media <- bi_class(tweets_media, x = retweet_t, y = media_t, dim = 2)

pal <- bi_pal_manual(val_1_1 = "#3B4994", val_1_2 = "#BE64AC",
                     val_2_1 = "#5AC8C8", val_2_2 = "#E8E8E8")

p <- ggplot(bi_media) +
  geom_tile(aes(x = hm, y = 0, width = 0.02, height = 0.7, fill = bi_class), color = NA) +
  bi_scale_fill(pal = pal, dim = 2) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  facet_wrap(vars(day), ncol = 1) +
  labs(
    title = "Tweets tagged with #30DayChartChallenge",
    subtitle = "source: Twitter · graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = "Fira Sans Medium") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15"),
    plot.title = element_text(color = "grey90", hjust = 0.85, margin = margin(0, 0, 15, 0), size = 19),
    plot.subtitle = element_text(color = "grey90", hjust = 0.89, margin = margin(0, 0, 65, 0), size = 14, family = "Fira Sans"),
    strip.text = element_text(color = "grey90", margin = margin(0, 0, 5, 0)),
    plot.margin = margin(20, 20, 20, 20)
    )

l <- ggplot(bi_media) +
  geom_tile(
    aes(x = retweet_t, y = media_t, fill = bi_class)) +
  bi_scale_fill(pal = pal, dim = 2) +
  labs(x = "no     yes\nretweet",
       y = "media\nno     yes") +
  coord_fixed() +
  theme_void(base_family = "Fira Sans Medium") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 8, color = "grey90", margin = margin(5, 5, 5, 5)),
    axis.title.y = element_text(angle = 90)
    )

ggdraw(p) +
  draw_plot(l, x = -0.35, y = 0.4, scale = 0.15) +
  ggsave(here::here("2021", "day-12-stripes", "day-12-stripes.png"), dpi = 320, width = 8, height = 8)
  
