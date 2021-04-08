library(tidyverse)
library(vhs)

tweets_w1 <- read_csv("2021/data/tweets_w1.csv") %>% 
  # filter(is_retweet == FALSE) %>%  
  select(status_id, text, hashtags, created_at, day)

animals <- tweets_w1 %>% 
  group_by(day) %>% 
  summarise(
    text_by_day = paste0(text, collapse = ""),
    text_by_day_length = nchar(text_by_day)
    ) %>% 
  ungroup() %>% 
  mutate(
    animals = strsplit("animals", ""),
    animals_n = list(1:7)
    )  %>% 
  rowwise() %>% 
  mutate(chars_n = list(str_count(text_by_day, str_sub("animals", 1:7, 1:7)))) %>%
  ungroup() %>% 
  select(-text_by_day) %>% 
  unnest(c(animals, animals_n, chars_n)) %>% 
  mutate(freq = chars_n/text_by_day_length * 100)

pal <- vhs("recoton")

ggplot(animals) +
  geom_col(aes(x = animals_n, y = freq, fill = animals)) +
  geom_text(aes(x = animals_n, y = 0.8, label = animals, family = "Fira Sans Bold"), color = "grey97") +
  scale_fill_manual(values = pal) +
  scale_y_continuous(breaks = c(0, 2, 4, 6), labels = c("", 2, 4, 6)) +
  facet_wrap(vars(paste0("Day ", day)), ncol = 1, strip.position = "right") +
  labs(
    title = str_wrap("Frequency (%) of some letters in tweets tagged with #30DayChartChallenge by day", 45),
    caption = "data: Twitter · graphic: Georgios Karamanis") +
  theme_minimal(base_family = "Fira Sans") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_text(size = 10),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey50"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 9, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(15, 20, 20, 20)
  ) +
  ggsave(here::here("2021/day-8-animals/day-8-animals.png"), dpi = 320, width = 4, height = 8)

