library(rtweet)
library(dplyr)
library(stringr)
library(ggplot2)
library(waffle)
library(wesanderson)

tweets_30 <- search_tweets("#30daychartchallenge", n = 10000)

tweets_30_source <- tweets_30 %>% 
  distinct(user_id, source) %>% 
  filter(source != "") %>% 
  group_by(source) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  mutate(fa_icon = case_when(
    source == "Twitter for Android" ~ "android",
    source == "Twitter for iPhone" ~ "apple",
    source == "Twitter for iPad" ~ "apple",
    str_detect(source, "Tweetbot") ~ "apple",
    str_detect(source, "Mac") ~ "apple",
    source == "Twitter Web App" ~ "firefox",
    str_detect(source, "_bot") ~ "dev",
    str_detect(source, "rstats") ~ "dev",
    str_detect(source, "API") ~ "dev",
    TRUE ~ "dev"
  )) %>% 
  group_by(fa_icon) %>% 
  summarise(icon_n = sum(n)) %>% 
  ungroup()

pal <- wes_palette("Moonrise2")

ggplot(tweets_30_source) +
  geom_pictogram(aes(color = fa_icon, label = fa_icon, values = icon_n),
                 family = "Font Awesome 5 Brands", size = 7,
                 flip = TRUE, n_rows = 10, make_proportional = TRUE) +
  scale_label_pictogram(
    name = NULL,
    values = c("android", "apple", "dev", "firefox"),
    labels = c("Twitter for Android", "iOS/macOS apps", "Bots and other", "Twitter Web App")
  ) +
  scale_color_manual(values = pal) +
  guides(color = FALSE, label = guide_legend(nrow = 2)) +
  labs(
    title = "#30DayChartChallenge\nTwitter clients used by unique users",
    caption = "Source: Twitter · Graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = "Futura Medium") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(vjust = 1, color = "grey40"),
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.margin = margin(15, 10, 10, 10),
    plot.title = element_text(hjust = 0.5, size = 12, margin = margin(0, 0, 10, 0), family = "Futura Bold", lineheight = 1, color = "#62AAC5"),
    plot.caption = element_text(hjust = 0.5, size = 7, margin = margin(0, 0, 0, 0), lineheight = 1, color = "#62AAC5")
  )  +
  ggsave(here::here("2021/day-2-pictogram/day-2-pictogram.png"), dpi = 320, width = 5, height = 6)

