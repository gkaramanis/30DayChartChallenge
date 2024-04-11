library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp/"), device = "png", height = 1080 * 2, width = 1350 * 2, units = "px", dpi = 320)

wh <- olympics %>% 
  filter(season == "Summer") %>% 
  group_by(age, year, sex) %>% 
  summarise(
    n = n(),
    h = mean(height, na.rm = TRUE),
    w = mean(weight, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(year, sex) %>% 
  group_by(sex) %>% 
  mutate(
    dh = round(h - first(na.omit(h)), 1),
    dw = round(w - first(na.omit(w)), 1)
  ) %>% 
  ungroup() %>% 
  filter(h > 0 & !is.na(age)) %>% 
  mutate(yob = year - age) %>% 
  mutate(sex = case_match(
    sex,
    "M" ~ "Men",
    "F" ~ "Women",
  )) %>% 
  filter(age > 17)


men_h <- read_csv(here::here("2024/data/OWID/average-height-of-men.csv")) %>% 
  janitor::clean_names() %>% 
  filter(entity == "World") %>% 
  mutate(sex = "Men")

women_h <- read_csv(here::here("2024/data/OWID/average-height-of-women.csv")) %>% 
  janitor::clean_names() %>% 
  filter(entity == "World") %>% 
  mutate(sex = "Women")

ggplot() +
  geom_smooth(data = wh, aes(x = yob, y = h, color = sex), fill = "grey80") +
  geomtextpath::geom_textline(data = men_h, aes(x = year, y = mean_male_height_cm, label = "World"), size = 3) +
  geomtextpath::geom_textline(data = women_h, aes(x = year, y = mean_female_height_cm, label = "World"), size = 3) +
  scale_color_manual(values = c("purple", "green4")) +
  facet_wrap(vars(sex), ncol = 2) +
  labs(
    title = "Mean height of Olympic athletes compared to the global mean height",
    subtitle = "Mean height of adults only",
    x = "Year of birth",
    y = "Mean height (cm)",
    caption = "Source: Kaggle & Our World in Data · Graphic: Georgios Karamanis"
    ) +
  theme_minimal(base_family = "Outfit") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.spacing.x = unit(2, "line"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank()
  )
  
