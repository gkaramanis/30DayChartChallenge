library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-09/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

athlete_bio <- read_csv(here::here("2024/data/olympedia/Olympic_Athlete_Bio.csv")) %>% 
  select(athlete_id, name, sex, born) %>% 
  mutate(
    born = as.Date(born),
    yob = year(born)
    ) %>% 
  filter(!is.na(born))

athlete_results <- read_csv(here::here("2024/data/olympedia/Olympic_Athlete_Event_Results.csv")) %>% 
  distinct(edition, athlete_id, name = athlete) %>% 
  filter(str_detect(edition, "Summer")) %>% 
  mutate(edition = parse_number(edition))

ages <- athlete_bio %>% 
  left_join(athlete_results) %>% 
  mutate(age = edition - yob) %>% 
  mutate(minor = age < 18) %>% 
  group_by(edition) %>% 
  summarise(
    total = n(),
    n = sum(minor),
    pct = n / total * 100
  )

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

pal <- c("#FF69B4", "#00FF00", "#FFA500")
pal <- c("#4B0082", "#39FF14", "#FF007F")

ggplot(ages) +
  geom_col(aes(edition, pct), fill = pal[3]) +
  coord_flip() +
  scale_x_reverse(breaks = seq(2020, 1896, -20)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = toupper("Share of athletes under 18\nin the Summer Olympics"),
    caption = "Source: Olympedia · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2b, base_size = 18) +
  theme(
    plot.background = element_rect(fill = pal[1], color = NA),
    axis.title = element_blank(),
    axis.text = element_text(color = pal[2]),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 28, hjust = 0.5, color = pal[2]),
    plot.caption = element_text(hjust = 0.5, size = 11, family = f2, color = pal[2]),
    plot.margin = margin(20, 50, 15, 50)
  )
  
