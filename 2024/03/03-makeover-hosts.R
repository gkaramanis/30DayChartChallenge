library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

hosts <- read_csv(here::here("2024/data/olympic_hosts.csv")) %>% 
  filter(game_season == "Summer")

hosts_n <- hosts %>% 
  mutate(
    city = str_remove(game_name, " \\d{4}"),
    city = case_when(
      city == "Rio" ~ "Rio de Janeiro",
      TRUE ~ city
    ),
    country = case_when(
      game_location == "Great Britain" ~ "UK",
      game_location == "USSR" ~ "Russia",
      game_location == "United States" ~ "USA",
      game_location == "Republic of Korea" ~ "Korea South",
      game_location == "Federal Republic of Germany" ~ "Germany",
      game_location == "Australia, Sweden" ~ "Australia",
      TRUE ~ game_location
    )
    ) %>% 
  left_join(maps::world.cities %>% select(city = name, country = country.etc, long, lat)) %>% 
  mutate(
    long = ifelse(city == "St. Louis", -90.1994, long),
    lat = ifelse(city == "St. Louis", 38.6270, lat)
    ) %>% 
  group_by(city) %>%
  arrange(game_year) %>%
  mutate(city_label = paste0(city, "\n", paste0(game_year, collapse = "\n"))) %>% 
  ungroup()

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

world <- map_data("world") %>% 
  filter(region != "Antarctica")

ggplot(hosts_n, aes(long, lat)) +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "#ffe700") +
  geom_point(color = "#EE334E", shape = "★", size = 2) +
  ggrepel::geom_label_repel(aes(label = city_label), size = 3.5, stat = "unique", family = f1b, lineheight = 0.9, max.overlaps = 2000, label.size = NA, segment.color = "#EE334E", segment.size = 0.3, alpha = 0.8, label.r = 0.3, seed = 1896) +
  annotate("text", 0, 150, label = toupper("Olympic Games\nhost cities"), family = f2b, color = "#001eff", lineheight = 0.9, size = 18) +
  annotate("text", 0, -80, label = "Source: Olympics.com · Graphic: Georgios Karamanis", family = f2, color = "#001eff") +
  coord_fixed(ratio = 1.4, clip = "off", ylim = c(-100, 200)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#74ee15", color = NA)
  )
