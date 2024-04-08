library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-08/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

games <- read_csv(here::here("2024/data/olympic_hosts.csv")) %>% 
  select(game_year, game_season) %>% 
  mutate(game_season = paste0(game_season, "\nGames"))

both_games <- games %>% 
  pivot_wider(values_from = game_season, names_from = game_season) %>% 
  filter(!is.na(`Winter\nGames`) & !is.na(`Summer\nGames`))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

col1 <- "#A0BD51"
col2 <- "#F7CD4D"

ggplot() +
  geom_segment(data = both_games, aes(x = game_year, xend = game_year, y = `Summer\nGames`, yend = `Winter\nGames`), linewidth = 2) +
  geom_point(data = games, aes(game_year, game_season), shape = 21, size = 4.5, stroke = 2.5, fill = "white") +
  scale_x_continuous(breaks = c(1896, 1924, 1992, 2022), expand = expansion(mult = 0.02)) +
  scale_y_discrete(limits = rev) +
  coord_radial(inner.radius = 0.5, start = 0, end = 1.7 * pi) +
  guides(
    theta = guide_axis_theta(angle = 90),
    r = guide_axis(angle = 0)
  ) +
  labs(
    title = toupper("From 1924 to 1992, the\nWinter and Summer Olympics\nwere held in the same year"),
    caption = "Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1b) +
  theme(
    plot.background = element_rect(fill = col2, color = NA),
    axis.text = element_text(size = 17, color = colorspace::darken(col1, 0.5)),
    axis.title = element_blank(),
    panel.background = element_rect(fill = col1, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = col2, linewidth = 2),
    axis.text.y = element_text(face = "bold", lineheight = 0.8, size = 15),
    plot.margin = margin(0, 20, 0, 20),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(30, 0, 20, 0), face = "bold"),
    plot.caption = element_text(hjust = 0.5, margin = margin(30, 0, 20, 0), family = f1b, size = 11)
  )
  
