library(tidyverse)
library(camcorder)
library(waffle)
library(plyr) # needed for waffle

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 6, height = 10, units = "in", dpi = 320)

# source: https://www.filminstitutet.se/sv/nyheter/2020/allt-fler-filmer-med-hbtq-tema-pa-bio/

films <- tribble(
  ~year, ~total, ~lgbtq,
  2015, 272, 16,
  2016, 305, 16,
  2017, 288, 20,
  2018, 284, 18, 
  2019, 293, 31
) %>% 
  mutate(not_lgbtq = total - lgbtq) %>% 
  pivot_longer(3:4)

f1 = "Futura"

ggplot(films, aes(fill = name, values = value)) +
  geom_waffle(color = "grey97") +
  geom_text(data = subset(films, name == "lgbtq"), aes(-1.5, 5, label = paste0(value, "\n(", round(value/total * 100), "%)")), size = 4.5, family = f1, color = "mediumpurple3") +
  geom_text(data = subset(films, name == "not_lgbtq"), aes(total %/% 10 + 1, total %% 10 + 1, label = ifelse(year == 2015, paste0(total, " total"), total)), hjust = 0, family = f1, vjust = 0) +
  scale_fill_manual(values = c("mediumpurple3", "grey70")) +
  coord_fixed() +
  facet_wrap(vars(year), ncol = 1) +
  labs(
    title = "LGBTQ-themed films played in Swedish cinemas",
    caption = "Source: Swedish Film Institute · Graphic: Georgios Karamanis"
      ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 15, margin = margin(20, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(10, 0, 10, 0))
  )
