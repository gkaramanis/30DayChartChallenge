library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 12, height = 8, units = "in", dpi = 320)

sex_viol <- tribble(
  ~"group_id", ~"group", ~"pct", ~"comment",
  1, "Out of the 781 respondents, 30% had been forced at least once to having sex against their will.", 30, "Ever",
  2, "Among those, about 1 in 3 were forced by a long-term or casual partner,", 33, NULL,
  3, "about 1 in 4 by a stranger,", 25, NULL,
  4, "and 1 in 5 by a friend.", 20, NULL,
  5, "More than one third of respondents in the age group 20-44 had been subjected to sexual violence the past 12 months.", 33, "Last 12 months"
)

people <- sex_viol %>% 
  rowwise() %>% 
  mutate(
    i = list(1:100),
    parts =  list(sample(c(letters, LETTERS), 100, replace = TRUE))
  ) %>% 
  unnest(c(i, parts)) %>% 
  group_by(group) %>% 
  mutate(col = if_else(row_number() <= pct, "mediumpurple3", "grey80")) %>% 
  ungroup()

f1 = "Futura"

ggplot(people) +
  geom_text(aes(i, group_id, color = col, label = parts), family = "WeePeople", size = 9) +
  geom_text(aes(0.5, group_id, label = group), family = f1, size = 5, stat = "unique", hjust = 0, nudge_y = -0.3, color = "mediumpurple2") +
  scale_color_identity() +
  scale_y_reverse() +
  labs(
    title = "Sexual violence among transgender people in Sweden",
    subtitle = "Based on a self-selected, anonymous web-based survey (2014)",
    caption = "Source: Health and health determinants among transgender persons - a report on the\nhealth status of transgender persons in Sweden (2015) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_text(hjust = 0.08, size = 16, margin = margin(15, 0, 0, 0), color = "mediumpurple3"),
    plot.subtitle = element_text(hjust = 0.08, size = 14, margin = margin(5, 0, 20, 0), color = "mediumpurple3"),
    plot.caption = element_text(hjust = 0.93, margin = margin(10, 0, 10, 0), color = "grey50"),
    plot.margin = margin(0, 0, 0, 0)
  )
