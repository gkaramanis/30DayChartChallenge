library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 12, height = 7, units = "in", dpi = 320)

legal <- tribble(
  ~legal_now, ~total, ~Changed, ~Want, ~No_need, ~Cannot, ~No_answer,
  "Female", 389, 55, 148, 34, 146, 6,
  "Male", 373, 60, 144, 123, 42, 4,
  "Don't want to say", 31, 0, 8, 0, 18, 5
  ) %>% 
  pivot_longer(3:last_col())

ggplot(legal) +
  geom_bar(aes(x = 1, y = value, fill = name), position = "fill", stat = "identity") +
  coord_polar(theta = "y") +
  xlim(0, 1.5) +
  facet_wrap(vars(legal_now))
