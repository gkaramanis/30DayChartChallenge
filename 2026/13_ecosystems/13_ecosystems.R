library(tidyverse)
library(camcorder)

gg_record(here::here("2026/temp/"), device = "png", height = 11.08, width = 8, units = "in", dpi = 320)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg      <- "#FAFAF6"
clr_text    <- "#1A1A18"
clr_text2   <- "#6B6B65"
clr_accent  <- "#E8A020"
clr_accent2 <- "#3D4F9F"
clr_grid    <- "#E0E0D8"

# Children's school journeys by grade and mode, Uppsala municipality 2021
# Source: Uppsala kommun, guardian survey (n > 7 400)
# https://www.uppsala.se/kommun-och-politik/sa-arbetar-vi-med-olika-amnen/cykel/barns-resvanor-i-uppsala-kommun/

school_journeys <- tibble(
  grade = c("F-class", "Year 1", "Year 2", "Year 3", "Year 4", "Year 5", "Year 6"),
  Walk = c(26, 31, 33, 37, 37, 40, 30),
  Cycling = c(31, 27, 29, 28, 29, 33, 32),
  `Public transit` = c(7,  9, 11, 15, 19, 15, 27),
  Car = c(36, 33, 27, 21, 15, 13, 11)
) |>
  pivot_longer(-grade, names_to = "mode", values_to = "share") |>
  mutate(
    grade = fct_inorder(grade),
    mode = factor(mode, levels = c("Car", "Public transit", "Cycling", "Walk"))
  )

ggplot(school_journeys, aes(x = mode, y = fct_rev(grade), size = share, fill = mode, color = after_scale(colorspace::darken(fill)))) +
  geom_point(alpha = 0.85, shape = 21) +
  geom_text(aes(label = paste0(share, "%")), color = clr_bg, family = f2, fontface = "bold", size = 5, show.legend = FALSE) +
  scale_x_discrete(expand = 0.15) +
  scale_y_discrete(expand = 0.1) +
  scale_size_area(max_size = 35, guide = "none") +
  scale_fill_manual(values = c("Car" = clr_accent2, "Public transit" = clr_accent, "Cycling" = clr_text, "Walk" = clr_text2), guide = "none") +
  labs(
    title = "Kids go their own way",
    subtitle = "Share of journeys by mode and grade in Uppsala, 2021",
    caption = "Source: Uppsala kommun, guardian survey · Graphic: Georgios Karamanis",
  ) +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(color = clr_text, size = 14, face = "bold"),
    axis.text.y = element_text(color = clr_text, size = 14),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(size = 13, color = clr_text2, margin = margin(t = 6, b = 20), hjust = 0.5),
    plot.caption = element_text(size = 10, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
