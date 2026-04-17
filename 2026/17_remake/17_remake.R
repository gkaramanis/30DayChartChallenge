library(tidyverse)
library(camcorder)

gg_record(here::here("2026/temp/"), device = "png", height = 11.08, width = 8, units = "in", dpi = 320)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg <- "#FAFAF6"
clr_text <- "#1A1A18"
clr_text2 <- "#6B6B65"
clr_accent <- "#E8A020"
clr_accent2 <- "#3D4F9F"
clr_grid <- "#E0E0D8"

# Children's school journeys by grade and mode, Uppsala municipality 2021
# Source: Uppsala kommun, guardian survey (n > 7 400)
# https://www.uppsala.se/kommun-och-politik/sa-arbetar-vi-med-olika-amnen/cykel/barns-resvanor-i-uppsala-kommun/

school_journeys <- tibble(
  grade = c("F-class", "Year 1", "Year 2", "Year 3", "Year 4", "Year 5", "Year 6"),
  Walk = c(26, 31, 33, 37, 37, 40, 30),
  Cycling = c(31, 27, 29, 28, 29, 33, 32),
  `Public transit` = c(7, 9, 11, 15, 19, 15, 27),
  Car = c(36, 33, 27, 21, 15, 13, 11)
) |>
  pivot_longer(-grade, names_to = "mode", values_to = "share") |>
  mutate(
    grade = fct_inorder(grade),
    mode = factor(mode, levels = c("Car", "Walk", "Cycling", "Public transit"))
  )

ggplot(school_journeys, aes(x = grade, y = share, group = mode,  fill = mode, color = mode)) +
  ggtrace::geom_line_trace(size = 2, color = clr_bg) +
  geom_point(size = 5, show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Car" = clr_accent2,
      "Walk" = clr_text2,
      "Cycling" = clr_text,
      "Public transit" = clr_accent
    ),
    guide = guide_legend(title = NULL, nrow = 1, reverse = TRUE)
  ) +
  scale_fill_manual(
    values = c(
      "Car" = clr_accent2,
      "Walk" = clr_text2,
      "Cycling" = clr_text,
      "Public transit" = clr_accent
    ),
    guide = guide_legend(title = NULL, nrow = 1, reverse = TRUE)
  ) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Kids go their own way",
    subtitle = "Share of journeys by mode and grade in Uppsala, 2021",
    caption = "Source: Uppsala kommun, guardian survey · Graphic: Georgios Karamanis",
  ) +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(margin = margin(l = 5, r = 20)),
    axis.title = element_blank(),
    axis.text.x = element_text(color = clr_text, size = 14, face = "bold"),
    axis.text.y = element_text(color = clr_text, size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(size = 13, color = clr_text2, margin = margin(t = 6, b = 20), hjust = 0.5),
    plot.caption = element_text(size = 10, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()