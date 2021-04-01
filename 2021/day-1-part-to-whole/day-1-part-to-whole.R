library(waffle)
library(dplyr)

day_1 <- data.frame(day = 1:30, done = c(TRUE, rep(FALSE, 29)))


ggplot(day_1) +
  geom_pictogram(aes(color = done, label = done, values = 1),
                 family = "Font Awesome 5 Free Solid", size = 10,
                 flip = TRUE, n_rows = 5) +
  scale_label_pictogram(
    name = NULL,
    values = c("times-circle", "check-circle"),
    labels = c(TRUE, FALSE)
  ) +
  scale_color_manual(values = c("#8023EB", "#58C1AA")) +
  labs(title = "Charts I've made for #30DayChartChallenge") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, family = "American Typewriter Bold", margin = margin(10, 0, 20, 0), size = 14, color = "#eb5223"),
    plot.margin = margin(20, 20, 10, 20),
    plot.background = element_rect(fill = "grey97", color = NA)
  ) +
  ggsave(here::here("2021/day-1-part-to-whole/day-1-part-to-whole.png"), dpi = 320, width = 5, height = 5)

