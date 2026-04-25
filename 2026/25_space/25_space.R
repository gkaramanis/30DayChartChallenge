library(tidyverse)
library(ggdist)
library(distributional)
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

# Parking spaces per street segment, Uppsala
# Source: Uppsala kommun (kartportal.uppsala.se)

parking <- bind_rows(
  sf::read_sf(here::here("2026/data/parking/avgiftsparkeringar.gpkg")) |> sf::st_drop_geometry(),
  sf::read_sf(here::here("2026/data/parking/tidsbegransad.gpkg")) |> sf::st_drop_geometry()
) |>
  filter(!is.na(AntalPlatser), !is.na(Adress))

top_streets <- parking |>
  count(Adress, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(Adress)

street_data <- parking |>
  filter(Adress %in% top_streets) |>
  mutate(Adress = fct_reorder(Adress, AntalPlatser, .fun = mean))

m_parking <- lm(AntalPlatser ~ Adress, data = street_data)

street_data |>
  expand(Adress) |>
  broom::augment(m_parking, newdata = _, se_fit = TRUE) |>
  ggplot(aes(y = Adress)) +
  stat_halfeye(aes(xdist = dist_student_t(df = df.residual(m_parking), mu = .fitted, sigma = .se.fit)), scale = 0.5, color = clr_accent2, fill = clr_accent) +
  geom_point(aes(x = AntalPlatser), data = street_data, pch = "▲", size = 2, color = clr_text2, position = ggpp::position_jitternudge(width = 0.05, direction = "as.is", y = -0.15, nudge.from = "original.y"), alpha = 0.5) +
  scale_y_discrete(labels = label_wrap_gen(20)) +
  labs(
    title = "Parking spaces in Uppsala",
    subtitle = "Estimated mean and uncertainty, top 10 streets",
    caption = "Source: Uppsala municipality · Graphic: Georgios Karamanis",
    x = "Number of parking spaces per street segment"
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    plot.background = element_rect(fill = clr_bg, color = NA),
    panel.grid.major.x = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(color = clr_text2, size = 14, margin = margin(t = 10)),
    axis.title.y = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, color = clr_text, hjust = 0.5, margin = margin(t = 20, b = 6)),
    plot.subtitle = element_text(hjust = 0.5, color = clr_text2, margin = margin(b = 16)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 30)),
    plot.margin = margin(t = 20, r = 35, b = 16, l = 30)
  )

record_polaroid()
