library(tidyverse)
library(ggrepel)
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

# Monthly ridership vs. average temperature
# Source: UL Statistik · SMHI Open Data

ul_raw <- read_csv(here::here("2026/data/ul_ridership_punctuality_2024_2025.csv"), show_col_types = FALSE)

temp_raw <- read_delim(here::here("2026/data/smhi_temperature_Uppsala_Aut_97510.csv"), delim = ";", skip = 10, show_col_types = FALSE)

temp_monthly <- temp_raw |>
  select(date = Datum, temp = Lufttemperatur) |>
  filter(!is.na(temp)) |>
  mutate(year = year(date), month = month(date)) |>
  filter(year %in% c(2024, 2025)) |>
  group_by(year, month) |>
  summarise(temp_avg = mean(temp, na.rm = TRUE), .groups = "drop")

ul <- ul_raw |>
  mutate(
    total = ul_city_trips,
    month_lab = month.abb[month]
  ) |>
  left_join(temp_monthly, by = c("year", "month"))

ggplot(ul, aes(x = temp_avg, y = total)) +
  geom_smooth(method = "lm", se = FALSE, color = clr_grid, linewidth = 1.2) +
  geom_point(aes(color = factor(year)), size = 5) +
  geom_text_repel(aes(label = month_lab, color = factor(year)), family = f1, size = 6, fontface = "bold", force_pull = 0, force = 5, seed = 99, bg.color = clr_bg, min.segment.length = 10) +
  scale_color_manual(values = c("2024" = clr_accent2, "2025" = clr_accent), name = NULL) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_x_continuous(labels = scales::label_number(suffix = "°C")) +
  labs(
    title = "A seasonal pattern",
    subtitle = "Bus ridership vs avg. temperature in Uppsala",
    caption = "Source: UL Statistik · SMHI Open Data · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15, color = clr_text, margin = margin(l = 5, r = 20), hjust = 0.5),
    legend.margin = margin(t = 20, r = 30),
    panel.grid.major = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.title = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, hjust = 0.5, face = "bold", size = 36, color = clr_text, margin = margin(t = 20, b = 6)),
    plot.subtitle = element_text(hjust = 0.5, color = clr_text2, margin = margin(b = 16)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0, margin = margin(t = 30)),
    plot.margin = margin(t = 20, r = 35, b = 16, l = 30)
  )

record_polaroid()

