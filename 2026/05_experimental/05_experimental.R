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

# Monthly UL ridership 2024–2025
# Source: UL Statistik — https://www.ul.se/sidfot/om-ul/statistik-for-ul/
ul_raw <- read_csv(
  here::here("2026/data/ul_ridership_punctuality_2024_2025.csv"),
  show_col_types = FALSE
)
# Hourly air temperature, Uppsala Aut station
# Source: SMHI Open Data API
# https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/1/station/97510/period/corrected-archive/data.csv

temp_raw <- read_delim(
  here::here("2026/data/smhi_temperature_Uppsala_Aut_97510.csv"),
  delim = ";", skip = 10, show_col_types = FALSE
)

temp_monthly <- temp_raw |>
  select(date = Datum, temp = Lufttemperatur) |>
  mutate(year = year(date), month = month(date)) |>
  filter(year %in% c(2024, 2025)) |> 
  group_by(month) |>
  summarise(temp_avg = mean(temp, na.rm = TRUE), .groups = "drop")

ul <- ul_raw |>
  select(month, total = ul_city_trips) |>
  group_by(month) |>
  summarise(total = mean(total), .groups = "drop")

temp_ul <- temp_monthly |>
  left_join(ul) |>
  mutate(
    month_plot = if_else(month < 7, month + 12L, month),
    month_abbr = month.abb[month]
    )
  
ggplot(temp_ul, aes(y = month_plot, x = 3e6, fill = temp_avg, width = total, height = 0.98)) +
  geom_tile(aes(color = after_scale(colorspace::darken(fill, 0.1))), linewidth = .8) +
  geom_text(aes(label = if_else(month %in% c(1, 6, 7), month_abbr, "")), fontface = "bold", color = clr_bg, size = 10) +
  scale_fill_gradient2(low = clr_accent2, high = clr_accent, midpoint = 5) +
  scale_y_continuous(breaks = c(6:12, 13:17), labels = month.abb[c(6:12, 1:5)]) +
  labs(
    title = "Cold months fill the buses",
    subtitle = "Bus ridership vs. temperature in Uppsala",
    caption = "Bar length = monthly ridership; color = temperature; both 2024–2025 averages\nSource: UL, SMHI · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5),
    plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(t = 10, b = 50)),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    plot.margin = margin(t = 20, l = 20, r = 20, b = 18)
  )

record_polaroid()
