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

# UL/Region Uppsala monthly pass (stad) prices 2014–2026
# Sources: UNT and UL archives; ul_30day_prices_historical.csv for 2025–2026

prices_raw <- read_csv(here::here("2026/data/unt_ul_priser_historik_1978_2024.csv"))
recent_raw <- read_csv(here::here("2026/data/ul_30day_prices_historical.csv"))

recent <- recent_raw |>
  filter(ticket_type == "30day_vuxen", year >= 2025) |>
  select(year, price = price_sek)

prices <- prices_raw |>
  filter(!is.na(periodbiljett_stad), year >= 2014) |>
  select(year, price = periodbiljett_stad) |>
  bind_rows(recent)

fit <- lm(log(price) ~ year, data = prices)

proj_years <- tibble(year = 2014:2050)
pred <- broom::augment(fit, newdata = proj_years, interval = "confidence") |>
  mutate(across(c(.fitted, .lower, .upper), exp))

ggplot() +
  geom_ribbon(data = pred, aes(x = year, ymin = .lower, ymax = .upper), fill = clr_accent, alpha = 0.12) +
  geom_line(data = pred |> filter(year <= 2026), aes(x = year, y = .fitted), color = clr_accent, linewidth = 1) +
  geom_line(data = pred |> filter(year >= 2026), aes(x = year, y = .fitted), color = clr_accent, linewidth = 1, linetype = "dashed") +
  geom_point(data = prices, aes(x = year, y = price), color = clr_text, size = 3.5, stroke = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = scales::label_number(big.mark = " ", suffix = " kr")) +
  labs(
    title = "2 500 kr by 2050?",
    subtitle = "Uppsala public transport monthly pass price",
    caption = "Nominal prices, log-linear model assumes constant % annual growth\nSource: UNT, UL · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color = clr_text2),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 16), lineheight = 1.3),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
