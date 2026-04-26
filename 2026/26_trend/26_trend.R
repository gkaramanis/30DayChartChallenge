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

# UL adult single ticket (one zone) prices 2010–2026
# Source: UL, ul.se; Region Uppsala press releases; web.archive.org

prices_raw <- read_csv(here::here("2026/data/ul_ticket_prices_historical.csv"))

prices <- bind_rows(
  prices_raw |>
    filter(str_starts(ticket_type, "enkelbiljett_vuxen")) |>
    mutate(fare = case_when(
      payment_method %in% c("värdekort", "reskassa") ~ "Stored value",
      payment_method %in% c("förköp", "förköp_sms") ~ "Advance ticket",
      payment_method == "ombord" ~ "On board"
    )) |>
    filter(!is.na(fare)) |>
    group_by(year, fare) |>
    slice_min(price_sek, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(year, price = price_sek, fare),
  # From 22 Jan 2024: 75-minutersbiljett — same price for all payment methods
  prices_raw |>
    filter(ticket_type == "75min_vuxen") |>
    transmute(year, price = price_sek) |>
    crossing(fare = c("Advance ticket", "On board"))
) |>
  filter(fare != "Stored value" | year <= 2020)

ggplot(prices, aes(x = year, y = price, color = fare, fill = fare, group = fare)) +
  geom_line(linewidth = 1.8, lineend = "round") +
  geom_point(shape = 21, size = 4, stroke = 0.5, color = clr_bg) +
  annotate("text", x = 2020.7, y = 26, label = "Discontinued", color = clr_text2, family = f1, size = 5, hjust = 0, vjust = 0.5) +
  annotate("segment", x = 2024, xend = 2024, y = 41, yend = 46, color = clr_text2, linewidth = 0.4) +
  annotate("text", x = 2025, y = 49, label = "Jan 2024: same price\nfor all payment methods", hjust = 1, family = f1, size = 6, color = clr_text2, lineheight = 1) +
  scale_color_manual(values = c("Advance ticket" = clr_accent, "On board" = clr_accent2, "Stored value" = clr_text2), breaks = c("On board", "Advance ticket", "Stored value"), name = NULL) +
  scale_fill_manual(values = c("Advance ticket" = clr_accent, "On board" = clr_accent2, "Stored value" = clr_text2), breaks = c("On board", "Advance ticket", "Stored value"), name = NULL) +
  scale_x_continuous(limits = c(2009, 2027)) +
  scale_y_continuous(labels = scales::label_number(suffix = " kr"), limits = c(10, 50)) +
  labs(
    title = "Uppsala bus ticket prices",
    subtitle = "Adult single-trip fare by payment type",
    caption = "Source: UL, web.archive.org · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2, base_size = 24) +
  theme(
    legend.position = "top",
    legend.location = "plot",
    legend.text = element_text(margin = margin(l = 5, r = 20)),
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.title = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 20)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
