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

# Source: UL GTFS via KoDa API
# 4-31 April, 2026
gtfs <- here::here("2026/data/ul_gtfs")

calendar_dates <- read_csv(file.path(gtfs, "calendar_dates.txt"))
trips  <- read_csv(file.path(gtfs, "trips.txt"))
routes <- read_csv(file.path(gtfs, "routes.txt"))
stop_times <- read_csv(file.path(gtfs, "stop_times.txt"))

april_trips <- calendar_dates |>
  mutate(date = ymd(date)) |> 
  filter(month(date) == 4) |>
  inner_join(trips, by = "service_id") |>
  inner_join(routes |> filter(route_desc == "Stadsbuss") |> select(route_id), by =
               "route_id")

departures <- april_trips |>
  inner_join(stop_times |> filter(stop_sequence == 1) |> select(trip_id, departure_time), by = "trip_id") |>
  mutate(
    hour = hour(departure_time),
    weekday = wday(date, label = TRUE),
    day = if_else(weekday %in% c("Sat", "Sun"), "weekend", "weekday"),
    day = fct_relevel(day, "weekend")
  )

by_hour <- departures |>
  count(day, hour) |>
  summarise(.by = c(day, hour), mean_trips = mean(n))

ggplot(by_hour, aes(x = hour, y = mean_trips, fill = day)) +
  geom_col(width = 0.75, linewidth = 2, position = position_dodge()) +
  shadowtext::geom_shadowtext(data = . %>% filter(day == "weekend") %>% filter(mean_trips == max(mean_trips) | mean_trips == min(mean_trips)), aes(label = mean_trips, color = day), position = position_dodge(width = 0.6), hjust = -0.2, vjust =0, angle = 90, bg.color = clr_bg, family = f1, fontface = "bold", size = 5) +
  shadowtext::geom_shadowtext(data = . %>% filter(day == "weekday") %>% filter(mean_trips == max(mean_trips) | mean_trips == min(mean_trips)), aes(label = mean_trips, color = day), position = position_dodge(width = 0.6), hjust = -0.2, vjust = 1, angle = 90, bg.color = clr_bg, family = f1, fontface = "bold", size = 5) +
  scale_x_continuous(expand = 0.0025, breaks = seq(0, 23, 3), labels = scales::label_number(suffix = ":00")) +
  scale_y_continuous(expand = FALSE) +
  scale_color_manual(values = c(clr_accent2, clr_accent)) +
  scale_fill_manual(values = c(clr_accent2, clr_accent)) +
  coord_radial(inner.radius = 0.25, rotate_angle = TRUE) +
  guides(theta = guide_axis_theta(angle = 0)) +
  labs(
    title = "Uppsala's bus clock",
    subtitle = "Departures per hour, {.#E8A020 **weekday**} vs {.#3D4F9F **weekend**}",
    caption = "Source: UL GTFS data, 4–30 April 2026 · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    axis.text.x = element_text(margin = margin(t = 10), face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5),
    plot.subtitle = marquee::element_marquee(family = f1, size = 25, hjust = 0.5, margin = margin(t = 10, b = 50)),
    plot.caption = element_text(family = f1, size = 12, hjust = 0.5, margin = margin(t = 30, b = 6.5)),
    plot.margin = margin(t = 16, b = 34)
  )

record_polaroid()
  