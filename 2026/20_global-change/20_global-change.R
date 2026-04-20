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

# Annual new car registrations by fuel type, Uppsala municipality, 2010–2025
# Source: SCB Statistikdatabasen, PersBilarDrivMedel
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/

cars_raw <- read_csv(here::here("2026/data/scb_nyreg_bilar_drivmedel_Uppsala.csv"), locale = locale(encoding = "latin1"))

annual_share <- cars_raw |>
  pivot_longer(starts_with("Antal"), names_to = "ym", values_to = "n") |>
  mutate(
    ym = str_remove(ym, "^Antal "),
    year = as.integer(str_sub(ym, 1, 4)),
    month = as.integer(str_sub(ym, 6, 7))
  ) |>
  filter(year >= 2010, year <= 2025) |>
  mutate(
    fuel_group = case_when(
      drivmedel %in% c("el", "laddhybrid") ~ "Electric / PHEV",
      drivmedel == "elhybrid" ~ "Hybrid (non-plug)",
      drivmedel %in% c("bensin", "diesel") ~ "Petrol / Diesel",
      TRUE ~ "Other"
    )
  ) |>
  group_by(year, fuel_group) |>
  summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |>
  group_by(year) |>
  mutate(share = n / sum(n) * 100) |>
  ungroup() |>
  filter(fuel_group == "Electric / PHEV")

# EV share time series by entity, Source: Our World in Data / IEA
# https://ourworldindata.org/grapher/share-car-sales-battery-plugin
owid_raw <- read_csv(here::here("2026/data/owid_ev_share_timeseries.csv"))

owid <- owid_raw |>
  mutate(
    share = `Battery-electric` + `Plug-in hybrid`,
    entity = case_when(
      Entity == "European Union (27)" ~ "EU",
      TRUE ~ Entity
    )
  ) |>
  select(year = Year, entity, share)

end_labels <- bind_rows(
  owid |> filter(year == max(year)) |> select(year, entity, share),
  annual_share |> filter(year == max(year)) |> select(year, share) |> mutate(entity = "Uppsala")
) 

ggplot() +
  geom_line(data = owid, aes(x = year, y = share, color = entity), lineend = "round", linewidth = 2, alpha = 0.8) +
  geom_point(data = owid %>% filter(year == max(year)), aes(x = year, y = share, color = entity)) +
  geom_line(data = annual_share, aes(x = year, y = share, color = "Uppsala"), lineend = "round", linewidth = 3, alpha = 0.8) +
  geom_point(data = annual_share %>% filter(year == max(year)), aes(x = year, y = share, color = "Uppsala"), size = 6) +
  ggrepel::geom_text_repel(data = end_labels, aes(x = year, y = share, color = entity, label = glue::glue("{entity}\n{scales::percent(share, scale = 1)}")), family = f1, size = 7, lineheight = 0.95, min.segment.length = 10, fontface = "bold", bg.color = clr_bg, force_pull = 0, point.padding = 20) +
  scale_color_manual(values = c(Uppsala = clr_accent, Sweden = clr_accent2, EU = clr_text2, World = "#A0A09A")) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  scale_y_continuous(labels = scales::label_number(suffix = "%"), expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs(
    title = "Going electric",
    subtitle = "EV and plug-in hybrid share of new cars",
    caption = "Source: SCB (2010-2025), Our World in Data (2010-2024) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f2, base_size = 24) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    panel.grid.major.y = element_line(color = clr_grid, linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = clr_text2),
    axis.title = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 20)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
