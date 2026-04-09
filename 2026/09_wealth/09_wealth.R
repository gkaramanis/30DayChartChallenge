library(tidyverse)
library(sf)
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

cars_uppsala <- read_csv(here::here("2026/data/TAB6589_en.csv"), locale = locale(encoding = "latin1")) |>
  filter(str_detect(region, "Uppsala")) |> 
  filter(year == 2025) |> 
  filter(status == "in use") |> 
  rename(cars = `Passenger cars in use registered on population 31 December, number`)

income_uppsala <- read_csv(here::here("2026/data/uppsala_income.csv")) |>
  filter(str_detect(region, "Uppsala")) |>
  filter(tabellinnehåll == "Medianvärde, tkr") |>
  filter(inkomstslag == "nettoinkomst") |>
  filter(kön == "totalt") |>
  group_by(region) |>
  filter(år == max(år)) |>
  mutate(median_income = parse_number(`Andel av befolkningen i inkomstklass`)) |>
  filter(!is.na(median_income))

population_uppsala <- read_csv(here::here("2026/data/TAB6574_population_uppsala.csv")) |> 
  filter(population > 0)

cars_income <- cars_uppsala |>
  left_join(income_uppsala, by = "region") |>
  left_join(population_uppsala, by = "region") |>
  filter(str_starts(region, "03", negate = TRUE)) |>
  mutate(
    regso = str_remove_all(region, "Uppsala \\(|\\)"),
    cars_per_capita = cars / population
  )

hull_groups <- cars_income |>
  mutate(group = case_when(
    regso == "Kåbo-Norra Rosendal" ~ "Kåbo-Norra Rosendal",
    regso %in% c("Studentstaden", "Västra Flogsta") ~ "Student areas",
    str_detect(regso, "omland") ~ "6 peripheral areas",
    regso == "Gamla Gottsunda-Vårdsätra-Vreta" ~ "Highest income"
  )) |>
  filter(!is.na(group)) |>
  mutate(description = str_flatten(regso, collapse = "\n"), .by = group) |>
  mutate(description = case_when(
    group == "6 peripheral areas" ~ "Stadens omland",
    group == "Kåbo-Norra Rosendal" ~ NA_character_,
    .default = description
  )) |>
  mutate(
    x0 = case_when(
      group == "6 peripheral areas" ~ 350,
      TRUE ~ NA
    ),
    y0 = case_when(
      group == "6 peripheral areas" ~ 0.6,
      group == "Student areas" ~ 0.35,
      group == "Highest income" ~ 0.25,
      TRUE ~ NA
    )
  )

ggplot(cars_income, aes(x = median_income, y = cars_per_capita)) +
  ggforce::geom_mark_hull(
    data = hull_groups,
    aes(group = group, label = group, description = description, x0 = x0, y0 = y0),
    color = NA, fill = clr_accent, alpha = 0.5,
    label.family = f1, label.fontsize = 15, label.buffer = unit(20, "mm"),
    con.colour = clr_grid, con.size = 0.8,
    concavity = 10, expand = unit(7, "mm"),
  ) +
  geom_point(aes(size = population), alpha = 0.7, color = clr_accent2) +
  scale_size_area(max_size = 12) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Median net income in thousand kronor",
    y = "Cars in use per capita",
    title = "Wheels & wealth in Uppsala…",
    subtitle = "…mostly correlate, except for one district",
    caption = "Source: Statistics Sweden, 2025, income 2024, bubble size: population \nGraphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, hjust = 0.5),
    plot.subtitle = element_text(family = f1, size = 25, hjust = 0.5, margin = margin(t = 10, b = 50)),
    plot.caption = element_text(family = f1, size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(angle = 90, margin = margin(r = 8)),
    axis.text = element_text(color = clr_text2),
    plot.margin = margin(t = 25, l = 25, r = 20, b = 18)
  )

record_polaroid()

