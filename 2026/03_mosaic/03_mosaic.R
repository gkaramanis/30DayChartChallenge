library(tidyverse)
library(marimekko)
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


# https://www.nvdb.se/sv/kund/statistik/

# Raw data
km_car_raw <- readxl::read_xlsx(here::here("2026/data/antal-kilometer-bilnat-per-vaghallare-efter-lan-och-kommun.xlsx"), sheet = "2025")

m_bicycle_raw <- readxl::read_xlsx(here::here("2026/data/antal-meter-cykelnat-per-vaghallare-efter-lan-och-kommun.xlsx"), sheet = "2025")

# Uppsala kommun
km_car <- km_car_raw |> 
  filter(Kommun == "Uppsala") |> 
  select(Enskild:Statlig) |> 
  mutate(across(everything(), as.numeric)) |> 
  pivot_longer(cols = Enskild:Statlig, names_to = "type", values_to = "length_km") |>
  mutate(network = "car")

km_bicycle <- m_bicycle_raw |> 
  filter(Kommun == "Uppsala") |> 
  select(Enskild:Statlig) |>
  mutate(across(everything(), as.numeric)) |> 
  pivot_longer(cols = Enskild:Statlig, names_to = "type", values_to = "length_m") |> 
  mutate(
    network = "bicycle",
    length_km = length_m / 1e3
    ) |> 
  select(-length_m)

roads <- bind_rows(km_car, km_bicycle) |> 
  mutate(type = case_when(
    type == "Enskild" ~ "Private",
    type == "Kommunal" ~ "Municipal",
    type == "Region" ~ "Regional",
    type == "Statlig" ~ "State"
  )) |> 
  mutate(type = fct_inorder(type))

ggplot(roads) +
  geom_marimekko(formula = ~ network | type, aes(fill = type, weight = length_km)) +
  layer(
    stat = StatMarimekkoTiles,
    geom = GeomText,
    mapping = aes(
      label = scales::number(
        after_stat(weight),
        accuracy = 1,
        suffix =  " km",
      ),
      size = after_stat(weight),
      angle = if_else(after_stat(round(weight)) == 446, 90, 0)
    ),
    data = roads,
    position = "identity",
    inherit.aes = FALSE,
    params = list(color = "white", family = f2, fontface = "bold")
  ) +
  scale_fill_manual(values = c(clr_accent2, clr_accent, clr_text)) +
  scale_size_continuous(range = c(4, 28)) +
  labs(
    title = "Roads and cycle paths",
    subtitle = "Uppsala municipality, 2025",
    caption = "Source: NVDB · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 24) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    axis.text = element_text(),
    axis.text.x = element_text(hjust = c(0.35, 0.5), margin = margin(t = 5)),
    axis.text.y = element_text(angle = 90, margin = margin(r = 5)),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5),
    plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(t = 10, b = 30)),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    plot.margin = margin(t = 20, l = 20, r = 20, b = 18)
    )

record_polaroid()
