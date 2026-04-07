library(tidyverse)
library(patchwork)
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

# https://nationellaemissionsdatabasen.smhi.se
# Greenhouse gas emissions, Uppsala municipality, by sector and transport mode

ghg_raw <- readxl::read_xlsx(here::here("2026/data/Kommunrapport_uppsala_vaxthusgaser_totalt.xlsx"), skip = 5, col_names = FALSE)

years <- as.integer(as.character(ghg_raw[1, 5:ncol(ghg_raw)]))

ghg <- ghg_raw[-c(1, 2), ] |>
  set_names(c("sektor", "undersektor", "lan", "kommun", as.character(years))) |>
  select(-lan, -kommun) |>
  pivot_longer(cols = -c(sektor, undersektor), names_to = "year", values_to = "emissions") |>
  mutate(year = as.integer(year), emissions = as.numeric(emissions) / 1e3)

# Total emissions, % change from 1990 baseline
total <- ghg |>
  filter(sektor == "Alla", undersektor == "Alla") |>
  mutate(pct_change = (emissions - emissions[year == 1990]) / emissions[year == 1990] * 100)

# Emissions by sector
sectors <- ghg |>
  filter(undersektor == "Alla", sektor != "Alla") |>
  mutate(sektor_en = recode_values(
    sektor,
    "Transporter" ~ "Transport",
    "Utrikes transporter" ~ "Transport",
    "Jordbruk" ~ "Agriculture",
    "Arbetsmaskiner" ~ "Machinery",
    "El och fjärrvärme" ~ "Electricity & heat",
    "Industri (energi + processer)" ~ "Industry",
    "Egen uppvärmning av bostäder och lokaler" ~ "Heating",
    "Produktanvändning (inkl. lösningsmedel)" ~ "Products",
    "Avfall (inkl.avlopp)" ~ "Waste"
  )) |> 
  mutate(sektor_en = fct_lump_n(sektor_en, w = emissions, n = 3)) |> 
  group_by(year, sektor_en) |> 
  summarise(emissions = sum(emissions), .groups = "drop") |> 
  mutate(sektor_en = fct_relevel(sektor_en, c("Electricity & heat", "Transport", "Agriculture", "Other")))

p1 <- ggplot(total, aes(year, pct_change)) +
  geom_hline(yintercept = 0, color = clr_grid, linewidth = 0.6) +
  geom_area(fill = clr_accent, alpha = 0.15) +
  geom_line(color = clr_accent, linewidth = 1.2) +
  geom_point(color = clr_accent, size = 2.5) +
  scale_x_continuous(expand = FALSE) +
  scale_y_continuous(breaks = c(-40, -20, 0, 10), labels = scales::label_percent(scale = 1), position = "right") +
  labs(
    title = "Uppsala emissions down",
    subtitle = "Change in total and breakdown by sector"
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    axis.text.x = element_text(margin = margin(t = 10, b = 0))
  )

p2 <- ggplot(sectors, aes(year, emissions, fill = sektor_en)) +
  geom_area(position = "stack", alpha = 0.9) +
  scale_x_continuous(expand = FALSE) +
  scale_y_continuous(breaks = seq(0, 1000, 250), labels = scales::label_number(suffix = " kt"), position = "right") +
  theme_void(base_family = f1, base_size = 14) +
  scale_fill_manual(values = c(clr_accent, clr_accent2, clr_text2, clr_grid)) +
  guides(fill = guide_legend(title = NULL, nrow = 1, byrow = TRUE, override.aes = list(alpha = 1))) +
  theme(
    axis.text.x = element_blank()
  )

# Join plots
p1 / p2 +
  plot_annotation(
    caption = "Source: Nationella emissionsdatabasen, SMHI · Graphic: Georgios Karamanis"
  ) &
  coord_cartesian(clip = "off") &
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.7, "line"),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5),
    plot.subtitle = element_text(family = f1, size = 25, hjust = 0.5, margin = margin(t = 10, b = 50)),
    plot.caption = element_text(family = f1, size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    axis.text = element_text(color = clr_text2),
    axis.text.y = element_text(hjust = 0, size = 10, margin = margin(l = 5)),
    plot.margin = margin(t = 10, l = 25, r = 20, b = 18)
  )
