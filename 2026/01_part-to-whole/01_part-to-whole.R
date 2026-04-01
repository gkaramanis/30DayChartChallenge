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

# https://www.sverigesmiljomal.se/miljomalen/frisk-luft/resvanor/
# Resvanor uppdelade på färdsätt, Uppsala län
# Distance traveled in thousand personkm

modal_raw <- readxl::read_xlsx(here::here("2026/data/Resvanor uppdelade på färdsätt och kön.xlsx"))

modal <- modal_raw |>
  filter(År == max(År)) |>
  pivot_longer(cols = 2:last_col(), names_to = "mode", values_to = "distance") |>
  mutate(
    pct = round(distance / sum(distance) * 100, 1),
    mode = case_when(
      mode == "Bil" ~ "Car",
      mode == "Allmän kollektivtrafik" ~ "Public transit",
      mode == "Cykel" ~ "Cycling",
      mode == "Till fots" ~ "Walking",
      mode == "Annat färdsätt" ~ "Other"
    )
  ) |>
  mutate(
    mode = fct_reorder(mode, pct, .desc = TRUE),
    label_y = cumsum(pct) - pct / 2,
    nudge = if_else(mode == "Walking", 0.1, 0)
  )

ggplot(modal, aes(x = 2, y = pct, fill = mode)) +
  geom_col(width = 1, color = clr_bg, linewidth = 0.8) +
  ggrepel::geom_text_repel( aes(x = 2, label = paste0(pct, "%")), position = position_stack(vjust = 0.5), family = f1, size = 10, color = clr_bg, fontface = "bold", bg.color = clr_text, seed = 0) +
  scale_x_continuous(limits = c(0.5, 2.5)) +
  scale_fill_manual(values = c(
    "Car" = clr_accent2,
    "Public transit" = clr_accent,
    "Cycling" = "#A0A098",
    "Walking" = clr_text2,
    "Other" = clr_grid
  )) +
  coord_polar(theta = "y", start = 0) +
  labs(
    title = "How Uppsala County moves",
    subtitle = paste0("Share of distance traveled by mode, ", modal$År[1]),
    caption = "Source: Sveriges Miljömål · Graphic: Georgios Karamanis"
  ) +
  guides(fill = guide_legend(title = NULL, nrow = 2, byrow = TRUE)) +
  theme_void(base_family = f1, base_size = 24) +
  theme(
    legend.position = "top",
    legend.key.width = unit(0.8, "lines"),
    legend.key.height = unit(1.5, "lines"),
    legend.key.spacing.x = unit(2, "lines"),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title = element_text(hjust = 0.5, family = f2, face = "bold", size = 36),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 10, b = 25)),
    plot.caption = element_text(hjust = 0.5, size = 12),
    plot.margin = margin(t = 20, b = 18)
  )

record_polaroid()

