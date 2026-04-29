library(tidyverse)
library(camcorder)

gg_record(here::here("2026/temp/"), device = "png", height = 11.08, width = 8, units = "in", dpi = 320)

f1 <- "Metropolis"
f2 <- "Karst"

clr_bg <- "#FAFAF6"
clr_text <- "#1A1A18"
clr_text2 <- "#6B6B65"
clr_accent <- "#E8A020"

# Public transport boardings per offered km, all Swedish counties, 2024
# Source: Trafikanalys, Kollektivtrafik 2024

kt_raw <- read_csv(here::here("2026/data/trafikanalys_kollektivtrafik_alla_lan_2023_2024.csv"),
                   show_col_types = FALSE)

kt <- kt_raw |>
  filter(year == 2024) |>
  mutate(
    county = str_remove(county, "s*$"),
    efficiency = boardings_thousands / offered_km_thousands
  ) |>
  arrange(efficiency) |>
  mutate(county = fct_inorder(county))

ggplot(kt, aes(x = efficiency, y = county)) +
  geom_segment(aes(x = 0, xend = efficiency, yend = county, color = county == "Uppsala"), linewidth = 5) +
  marquee::geom_marquee(aes(x = efficiency + 0.2, color = county == "Uppsala", label = paste0("**", round(efficiency, 1), "** ", county)), size = 6, hjust = 0, family = f1) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.35))) +
  scale_color_manual(values = c("TRUE" = clr_accent, "FALSE" = clr_text2)) +
  labs(
    title = "Boardings per km of service",
    subtitle = "Swedish counties, public transport 2024",
    caption = "Source: Trafikanalys · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 24) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 34, color = clr_text, hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(t = 6, b = 50)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
