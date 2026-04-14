library(tidyverse)
library(eurostat)
library(ggcirclepack)
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

# Road freight loaded in Uppsala county by goods type
# Source: Eurostat, National road freight transport by NUTS3 region of loading and type of goods
# https://ec.europa.eu/eurostat/databrowser/view/road_go_na_rl3g

freight_raw <- get_eurostat("road_go_na_rl3g", filters = list(geo = "SE121"))

nst07_labels <- c(
  GT01 = "Agriculture\n& fish",
  GT02 = "Coal & crude oil",
  GT03 = "Mining &\nquarrying",
  GT04 = "Food &\nbeverages",
  GT05 = "Textiles & leather",
  GT06 = "Wood, paper\n& print",
  GT07 = "Coke &\npetroleum",
  GT08 = "Chemicals &\nplastics",
  GT09 = "Non-metallic\nminerals",
  GT10 = "Basic &\nfab. metals",
  GT11 = "Machinery &\nelectronics",
  GT12 = "Transport equipment",
  GT13 = "Furniture & other",
  GT14 = "Secondary\nraw materials",
  GT15 = "Waste &\nrecycling",
  GT16 = "Mail &\nparcels",
  GT17 = "Transport\nsupport",
  GT18 = "Removals &\nmisc.",
  GT19 = "Mixed goods",
  GT20 = "Other"
)

freight <- freight_raw |>
  filter(nst07 != "TOTAL", nst07 != "UNK", unit == "THS_T") |>
  filter(time == max(time)) |>
  filter(!is.na(values)) |>
  mutate(
    label = coalesce(nst07_labels[nst07], nst07),
    category = case_when(
      nst07 %in% c("GT03", "GT01", "GT06", "GT09") ~ "Raw & natural",
      nst07 %in% c("GT04", "GT07", "GT08", "GT10", "GT11") ~ "Manufactured",
      TRUE ~ "Logistics & mixed"
    ),
    category = factor(category, levels = c("Raw & natural", "Manufactured", "Logistics & mixed"))
  )

ggplot(freight, aes(id = label, area = values, fill = category, label = paste0(label, "\n", scales::number(values)))) +
  geom_circlepack(alpha = 0.85, color = NA) +
  geom_circlepack_text(family = f1, fontface = "bold", color = clr_bg, vjust = 0.55) +
  scale_fill_manual(values = c("Raw & natural" = clr_text, "Manufactured" = clr_accent2, "Logistics & mixed" = clr_accent)) +
  scale_size_area(max_size = 11) +
  # coord_fixed() +
  labs(
    title = "Goods on the move",
    subtitle = "Road freight loaded in Uppsala County, th. tonnes",
    caption = "Source: Eurostat, road_go_na_rl3g (2024) · Graphic: Georgios Karamanis"
    ) +
  guides(
    fill = guide_legend(nrow = 1, reverse = TRUE),
    size = "none"
    ) +
  theme_void(base_family = f1, base_size = 24) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, color = clr_text, margin = margin(l = 5)),
    legend.key.spacing.x = unit(2, "lines"),
    plot.background = element_rect(fill = clr_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, color = clr_text, hjust = 0.5, margin = margin(t = 20, b = 6)),
    plot.subtitle = element_text(color = clr_text2, hjust = 0.5, margin = margin(b = 65)),
    plot.caption = element_text(size = 12, color = clr_text2, hjust = 0.5, margin = margin(t = 50)),
    plot.margin = margin(t = 20, r = 20, b = 16, l = 20)
  )

record_polaroid()
