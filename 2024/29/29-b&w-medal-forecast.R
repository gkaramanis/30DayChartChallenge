library(tidyverse)
library(treemapify)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-29"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://www.nielsen.com/news-center/2024/virtual-medal-table-forecast/

vmt <- tribble(
  ~NOC, ~Gold, ~Silver, ~Bronze, ~Total,
  "United States",  39,      37,      47,     123,
  "China",           35,      30,      24,      89,
  "Great Britain",  13,      23,      30,      66,
  "France",          28,      19,       8,      55,
  "Australia",       13,      19,      18,      50,
  "Japan",           13,      16,      20,      49,
  "Italy",           12,      20,      15,      47,
  "Netherlands",     18,       5,      15,      38,
  "Germany",          9,      13,      14,      36,
  "Korea Republic",  9,       4,      11,      24
) %>% 
  mutate(rank = row_number())

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

ggplot(vmt, aes(area = Total, label = paste0(rank, ". ", NOC, "\n", Total))) +
  geom_treemap(aes(fill = NOC == "France"), color = "white", size = 10, start = "topleft", layout = "srow") +
  geom_treemap_text(color = "white", family = f1b, fontface = "bold", place = "centre", grow = TRUE, padding.x = unit(1, "line"), padding.y = unit(1, "line"), start = "topleft", min.size = 1, reflow = TRUE, layout = "srow") +
  scale_fill_manual(values = c("#0055A4", "#EF4135")) +
  labs(
    title = toupper("Paris 2024<br>medal forecast"),
    caption = toupper("Source: Gracenote Virtual Medal Table · Graphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = ggtext::element_textbox_simple(hjust = 0.5, size = 48, face = "bold", fill = "black", color = "white", halign = 0.5, padding = margin(8, 0, 0, 0), margin = margin(0, 4, 3, 4), lineheight = 0.9),
    plot.caption = ggtext::element_textbox_simple(hjust = 0.5, size = 10.9, face = "bold", fill = "black", color = "white", halign = 0.5, padding = margin(8, 0, 5, 0), margin = margin(0, 4, 0, 4))
  )
  
