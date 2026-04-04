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

# https://www.trafa.se/globalassets/statistik/bantrafik/punktlighet-pa-jarnvag/2026/punktlighet-pa-jarnvag-2025-tabellverk.html
# Tabell 6b. Punktlighet på järnväg. STM (+5) efter år och län. År 2025 och 2024

uppsala_delays <- readxl::read_xlsx(here::here("2026/data/punktlighet-på-järnväg-stm-årspublicering-efter-år-och-län.xlsx"), skip = 9, n_max = 1, col_names = FALSE) |> 
  pivot_longer(cols = 2:last_col(), names_to = "metric", values_to = "value") |> 
  select(value)

col_names <- readxl::read_xlsx(here::here("2026/data/punktlighet-på-järnväg-stm-årspublicering-efter-år-och-län.xlsx"), skip = 6, n_max = 1) |> 
  # merge the two rows into one for each column
  pivot_longer(cols = everything(), names_to = "metric", values_to = "year") |> 
  filter(!is.na(year)) |> 
  mutate(metric = str_remove(metric, "\\.{3}[\\d]+")) 

upp_del <- bind_cols(col_names, uppsala_delays) |> 
  mutate(value = round(value, 1))

upp_del |> 
  filter(str_detect(metric, "Andel framförda|högst 5")) |> 
  ggplot(aes(x = year, y = value, group = metric, color = metric)) +
  geom_line(linewidth = 2.5) +
  geom_point(size = 5) +
  geom_text(aes(label = value), size = 8, family = f1, fontface = "bold", nudge_y = 0.7) +
  geom_text(data = NULL, aes(x = "2024", y = 97, label = "% of trains operated"), nudge_x = 0.5, color = clr_accent2) +
  geom_text(data = NULL, aes(x = "2024", y = 89, label = "% arriving within 5 min of schedule"), nudge_x = 0.5, color = clr_accent) +
  scale_color_manual(values = c(clr_accent, clr_accent2)) +
  labs(
    title = "Uppsala trains on track",
    subtitle = "More running, more on time",
    caption = "Source: Trafikanalys · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1, base_size = 24) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = clr_bg, color = NA),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.title.position = "plot",
    plot.title = element_text(family = f2, face = "bold", size = 36, hjust = 0.5),
    plot.subtitle = element_text(size = 25, hjust = 0.5, margin = margin(t = 10, b = 50)),
    plot.caption = element_text(size = 12, hjust = 0.5, margin = margin(t = 30, b = 10)),
    plot.margin = margin(t = 20, l = 20, r = 20, b = 18)
  )

record_polaroid()
