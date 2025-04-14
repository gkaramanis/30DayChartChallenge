library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

vab <- readxl::read_xlsx(here::here("2025/data/TfpVabErsUtbLanKommun.xlsx"), skip = 2) %>%
  mutate(
    month = as.numeric(Månad),
    year = as.numeric(År),
    ndays = `Antal nettodagar`
    ) %>%
  filter(!is.na(ndays))

f1 <- "Outfit"
f2 <- "Domine"

pal <- MetBrewer::met.brewer("VanGogh3")

ggplot(vab, aes(x = month, y = ndays, group = year, color = year, label = year)) +
  geom_line(data = . %>% filter(year != 2020), linewidth = 0.5, alpha = 0.7) +
  # Highlight 2020
  geomtextpath::geom_textline(data = . %>% filter(year == 2020), color = "purple", linewidth = 1.5, hjust = 0.13) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_color_gradientn(colors = pal, breaks = c(2006, 2012, 2018, 2024)) +
  labs(
    title = "Seasonal trends and pandemic effect on Swedish VAB days",
    subtitle = str_wrap("Total net monthly days for childcare leave (VAB) follow a seasonal pattern, with peaks in February/March and lows in July. March 2020 saw a significant increase, nearly double the previous high, coinciding with the start of the COVID-19 pandemic.", 105),
    caption = "Source: Swedish Social Insurance Agency (Försäkringskassan) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.height = unit(0.5, "line"),
    legend.key.width = unit(2.5, "line"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text = element_text(color = "grey40"),
    plot.title = element_text(family = f2, size = 20, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, margin = margin(b = 20), lineheight = 1.1),
    plot.caption = element_text(hjust = 0, margin = margin(t = 15)),
    plot.margin = margin(10, 10, 10, 10)
  )

