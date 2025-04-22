library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

vis_raw <- readxl::read_xlsx(here::here("2025/data/kallsystem_b620cd39-c736-4b45-be3e-c7f5e091c262.xlsx")) 

digi <- vis_raw %>% 
  filter(str_detect(Titel, "digital|e-tjänster")) %>% 
  mutate(
    value = scales::rescale(Värde, from = c(0, 100), to = c(0, 5)),
    stars = round(value), 
    stars_label = case_when(
      stars == 0 ~ " ",
      stars == 1 ~ "★☆☆☆☆",
      stars == 2 ~ "★★☆☆☆",
      stars == 3 ~ "★★★☆☆",
      stars == 4 ~ "★★★★☆",
      stars == 5 ~ "★★★★★"
    ),
    year = as.numeric(Mätperiod),
    Titel = case_when(
      Titel == "Positiv till digital teknik för vård, konsultation och behandling" ~ "Positive towards digital technology for care, consultation and treatment",
      Titel == "Positiv till att vårdas hemma med hembesök och stöd av digital teknik" ~ "Positive towards home care with home visits and support from digital technology",
      Titel == "Positiv till 1177:s e-tjänster" ~ "Positive towards 1177's e-services"
    ),
    title_lab = fct_reorder(str_wrap(Titel, 45), -stars)
  ) 

annot <- data.frame(
  x = 2021,
  y = c(69, 67),
  label = c(
    "Counties",
    "National average"
  ),
  title_lab = unique(digi$title_lab)[3]
)

f1 <- "Proxima Nova"
f2 <- "Graphik Compact"

ggplot(digi, aes(x = year, y = Värde, group = Enhetsnamn)) +
  geom_line(data = . %>% filter(Enhetsnamn != "Riket"), color = "#cfd5ce") +
  geom_line(data = . %>% filter(Enhetsnamn == "Riket"), color = "black") +
  geom_text(data = . %>% filter(Mätperiod == max(Mätperiod)), aes(label = stars_label), nudge_y = -1.5, nudge_x = -0.2) +
  geom_text(data = annot, aes(x, y, label = label), inherit.aes = FALSE, hjust = 0, family = f2) +
  geom_segment(data = annot, aes(x - 0.2, xend = 2020, y), inherit.aes = FALSE, color = c("#cfd5ce", "black")) +
  scale_color_manual(values = c("grey90", "black")) +
  scale_x_continuous(breaks = 2020:2024, labels = c(2020, "", "", "", 2024), expand = c(0, 0.5)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  coord_cartesian(clip = "off") +
  facet_wrap(vars(title_lab)) +
  labs(
    title = "Swedes most positive towards 1177's e-services, less so towards digital technology for care",
    subtitle = str_wrap(
      "Four out of five are positive to 1177's e-services (Sweden's national health portal), while only two out of five are positive to digital technology for care, consultation and treatment. Positivity towards home care with digital support is in between. Support for all three indicators rose in 2021 during the pandemic, but only support for 1177's e-services remained high after it.",
      130
    ),
    caption = "Source: Vården i siffror · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f4f4f4", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(color = "#869798"),
    axis.ticks.length.x = unit(0.4, "line"),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#fffffe", color = "#869798"),
    strip.text = element_text(size = 10.5, face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12.5, margin = margin(0, 0, 20, 0), lineheight = 1),
    plot.caption = element_text(margin = margin(15, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )

