library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 9, dpi = 320)

# 'Table 11b. External cause: Complication of medical and surgical care (Y40-Y84), by age, 2001-2023, number of patients per 100,000 inhabitants discharged from hospital
complications_raw <- readxl::read_xlsx(here::here("2025/data/2024-9-9232-tables.xlsx"), sheet = "11. Komplikationer", skip = 31, n_max = 23) %>% 
  rename(year = 1)

complications <- complications_raw %>% 
  pivot_longer(2:last_col(), names_to = "age_group", values_to = "value") %>% 
  mutate(
    age_group = if_else(age_group == "0-4", "0-4 years", age_group),
    age_group = fct_inorder(age_group)
    )

age_labels <- complications %>% 
  filter(year == max(year)) %>% 
  arrange(desc(age_group)) %>% 
  mutate(y = cumsum(value) - value / 2)

pal <- MetBrewer::met.brewer("Cross")

f1 <- "Outfit"
f2 <- "Outfit Black"

ggplot(complications, aes(x = year, y = value, fill = age_group)) +
  geom_area(aes(color = after_scale(colorspace::darken(fill, 0.5))), linewidth = 0.5) +
  ggrepel::geom_label_repel(data = age_labels, aes(x = year, y = y, label = age_group, segment.color = age_group), color = "white", fontface = "bold", family = f1, direction = "y", nudge_x = 2, point.padding = 0.5, segment.size = 1, hjust = 0) +
  scale_fill_manual(values = pal, aesthetics = c("fill", "segment.color")) +
  scale_x_continuous(breaks = seq(2001, 2023, 2), limits = c(2001, 2025)) +
  scale_y_continuous(labels = scales::number, expand = c(0.01, 0)) +
  labs(
    title = "Are medical and surgical complications becoming more frequent in Sweden?",
    subtitle = str_wrap("Swedish complication rates (per 100 000 inhabitants, by age, 2001-2023). Rates initially rose, fluctuated, dipped significantly in 2020 (COVID-19), then increased. Older adults consistently show higher rates. Trends reflect demographics, procedure volumes, diagnostics, and reporting standards.", 106),
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text = element_text(size = 10),
    axis.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", margin = margin(b = 5), size = 18),
    plot.subtitle = element_text(margin = margin(b = 40), size = 14, lineheight = 1),
    plot.caption = element_text(margin = margin(t = 15), size = 10),
    plot.margin = margin(10, 10, 10, 20)
  )
