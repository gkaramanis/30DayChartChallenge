library(tidyverse)
library(marquee)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 8, height = 8, dpi = 320)

lung_cancer_raw <- xlsx::read.xlsx(here::here("2025/data/2024-10-9300-tables.xlsx"), sheetName = "Tabell 5. Lungcancer", startRow = 30) %>% 
  janitor::clean_names()

lung_cancer <- lung_cancer_raw %>% 
  mutate(year = parse_number(ar)) %>% 
  filter(!is.na(year)) %>% 
  select(year, starts_with(c("individer", "doda"))) %>% 
  filter(year == 1973 | year == 2023) %>% 
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>% 
  pivot_wider(names_from = year) %>% 
  separate(variable, into = c("type", "sex"), sep = "_") %>% 
  mutate(
    type = if_else(type == "individer", "diagnosed", "deceased"),
    sex = if_else(sex == "kvinnor", "Women", "Men")
  )

f1 <- "Domine"
f2 <- "Inter Display"

ggplot(lung_cancer, aes(x = 1973, xend = 2023, y = `1973`, yend = `2023`, color = sex, linetype = type, label = paste(sex, type))) +
  geomtextpath::geom_textsegment(linewidth = 2, family = f2, size = 5, hjust = 0.45) +
  geom_point(size = 5) +
  geom_point(data = . %>% select(type, sex, `2023`), aes(x = 2023, y = `2023`), size = 5) +
  scale_x_continuous(breaks = c(1973, 2023), minor_breaks = NULL, expand = c(0.2, 0)) +
  scale_y_continuous(limits = c(9, 50)) +
  scale_color_manual(values = c("#E96E4E", "#405D93")) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14, base_family = f1) +
  labs(
    title = "Women's lung cancer rates now exceed men's in Sweden",
    subtitle = "In 1973, {.#E96E4E **men**} had nearly 4x higher rates than {.#405D93 **women**} for both diagnosis (40.0 vs 10.5) and deaths (38.4 vs 10.8) per 100 000. By 2023, {.#405D93 **women**} surpassed {.#E96E4E **men**} in both categories—48.6 vs 37.1 for diagnosis and 37.1 vs 30.3 for mortality.",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
    y = "per 100 000",
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, margin = margin(0, 0, 0, 0)),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_marquee(margin = margin(0, 0, 30, 0), lineheight = 1.1, family = f2, width = 1.05),
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 30, 10, 30)
  )
