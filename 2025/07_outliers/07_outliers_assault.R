library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 9, height = 8, dpi = 320)

# Yttre orsaker 2024-9-9232-tables.xlsx
# Table 16i. Assault (X85–Y09), by home county and sex, 2023, number of patients per 100,000 inhabitants discharged from hospital

external_causes <- readxl::read_xlsx(here::here("2025/data/2024-9-9232-tables.xlsx"), sheet = "16. Yttre orsaker - län", skip = 219, n_max = 22)

external_causes_long <- external_causes %>% 
  pivot_longer(2:last_col(), names_to = "variable") %>% 
  filter(!is.na(value)) %>% 
  janitor::clean_names() %>% 
  filter(str_detect(variable, "100 000"))

country <- external_causes_long %>% 
  filter(hemlan == "Riket") %>% 
  select(country = value, variable)

regions <- external_causes_long %>% 
  filter(hemlan != "Riket") %>% 
  left_join(country) %>% 
  mutate(
    difference = value - country,
    variable = case_when(
      str_detect(variable, "Kvinnor") ~ "Women",
      str_detect(variable, "Män") ~ "Men",
      str_detect(variable, "Totalt") ~ "Total"
    ),
    variable = fct_inorder(variable)
    )

pal <- MetBrewer::met.brewer("Cassatt1", direction = -1) %>% 
  colorspace::darken(., 0.1) %>% 
  colorspace::desaturate(., -0.5)

f1 <- "Poppins"

ggplot(regions) +
  geom_col(aes(0, difference, fill = difference), width = 1) +
  geom_tile(aes(0, 0), height = 0.1) +
  scale_fill_stepsn(colors = pal, breaks = seq(-15, 15, 5), limits = c(-15, 15)) +
  scale_y_continuous(limits = c(-14, 14)) +
  guides(fill = guide_colorsteps(show.limits = TRUE)) +
  coord_flip(expand = FALSE) +
  facet_grid(vars(hemlan), vars(variable)) +
  labs(
    title = "Which Swedish counties stand out in assault-related hospitalizations?",
    subtitle = "Each bar shows how county rates differ from Sweden's national average per 100 000 for assault-related hospitalizations in 2023 (5.9 women, 16.6 men, 11.3 total). {.#b1615c **Higher rates**} extend right, {.#5a5a83 **lower rates**} extend left.",
    caption = "Source: National Board of Health and Welfare; Assault (X85–Y09), by home county and sex, 2023,\nnumber of patients per 100 000 inhabitants discharged from hospital · Graphic: Georgios Karamanis",
    fill = "Difference from national average per 100 000",
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "top",
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "grey50", size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#3F4F4F", linewidth = 0.2),
    panel.grid.minor.y = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    strip.text.x = element_text(color = "white", face = "bold"),
    strip.text.y = element_text(angle = 0, hjust = 0, color = "white"),
    strip.background = element_rect(fill = "#2F394D"),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = marquee::element_marquee(width = 1.1),
    plot.caption.position = "plot",
    plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 0),
    plot.margin = margin(10, 10, 10, 10)
  )
