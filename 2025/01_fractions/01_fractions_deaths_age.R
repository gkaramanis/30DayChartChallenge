library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 9, height = 8, dpi = 320)

deaths_age <- read_csv("https://ourworldindata.org/grapher/deaths-by-age-group.csv?v=1&csvType=filtered&useColumnShortNames=true&country=~SWE") %>% 
  janitor::clean_names()

deaths_age_long <- deaths_age %>% 
  pivot_longer(4:last_col(), names_to = "age_group", values_to = "deaths") %>% 
  mutate(
    age_group = str_remove(age_group, "deaths_sex_all_age_"),
    age_group = str_remove(age_group, "_variant_estimates_.+"),
    age_group = str_replace(age_group, "_", "-"),
    age_group = str_replace(age_group, "plus", "+"),
    age_group = case_when(
      # age_group == "0-9" ~ "0-19",
      # age_group == "10-19" ~ "0-19",
      age_group == "20-29" ~ "20-39",
      age_group == "30-39" ~ "20-39",
      age_group == "40-49" ~ "40-59",
      age_group == "50-59" ~ "40-59",
      age_group == "60-69" ~ "60-79",
      age_group == "70-79" ~ "60-79",
      age_group == "80-89" ~ "80-99",
      age_group == "90-99" ~ "80-99",
      TRUE ~ age_group
    ),
    age_group = fct_inorder(age_group)
  ) %>% 
  group_by(year, age_group) %>% 
  reframe(deaths = sum(deaths, na.rm = TRUE))

pal <- MetBrewer::met.brewer("Hokusai1", n = 7)

f1 <- "Caladea"

ggplot(deaths_age_long) +
  geom_bar(aes(x = year, y = deaths, fill = age_group), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal, guide = guide_legend(reverse = TRUE, nrow = 1)) +
  scale_x_continuous(breaks = seq(1950, 2020, 10), minor_breaks = seq(1950, 2020, 5)) +
  scale_y_continuous(labels = scales::percent_format(), sec.axis = dup_axis(), breaks = seq(0.2, 1, 0.2), minor_breaks = seq(0, 1, 0.1)) +
  coord_cartesian(expand = FALSE) +
  guides(
    x = guide_axis(minor.ticks = TRUE),
    y = guide_axis(minor.ticks = TRUE)
  ) +
  labs(
    title = "The shifting age of death in Sweden",
    subtitle = str_wrap("Swedish mortality patterns have changed significantly from 1950. Deaths among those aged 80-99 now make up the largest share (58%), while deaths of centenarians increased 35-fold. Child mortality (0-9) plummeted from 4.7% to 0.3%. The COVID-19 pandemic caused a notable spike in elderly deaths in 2020.", 104),
    caption = "Source: Our World in Data · Graphic: Georgios Karamanis",
    fill = NULL
  ) +
  theme_void(base_family = f1, base_size = 14) +
  theme(
    legend.position = "top",
    legend.text.position = "top",
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(10, 0, 5, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.ticks = element_line(color = "grey50"),
    axis.ticks.length = unit(0.1, "lines"),
    axis.minor.ticks.length = unit(0.1, "lines"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.margin = margin(10, 10, 10, 10)
  )

