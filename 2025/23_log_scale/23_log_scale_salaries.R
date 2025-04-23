library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 8, height = 8, dpi = 320)

# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0104__AM0104A/Statlig2gSSYK412/
# Genomsnittlig månadslön inom statlig sektor, medellön efter Yrke (SSYK 2012), kön och år

salaries_raw <- read_csv("~/Desktop/00000052_20250423-073904.csv", locale = locale(encoding = "latin1")) 

salaries <- salaries_raw %>% 
  rename(profession = 1, sex = 2) %>% 
  filter(str_detect(profession, "chefer")) %>%
  mutate(across(3:last_col(), as.numeric)) %>% 
  pivot_longer(3:last_col(), names_to = "year", values_to = "salary") %>%
  mutate(year = as.numeric(year)) %>% 
  pivot_wider(values_from = salary, names_from = sex) %>% 
  mutate(
    w_ratio = kvinnor / män,
    profession_en = case_when(
      str_detect(profession, "Klinik- och verksamhetschefer") ~ "Clinical and operations managers, level 1",
      str_detect(profession, "Avdelnings- och enhetschefer") ~ "Department and unit managers, level 2",
      TRUE ~ profession
    )
  )

f1 <- "Rowan"
f2 <- "Poppins"

ggplot(salaries, aes(year, w_ratio, color = profession_en)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(size = 3) +
  # Annotations
  annotate("label", x = 2018, y = 1, label = "Equal pay", family = f2, colour = "grey30") +
  annotate("label", x = 2018, y = 1.1, label = "Women earn more", family = f2, colour = "grey30") +
  annotate("label", x = 2018, y = 0.75, label = "Men earn more", family = f2, colour = "grey30") +
  annotate("segment", x = 2018, xend = 2018, y = 0.98, yend = 0.77, arrow = arrow(length = unit(0.18, "cm")), colour = "grey30") +
  annotate("segment", x = 2018, xend = 2018, y = 1.02, yend = 1.07, arrow = arrow(length = unit(0.18, "cm")), colour = "grey30") +
  scale_color_manual(
    values = c(
      "Clinical and operations managers, level 1" = "#0072B2",
      "Department and unit managers, level 2" = "#D55E00"
    )
  ) +
  scale_y_log10() +
  labs(
    title = "Women health care managers earn a lot less than men",
    y = "Salary ratio (women / men, logarithmic scale)",
    subtitle = str_wrap(
      "The salary ratio (women / men) shows that women have, in most years, had lower average monthly salaries in both senior and middle management roles in health care. In 2023, women earned 14 400 SEK less as clinical and operations managers and 18 600 SEK less as department and unit managers.", 78),
    caption = "Average monthly salary in the public sector by occupation (SSYK 2012), sex and year. Men's salaries for 2018 are missing\nSource: Statistics Sweden · Graphic: Georgios Karamanis"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
   theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    axis.text = element_text(family = f2, size = 12),
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(family = f2, lineheight = 1, size = 13),
    plot.caption = element_text(margin = margin(15, 0, 0, 0), size = 10),
    plot.margin = margin(10, 20, 10, 20)
  )

