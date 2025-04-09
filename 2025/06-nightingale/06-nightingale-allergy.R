library(tidyverse)
library(shadowtext)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

allergy_raw <- readxl::read_xlsx(here::here("2025/data/Statistikdatabasen_2025-allergy.xlsx"), skip = 1) %>% 
  janitor::clean_names()

unique(allergy_raw$lakemedel)

allergy <- allergy_raw %>% 
  select(month = 1, drug = lakemedel, region, dispensed = expedieringar_1000_invanare) %>% 
  separate(month, into = c("month", "year")) %>% 
  filter(!is.na(drug)) %>% 
  mutate(
    region = str_remove(region, "s* län"),
    month = fct_relevel(month, c("Januari", "Februari", "Mars", "April", "Maj", "Juni", "Juli", "Augusti", "September", "Oktober", "November", "December"))
  ) %>% 
  group_by(month, region) %>% 
  summarise(
    dispensed_region = sum(dispensed)
  ) %>% 
  ungroup() %>% 
  filter(region != "Riket") 

pal <- MetBrewer::met.brewer("VanGogh3", 6)

f1 <- "General Sans"

ggplot(allergy, aes(month, dispensed_region, fill = dispensed_region)) +
  annotate("shadowtext", x = 8, y = 8, label = "8", family = f1, size = 3, color = "#C9C0B3", bg.color = "white") +
  annotate("shadowtext", x = 8, y = 6, label = "6", family = f1, size = 3, color = "#C9C0B3", bg.color = "white") +
  annotate("shadowtext", x = 8, y = 4, label = "4", family = f1, size = 3, color = "#C9C0B3", bg.color = "white") +
  annotate("shadowtext", x = 8, y = 2, label = "2", family = f1, size = 3, color = "#C9C0B3", bg.color = "white") +
  geom_col() +
  scale_fill_stepsn(colors = pal) +
  scale_x_discrete(labels = function(x) if_else(x == "Maj", "May", "")) +
  scale_y_continuous(breaks = seq(0, 8, 2), limits = c(0, 9)) +
  guides(fill = guide_colorsteps(direction = "horizontal", show.limits = TRUE)) +
  coord_radial() +
  facet_wrap(vars(region), ncol = 6) +
  labs(
    title = "Allergy medications dispensed in Sweden in 2023",
    subtitle = "As spring blooms in May, allergy prescriptions surge across Sweden, reaching their peak of 8.3 per 1 000 residents in Stockholm.\nWinter months see a dramatic drop to less than 1 prescription per 1 000 in most regions",
    fill = "dispensed medications per 1 000 inhabitants"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = c(0.75, 0.125),
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, size = 10),
    legend.key.width = unit(2, "lines"),
    legend.key.height = unit(0.5, "lines"),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "#E5E1D9"),
    strip.text = element_text(face = "bold", margin = margin(5, 0, -5, 0)),
    plot.background = element_rect(fill = "#FEFBF3", color = NA),
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(margin = margin(5, 0, 10, 0), lineheight = 1),
    plot.margin = margin(10, 10, 10, 10)
  )

