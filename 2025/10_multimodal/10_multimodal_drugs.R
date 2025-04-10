library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 7, height = 8, dpi = 320)

drug_files <- list.files(path = here::here("2025/data/"), pattern = "Statistikdatabasen_10_", full.names = TRUE)

drugs_raw <- read_delim(drug_files) %>% 
  janitor::clean_names()
  
drugs <- drugs_raw %>% 
  mutate(alder = fct_inorder(alder)) %>% 
  mutate(patienter_1000_invanare = str_replace(patienter_1000_invanare, "\\,", "\\.")) %>% 
  mutate(patienter_1000_invanare = as.numeric(patienter_1000_invanare)) %>%
  filter(!alder %in% c("0­85+")) %>% 
  group_by(lakemedel, alder) %>% 
  summarise(total_per_1000 = sum(patienter_1000_invanare)) %>% 
  ungroup() %>% 
  mutate(drug = case_match(
    lakemedel,
    "A06 Medel vid förstoppning" ~ "Drugs for constipation",
    "A07AA Antibiotika" ~ "Antibiotics",
    "R03 Medel vid obstruktiva luftvägssjukdomar" ~ "Drugs for obstructive airway diseases"
  ),
  age_group = str_replace(alder, "­", "-"),
  age_group = fct_inorder(age_group)
  )

f1 <- "Mona-Sans"

# Elegant palette:
pal <- c("#F0A500", "#D9BF77", "#A2C2E1")

ggplot(drugs, aes(x = age_group, y = total_per_1000, fill = drug)) +
  geom_col() +
  scale_fill_manual(values = pal) +
  facet_wrap(vars(drug), scale = "free_y", ncol = 1) +
  labs(
    title = "Common medications age patterns in Sweden",
    subtitle = "Patients per 1 000 residents in 2024, selected drug categories",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
    x = "Age group"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(5, 0, 0, 0)),
    axis.text.x = element_text(size = 7),
    panel.spacing.y = unit(2, "lines"),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.margin = margin(10, 20, 10, 20),
    plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 0.5)
  )
  
