library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

inpatient_raw <- readxl::read_xlsx("2025/data/downloaded/2024-9-9230-tables.xlsx", sheet = "Tabell 5", skip = 3)

inpatient <- inpatient_raw %>% 
  filter(`Antal patienter` == "Totalt") %>% 
  mutate(`Kön och ålder` = c(
    "Män", "Kvinnor", "Totalt",
    "Män per 100 000", "Kvinnor per 100 000", "Totalt per 100 000"
    )) 

inpatient_per_100k <- inpatient %>% 
  pivot_longer(`1988`:`2023`, names_to = "year", values_to = "patients_per_100000") %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(str_detect(`Kön och ålder`, "100 000"))

inpatient_100k_age <- readxl::read_xlsx("2025/data/downloaded/2024-9-9230-tables.xlsx", sheet = "Tabell 5", skip = 28) %>% 
  filter(`Antal patienter per 100.000 invånare` != "Totalt" & 
           !is.na(`Antal patienter per 100.000 invånare`)) %>%
  fill(`Kön och ålder`) %>%
  rename(age_group = `Antal patienter per 100.000 invånare`, sex = `Kön och ålder`) %>% 
  pivot_longer(`1988`:`2023`, names_to = "year", values_to = "patients_per_100000") %>%
  mutate(age_group = fct_inorder(age_group)) %>% 
  group_by(year, age_group) %>% 
  reframe(patients_per_100000 = sum(patients_per_100000)) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(age_group %in% c("15–24", "25–44", "45–64", "65–74"))

f1 <- "Georgia"
f2 <- "Radio Canada SemiCondensed"

ggplot() +
  geom_tile(data = inpatient_100k_age, aes(year, 0, fill = patients_per_100000)) +
  geom_vline(xintercept = c(1994.5, 2019.5), linetype = "dashed", color = "grey99") +
  scale_x_continuous(breaks = seq(1990, 2020, by = 5), expand = c(0.02, 0)) +
  MetBrewer::scale_fill_met_c("VanGogh3") +
  facet_wrap(vars(age_group), ncol = 1) +
  labs(
    title = "35 years of mental health hospitalizations in Sweden",
    subtitle = str_wrap("Each stripe represents yearly hospital discharge rates per 100 000 residents for mental health conditions. The 1995 mental healthcare reform shifted care to communities, reducing hospitalizations in older age groups. Meanwhile, young adult (15-24) admissions doubled, possibly due to better mental health awareness and advances in diagnosis. Vertical lines mark the 1995 reform and the 2020 COVID-19 pandemic.", 140),
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
    fill = "Cases per 100,000"
  ) +
  coord_cartesian(clip = "off") +
  theme_void(base_family = f1) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, margin = margin(b = 15), family = f2),
    plot.caption = element_text(size = 9, color = "grey40", margin = margin(t = 10), hjust = 0),
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(b = 10),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Patients per 100 000 residents
# Discharges from hospital
# Chapter V Mental and behavioural disorders (F00–F99)
# ICD10 since 1997, translations previous years may not be perfect