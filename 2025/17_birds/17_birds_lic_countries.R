library(tidyverse)
library(treemapify)
library(scales)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

# 'Table 2.1 Licences granted in 2023 by Sex, Licence and Education in Sweden, EU/EFTA+Switzerland or other countries
countries_raw <- readxl::read_xlsx(here::here("2025/data/2024-9-9198-tables.xlsx"), sheet = "2.1 Legitmation, utb.land", skip = 4, n_max = 22)

countries <- countries_raw %>% 
  select(profession = "Kvinnor och män...1", Sweden = "Sverige", "EU/EFTA+Switzerland" = "EU28/EES+Schweiz, exkl Sverige", "Other country" = "Tredje land", total = "Totalt ***") %>% 
  mutate(ratio_se = Sweden / total) %>%
  pivot_longer(Sweden:`Other country`, names_to = "country", values_to = "n") %>% 
  mutate(
    ratio = n / total,
    profession = str_remove(profession, "\\s*\\*+"),
    profession_eng = case_match(
      profession,
      "Apotekare" ~ "Apothecary",
      "Arbetsterapeut" ~ "Occupational Therapist",
      "Audionom" ~ "Audiologist",
      "Barnmorska" ~ "Midwife",
      "Biomedicinsk analytiker" ~ "Biomedical Analyst",
      "Dietist" ~ "Nutritionist",
      "Fysioterapeut" ~ "Physiotherapist",
      "Hälso- och sjukvårdskurator" ~ "Health Care Counselor",
      "Kiropraktor" ~ "Chiropractor",
      "Logoped" ~ "Speech Therapist",
      "Läkare" ~ "Physician",
      "Naprapat" ~ "Naprapath",
      "Optiker" ~ "Optician",
      "Psykolog" ~ "Psychologist",
      "Psykoterapeut" ~ "Psychotherapist",
      "Receptarie" ~ "Pharmacist",
      "Röntgensjuksköterska" ~ "Radiology Nurse",
      "Sjuksköterska" ~ "Nurse",
      "Tandhygienist" ~ "Dental Hygienist",
      "Tandläkare" ~ "Dentist",
      "Ortopedingenjör" ~ "Orthopedic Engineer",
      "Sjukhusfysiker" ~ "Hospital Physicist"
    ),
    profession_eng = fct_reorder(profession_eng, ratio_se)
    )


f1 <- "Futura"

ggplot(countries, aes(area = n, fill = country)) +
  geom_treemap(color = NA) +
  geom_treemap_text(aes(label = if_else(country == "Sweden", paste0(str_wrap(profession_eng, 15), "\n", number(n)), as.character(number(n)))), colour = "#4A2B47", place = "centre", family = f1) +
  scale_fill_manual(values = c("#E0C568", "#f260a4", "#F9F9F9")) +
  facet_wrap(vars(profession_eng)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Flocking to Sweden",
    subtitle = "Number of licences granted in 2023 by country of education: Sweden, {.#E0C568 **EU/EFTA**}, and {.#f260a4 **other countries**}. Professions ordered by proportion educated outside Sweden.",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen)\nGraphic: Georgios Karamanis"
    ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#f4f4f4", color = NA),
    panel.background = element_rect(fill = NA, color = "#4A2B47", linewidth = 1),
    strip.text = element_blank(),
    panel.spacing = unit(0.5, "line"),
    plot.margin = margin(10, 15, 10, 15),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = marquee::element_marquee(width = 1, margin = margin(5, 0, 15, 0), lineheight = 1, size = 15.5),
    plot.caption = element_text(margin = margin(-30, 0, 10, 0), size = 10)
  )

