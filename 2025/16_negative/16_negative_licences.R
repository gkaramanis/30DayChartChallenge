library(tidyverse)
library(geomtextpath)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 12, height = 8, dpi = 320)

# 'Table 1. Licences granted and Number of Practioners with Licence under 65 years od age,  categorised by Sex, 2019-2023
licences_raw_women <- readxl::read_xlsx(here::here("2025/data/2024-9-9198-tables.xlsx"), sheet = "1. Legitimationer 2019-2023", skip = 27, n_max = 22) %>% 
  mutate(sex = "women") %>% 
  rename(profession = 1) 

licences_raw_men <- readxl::read_xlsx(here::here("2025/data/downloaded/2024-9-9198-tables.xlsx"), sheet = "1. Legitimationer 2019-2023", skip = 50, n_max = 22) %>% 
  mutate(sex = "men") %>% 
  rename(profession = 1) 

licences <- bind_rows(licences_raw_women, licences_raw_men) %>% 
  select(1, sex, `2019` = 3, `2020` = 5, `2021` = 7, `2022` = 9, `2023` = 11) %>% 
  mutate(
    across(`2019`:`2023`, as.numeric),
    profession = str_remove(profession, "\\s*\\*+")
    ) %>%
  mutate(negative_trend = if_else(`2019` > `2023`, TRUE, FALSE)) %>%
  pivot_longer(`2019`:`2023`, names_to = "year", values_to = "licences") %>% 
  group_by(profession) %>% 
  mutate(total_licences = sum(licences, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(total_licences > 4000) %>% 
  mutate(profession = case_match(
    profession,
    "Apotekare" ~ "Apothecary",
    "Arbetsterapeut" ~ "Occupational Therapist",
    "Audionom" ~ "Audiologist",
    "Barnmorska" ~ "Midwife",
    "Biomedicinsk analytiker" ~ "Biomedical Analyst",
    "Dietist" ~ "Nutritionist",
    "Fysioterapeut" ~ "Physiotherapist",
    "Hälso- och sjukvårdskurator" ~ "Health and Medical Care Curator",
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
    "Tandläkare" ~ "Dentist"
  ))

f1 <- "Radio Canada SemiCondensed"
f2 <- "Sofia Sans Extra Condensed"


pal <- c("#DFDED5", "#336703", "#d71fa6", "#070707")

ggplot(licences, aes(x = year, y = licences, group = sex, color = negative_trend)) +
  geom_textline(aes(label = sex,
                    linetype = sex,
                    fontface = if_else(negative_trend, "bold", "plain"),
                    hjust = case_when(
                      profession %in% c("Physician", "Naprapath") & sex == "women" ~ 0.2,
                      profession %in% c("Physician", "Naprapath") & sex == "men" ~ 0.7,
                      TRUE ~ 0.5
                    )), 
                text_only = TRUE, 
                family = f2) +
  scale_x_discrete(breaks = c(2019, 2023)) +
  scale_y_log10(labels = scales::label_number()) +
  scale_linetype_manual(values = c("111111", "solid")) +
  scale_color_manual(values = c(pal[2], pal[3])) +
  facet_wrap(vars(str_wrap(profession, 15)), ncol = 10) +
  labs(
    title = "Trends in licensed health practitioners in Sweden",
    subtitle = "Number of licenced practioners under 65 years, 2019-2023. Professions with a {.#d71fa6 **negative**} trend (fewer in 2023 than 2019) are highlighted.",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis"
  ) +
  theme_bw(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = pal[1]),
    strip.text = element_text(color = pal[4], face = "bold"),
    axis.title = element_blank(),
    axis.text.x = element_text(family = f2, size = 10),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(size = 16, face = "bold", margin = margin(0, 0, 2, 0)),
    plot.subtitle = marquee::element_marquee(margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(margin = margin(10, 0, 0, 0))
  )

