library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

muni_care_raw <- readxl::read_xlsx(here::here("2025/data/2024-5-9113-tables.xlsx"), sheet = "4.Andel med åtgärd kommun", skip = 3)

muni_class <- readxl::read_xlsx(here::here("2025/data/Kommungruppsindelning-2023.xlsx"), skip = 1) 

muni_care <- muni_care_raw %>% 
  filter(!is.na(Namn)) %>% 
  mutate(across(3:last_col(), as.numeric)) %>% 
  pivot_longer(3:last_col(), names_to = "metric", values_to = "pct") %>%
  filter(str_detect(metric, "Samtliga|Total", negate = TRUE)) %>%
  filter(nchar(Kod) > 2) %>% 
  filter(str_detect(metric, "64", negate = TRUE)) %>%
  separate(metric, into = c("sex", "age_group"), sep = " ", remove = FALSE) %>% 
  left_join(muni_class, by = c("Kod" = "Kommunkod")) %>% 
  mutate(
    age_group = case_match(
      age_group,
      "65–79" ~ "65-79 years",
      "80–" ~ "80+ years"
    ),
    big_group = case_when(
      startsWith(Gruppkod, "A") ~ "Metropolitan municipalities",
      startsWith(Gruppkod, "B") ~ "Large cities and neighboring municipalities",
      startsWith(Gruppkod, "C") ~ "Small cities and rural municipalities"
    )) %>% 
  arrange(Gruppkod) %>% 
  mutate(big_group = fct_inorder(big_group))

muni_care %>% 
  group_by(age_group, big_group) %>% 
  reframe(
    median_pct = median(pct, na.rm = TRUE),
    min_pct = range(pct, na.rm = TRUE)[1],
    max_pct = range(pct, na.rm = TRUE)[2]
  )

f1 <- "Outfit"

ggplot(muni_care, aes(pct, fill = big_group, group = NA)) +
  ggdist::geom_weave(layout = "hex", alpha = 0.9, dotsize = 0.9, color = NA) +
  scale_fill_manual(
    values = c(
    "Metropolitan municipalities" = "#FF9E00", 
    "Large cities and neighboring municipalities" = "#B388FF",
    "Small cities and rural municipalities" = "#00BFA5"),
    guide = guide_legend(override.aes = list(size = 4.5))
    ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  facet_wrap(vars(age_group), ncol = 1) +
  labs(
    title = "Rural-urban divide in Swedish municipal healthcare",
    subtitle = str_wrap("Metropolitan areas have the lowest median rates (3.7% for ages 65-79, 20.5% for 80+), while both larger cities and rural municipalities show similar higher rates (~9% for 65-79, ~42% for 80+). The low metropolitan rates are driven by Stockholm County municipalities which don't provide home care in ordinary housing, combined with younger populations in urban areas.", 128),
    x = "Percentage of population receiving care",
    y = "Municipalities",
    caption = "Data: Swedish National Board of Health and Welfare & Swedish Association of Local Authorities and Regions · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = "#F5F5F5", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    strip.text = element_text(face = "bold", size = 13),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.box = "vertical",
    legend.margin = margin(10, 0, 10, 0),
    legend.spacing = unit(0, "pt"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11, margin = margin(0, 10, 0, 2)),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    plot.caption = element_text(color = "grey30", margin = margin(20, 0, 0, 0)),
    plot.margin = margin(10, 10, 10, 10)
  )