library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 8, height = 8, dpi = 320)

# Table 9.1 Prescribed drugs. Prevalence for selected groups of drugs 2006 ̶ 2023, patients per 1,000 inhabitants, women
drugs_w_raw <- readxl::read_xlsx(here::here("2025/data/2024-4-9027-tables.xlsx"), sheet = "9.1 Prevalens kv. 2006-2023", skip = 3) %>% 
  mutate(sex = "women")

# Table 10.1 Prescribed drugs. Prevalence for selected groups of drugs 2006 ̶ 2023, patients per 1,000 inhabitants, men
drugs_m_raw <- readxl::read_xlsx(here::here("2025/data/2024-4-9027-tables.xlsx"), sheet = "10.1 Prevalens män 2006-2023", skip = 3) %>% 
  mutate(sex = "men")
  
drugs <- bind_rows(drugs_w_raw, drugs_m_raw) %>%
  filter(!is.na(`2006`)) %>%
  mutate(across(`2006`:`2023`, as.numeric)) %>%
  pivot_longer(cols = `2006`:`2023`, names_to = "year", values_to = "value") %>%
  rename(atc_group = 1) %>%
  filter(str_detect(atc_group, "Antibiotika")) %>%
  mutate(year = as.numeric(year)) 

bg_col <- "grey99"
line_cols <- c("men" = "#E69F00", "women" = "#56B4E9") 
text_col <- "grey10"

ggplot(drugs, aes(x = year, y = value, group = sex, color = sex)) +
  annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.3) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_smooth(se = FALSE, linewidth = 1.5) +
  geom_text(data = filter(drugs, year == 2023), aes(label = str_to_title(sex)),  hjust = 1, nudge_x = -0.3, nudge_y = 8, size = 5, fontface = "bold") +
  scale_color_manual(values = line_cols) +
  scale_x_continuous(breaks = seq(2006, 2023, by = 2)) + 
  labs(
    title = "Swedish antibiotic prescriptions fall, gender gap persists",
    subtitle = "Patients per 1 000 inhabitants receiving at least one prescription decreased by around **37%** between 2006 and 2023, with a notable dip during the COVID-19 pandemic. Women consistently receive prescriptions at a higher rate; in 2023, the rate for women was approximately **50%** higher than for men.", 
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
    color = NULL
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = text_col),
    axis.title = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "Roboto Slab", size = 22, face = "bold", color = text_col, margin = margin(b = 3)),
    plot.subtitle = marquee::element_marquee(size = 14, color = text_col, margin = margin(b = 30), lineheight = 1.1, width = 0.98),
    plot.caption = element_text(size = 9, color = "grey50", hjust = 0, margin = margin(t = 15)),
    plot.margin = margin(15, 15, 15, 15) 
  )

