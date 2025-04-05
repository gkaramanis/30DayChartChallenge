library(tidyverse)
library(geofacet)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 12, height = 8, dpi = 320)

# Table 1. Regional distribution of employed dentists, doctors, midwives,
# nurses, pharmacists, psychologists and physiotherapists.
# Per 100 000 inhabitants, 1 november 2022

healthcare_workforce <- tribble(
  ~county, ~dentists, ~doctors, ~midwives, ~nurses, ~pharmacists, ~psycholog., ~physiother.,
  "Stockholm", 76, 361, 68, 1043, 29, 126, 72,
  "Uppsala", 79, 515, 90, 956, 57, 93, 84,
  "Östergötland", 76, 462, 63, 1125, 27, 137, 84,
  "Kronoberg", 69, 332, 59, 1195, 19, 84, 74,
  "Gotland", 81, 468, 95, 1218, 15, 76, 70,
  "Skåne", 79, 429, 65, 1052, 32, 69, 63,
  "Södermanland", 72, 369, 79, 33, 15, 64, 71,
  "Jönköping", 69, 368, 66, 1212, 31, 52, 71,
  "Kalmar", 81, 350, 85, 1182, 30, 52, 70,
  "Blekinge", 71, 373, 86, 1347, 64, 47, 70,
  "Halland", 78, 338, 69, 1073, 52, 64, 74,
  "Värmland", 70, 350, 76, 1179, 52, 76, 90,
  "Västmanland", 63, 432, 64, 1064, 20, 78, 74,
  "Gävleborg", 72, 305, 90, 1275, 37, 103, 62,
  "Jämtland", 70, 419, 65, 1190, 26, 97, 62,
  "Norrbotten", 62, 342, 85, 1207, 17, 57, 78,
  "Västra Götaland", 84, 425, 83, 1127, 37, 103, 84,
  "Örebro", 74, 419, 90, 1275, 29, 97, 74,
  "Dalarna", 62, 342, 74, 1190, 27, 57, 62,
  "Västernorrland", 62, 342, 78, 1175, 14, 63, 62,
  "Västerbotten", 87, 526, 74, 1433, 15, 108, 87,
  "Riket", 78, 414, 77, 1103, 36, 93, 78
)

hw_long <- healthcare_workforce %>% 
  pivot_longer(cols = -county, names_to = "profession", values_to = "count") %>% 
  group_by(profession) %>%
  arrange(-count) %>%
  mutate(rank = row_number()) %>% 
  ungroup()

# Rotate Sweden 90 degrees
se_counties_rotated <- se_counties_grid2 %>% 
  as_tibble() %>% 
  mutate(
    x = as.integer(row),
    y = as.integer(max(col) + 1 - col)
  ) %>% 
  select(code, name, col = x, row = y) %>% 
  mutate(row = ifelse(row == 1, row, row - 1))

pal <- MetBrewer::met.brewer("Johnson", n = 10, direction = -1)

f1 <- "Source Serif Pro"
f2 <- "Radio Canada Condensed"

ggplot(hw_long, aes(profession, 21 - rank, label = rank)) +
  geom_hline(yintercept = 21 - 10, color = "grey70") +
  geom_segment(aes(yend = 0), color = "grey30", linetype = "dotted") +
  geom_point(aes(fill = if_else(rank <= 10, rank, NA)), size = 5, shape = 21) +
  geom_text(data = . %>% filter(rank <= 10), color = "white", size = 3.3, vjust = 0.45, family = f2, fontface = "bold") +
  scale_x_discrete(limits = rev) +
  scale_fill_gradientn(colors = pal, na.value = "grey90") +
  coord_flip(clip = "off") +
  facet_geo(vars(county), grid = se_counties_rotated) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  labs(
    title = "Where are Sweden's healthcare professionals?",
    subtitle = str_wrap("Ranking of Swedish counties by number of healthcare professionals per 100 000 inhabitants. Each county shows how it ranks (1-21) for seven different professions, with highlighted top 10 positions. Västerbotten leads with the highest number of nurses, dentists and doctors, while Uppsala ranks lower for nurses despite high numbers of other medical professionals. Data from November 2022.", 140),
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.background = element_rect(fill = NA, color = "grey70"),
    panel.spacing = unit(0.7, "lines"),
    strip.text = element_text(margin = margin(0, 0, 3, 0), family = f1, face = "bold", size = 11),
    axis.text.y = element_text(family = f2, hjust = 1, margin = margin(0, 5, 0, 0), size = 9),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(margin = margin(3, 0, 10, 0), size = 12),
    plot.caption.position = "plot",
    plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 0, size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )
  
record_polaroid()
