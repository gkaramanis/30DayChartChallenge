library(tidyverse)
library(ggh4x)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 12, height = 8, dpi = 320)

# Exclusively or partially breastfed infants born 2021. By municipality
breastfeeding_raw <- readxl::read_xlsx(here::here("2025/data/2023-9-8758-tables.xlsx"), sheet = "8.1 Kommun, 2021", skip = 4)

colnames(breastfeeding_raw) <- c("municipality",
                                 "breastfeeding_2m", "breastfeeding_2m_pct", 
                                 "breastfeeding_4m", "breastfeeding_4m_pct",
                                 "breastfeeding_6m", "breastfeeding_6m_pct",
                                 "breastfeeding_8m", "breastfeeding_8m_pct",
                                 "breastfeeding_12m", "breastfeeding_12m_pct"
                                 )

breastfeeding_long <- breastfeeding_raw %>% 
  separate(municipality, into = c("municipality_code", "municipality"), sep = " ") %>% 
  mutate(across(breastfeeding_2m:last_col(), as.numeric)) %>% 
  pivot_longer(cols = breastfeeding_2m:last_col()) %>% 
  filter(str_detect(name, "_pct")) %>% 
  filter(nchar(municipality_code) > 2) %>% 
  mutate(
    name = fct_inorder(name),
    month = parse_number(as.character(name))
    )

# SKR municipality classification
muni_class <- readxl::read_xlsx(here::here("2025/data/Kommungruppsindelning-2023.xlsx"), skip = 1) %>% 
  mutate(
    Huvudgrupp = case_when(
      Huvudgrupp == "Storstäder och storstadsnära kommuner" ~ "Large cities and municipalities near large cities",
      Huvudgrupp == "Större städer och kommuner nära större stad" ~ "Medium-sized towns and municipalities near medium-sized towns",
      Huvudgrupp == "Mindre städer/tätorter och landsbygdskommuner" ~ "Smaller towns/urban areas and rural municipalities"
    ),
    `Kommungrupp 2023` = case_when(
      `Kommungrupp 2023` == "Storstäder" ~ "Large cities",
      `Kommungrupp 2023` == "Pendlingskommun nära storstad" ~ "Commuting municipalities near large cities",
      `Kommungrupp 2023` == "Större stad" ~ "Medium-sized towns",
      `Kommungrupp 2023` == "Pendlingskommun nära större stad" ~ "Commuting municipalities near medium-sized towns",
      `Kommungrupp 2023` == "Lågpendlingskommun nära större stad" ~ "Commuting municipalities with a low commuting rate near medium-sized towns",
      `Kommungrupp 2023` == "Mindre stad/tätort" ~ "Small towns",
      `Kommungrupp 2023` == "Pendlingskommun nära mindre tätort" ~ "Commuting municipalities near small towns",
      `Kommungrupp 2023` == "Landsbygdskommun" ~ "Rural municipalities",
      `Kommungrupp 2023` == "Landsbygdskommun med besöksnäring" ~ "Rural municipalities with a visitor industry"
    )
  ) %>% 
  add_count(Huvudgrupp, Gruppkod) %>%
  mutate(
    Gruppkod = fct_inorder(Gruppkod),
    Huvudgrupp = str_wrap(Huvudgrupp, 38),
    Huvudgrupp = fct_inorder(Huvudgrupp),
    `Kommungrupp 2023` = fct_inorder(`Kommungrupp 2023`),
    kommungrupp_label = str_wrap(paste0(`Kommungrupp 2023`, " (", n, ")"), 25),
    kommungrupp_label = fct_inorder(kommungrupp_label)
    )

country <- breastfeeding_long %>% 
  filter(str_detect(municipality_code, "RIKET"))

municipalities <- breastfeeding_long %>% 
  filter(!is.na(value) & !is.na(municipality)) %>% 
  left_join(muni_class, by = c("municipality_code" = "Kommunkod")) 

f1 <- "Fira Sans Compressed"
f2 <- "Familjen Grotesk"

pal <- c(
  line_grey = "grey60",
  smooth_fill = "#faa911",
  smooth_line = "#074a92",
  background = "#fbfcfc",
  axis_text = "grey40",
  text_dark = "grey20",
  facet_line = "#8B4513" 
)

strip_txt <- strip_nested(
  text_x = elem_list_text(
    size = 12,
    face = c("bold", "plain"),
    color = c(pal["axis_text"], pal["axis_text"])
  ),
  by_layer_x = TRUE
)

ggplot(municipalities, aes(month, value, group = municipality)) +
  geom_line(linewidth = 0.1, alpha = 0.7, color = pal["line_grey"]) +
  geom_smooth(aes(group = Gruppkod), fill = pal["smooth_fill"], color = pal["smooth_line"]) +
  scale_x_continuous(breaks = c(2, 12), minor_breaks = c(4, 6, 8)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), breaks = seq(0, 100, 20)) +
  facet_nested(~ Huvudgrupp + kommungrupp_label, nest_line = TRUE, strip = strip_txt) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Breastfeeding rates don't vary much by how urban a municipality is",
    subtitle = str_wrap("This chart shows the percentage of infants born in 2021 who were exclusively or partially breastfed at 2, 4, 6, 8, and 12 months old. The smoothed lines show the average trend for each subgroup, with the number of municipalities in parentheses. Data is missing for municipalities in several counties (Västernorrland, Dalarna, Västmanland, Örebro, Blekinge, and Södermanland).", 130),
    caption = "Source: Swedish National Board of Health and Welfare (Socialstyrelsen) & Swedish Association of Local Authorities and Regions (SALAR) · Graphic: Georgios Karamanis",
  ) +
  theme_minimal(base_family = f1) +
  theme(
    plot.background = element_rect(fill = pal["background"], color = NA),
    axis.title = element_blank(),
    axis.text = element_text(colour = pal["axis_text"]),
    strip.text = element_text(vjust = 0, colour = pal["text_dark"], size = 8.5, margin = margin(10, 0, 10, 0)),
    ggh4x.facet.nestline = element_line(color = pal["text_dark"], linewidth = 0.5),
    panel.spacing.x = unit(0.5, "lines"),
    plot.title = element_text(family = f2, face = "bold", colour = pal["text_dark"], size = 20),
    plot.subtitle = element_text(family = f2, colour = pal["text_dark"], size = 13),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_text(margin = margin(20, 0, 0, 0), hjust = 0, colour = pal["text_dark"])
  )

