library(rvest)
library(tidyverse)
library(camcorder)
library(ggforce)
library(ggnewscale)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 9, height = 10, units = "in", dpi = 320)

url1 <- "https://www.rainbowhealthontario.ca/TransHealthGuide/gp-femht.html"
url2 <- "https://www.rainbowhealthontario.ca/TransHealthGuide/gp-mascht.html"

read_plot_f <- function(url, tbl) {
  
  ht_tbl <- read_html(url) %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    .[[tbl]]
  
  ht <- ht_tbl %>% 
    janitor::clean_names() %>% 
    mutate(
      physical_effects = sub("b$|c$", "", physical_effects),
      physical_effects = paste0(physical_effects, "\n", reversibility),
      physical_effects = fct_rev(fct_inorder(physical_effects))
      ) %>% 
    select(-time_course_years) %>% 
    separate(expected_onset, sep = " - ", into = c("onset_from", "onset_to")) %>% 
    mutate(
      onset_to = parse_number(onset_to)
    ) %>% 
    separate(expected_max_effect, sep = " - ", into = c("max_from", "max_to")) %>% 
    separate(max_to, sep = " ", into = c("max_to", "unit")) %>% 
    mutate(
      max_from = if_else(str_detect(unit, "year"), as.numeric(max_from) * 12, as.numeric(max_to)),
      max_to = if_else(str_detect(unit, "year"), as.numeric(max_to) * 12, as.numeric(max_to)),
      across(starts_with("onset"), as.numeric),
      onset_from = replace_na(onset_from, 0),
      onset_to = replace_na(onset_to, 0),
      max_from = if_else(str_detect(physical_effects, "Thinned"), 3*12, max_from),
      max_from = if_else(str_detect(physical_effects, "Cessation"), onset_to, max_from),
      max_from = replace_na(max_from, 5*12),
      max_to = replace_na(max_to, 5*12),
      max_to = if_else(str_detect(physical_effects, "Cessation"), onset_to, max_to)
    ) %>% 
    select(-unit) %>% 
    rowwise() %>% 
    mutate(
      xmin = list(c(onset_from, onset_to)),
      xmax = list(c(onset_to, max_to))
    ) %>% 
    ungroup() %>% 
    unnest(c(xmin, xmax))
  
  f1 <- "Outfit"
  
  ggplot(ht) +
    geom_link2(aes(x = xmin, y = physical_effects), size = 6, color = "#CED094", lineend = "square") +
    geom_link2(aes(x = xmax, y = physical_effects, color = stat(index)), size = 6, lineend = "square") +
    scale_color_gradient(low = "#F2F3DA", high = c("#F2F3DA", "#9E88B3")) +
    new_scale_color() +
    geom_link2(data = ht %>% filter(max_from == 60 & max_to == 60), aes(x = xmax, y = physical_effects, color = stat(index)), size = 6, lineend = "square") +
    scale_x_continuous(breaks = 0:5 * 12, labels = function(x) {x/12}) +
    scale_color_gradient(low = "#F2F3DA", high = "#F1E7FF") +
    labs(
      x = "Time Course (years)",
      subtitle = "Reversibility, onset and maximum effects of physical changes",
      caption = "Source: Rainbow Health Ontario · Graphic: Georgios Karamanis"
    ) +
    theme_minimal(base_family = f1, base_size = 14) +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "grey97", color = NA),
      axis.title.y = element_blank(),
      axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      plot.subtitle = element_text(size = 12)
    )
}             

read_plot_f(url1, 3) +
  labs(title = "Feminizing Hormone Therapy")

read_plot_f(url2, 2) +
  labs(title = "Masculinizing Hormone Therapy")

