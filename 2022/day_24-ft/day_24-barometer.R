library(tidyverse)
library(camcorder)
library(cowplot)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 10, height = 7, units = "in", dpi = 320)

# Sheets with questions
# Highest elected position
# QC6R.10

# Colleague
# QC12R.11

# "Child-in-law"
# QC13R.11

countries <- data.frame(
  code = c("NL", "UK", "IE", "ES", "FR", "PL", "IT", "EL", "LT", "HU", "BG"),
  name = c("Netherlands", "UK", "Ireland", "Spain", "France", "Poland", "Italy", "Greece", "Latvia", "Hungary", "Bulgaria")
  )

readin_data <- function(sheet) {
  readxl::read_xls(here::here("2022/data/ebs_493_volume_A.xls.xls"), sheet = sheet, skip = 8) %>% 
  janitor::clean_names() %>% 
  filter(x1 == "Total 'Comfortable'") %>% 
  mutate(across(2:last_col(), ~as.numeric(.x) * 100)) %>% 
  pivot_longer(2:last_col(), names_to = "country", values_to = "pct") %>% 
  mutate(country = toupper(country)) %>% 
  filter(country %in% countries$code) %>% 
  mutate(country = fct_relevel(country, rev(countries$code)))
  }

politician <- readin_data("QC6R.10") %>% 
  mutate(person = "Politician")

colleague <- readin_data("QC12R.11") %>% 
  mutate(person = "Work colleague")

childinlaw <- readin_data("QC13R.11")  %>% 
  mutate(person = "Child in law")

all_people <- bind_rows(politician, colleague, childinlaw)

lines_df <- childinlaw %>% 
  left_join(colleague, by = "country")

f1 <- "Cambay"

p <- ggplot() +
  geom_segment(data = lines_df, aes(x = pct.x, xend = pct.y, y = country, yend = country), color = "grey85", size = 0.35) +
  geom_point(data = all_people, aes(x = pct, y = country, color = person), size = 5.5) +
  scale_x_continuous(breaks = seq(0, 90, 10), limits = c(0, 92), expand = expansion(add = c(1, 4))) +
  scale_y_discrete(labels = rev(countries$name)) +
  scale_color_manual(values = c("#8BD9E3", "#275295", "#DA688D")) +
  guides(color = guide_legend(byrow = TRUE)) +
  labs(
    title = "Acceptance of transgender people",
    subtitle = "% of people who say they would feel comfortable with a trans person as a …",
    caption = "Source: Eurobarometer 2019\nGraphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = c(0.82, 1.145),
    legend.text = element_text(size = 16, color = "grey40"),
    legend.spacing.y = unit(0.1, "line"),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#FFF1E0", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "#D8D7C7", size = 0.35),
    axis.title = element_blank(),
    axis.text = element_text(size = 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 20, margin = margin(10, 0, 0, 0)),
    plot.subtitle = element_text(size = 15.5, margin = margin(12, 0, 55, 0), color = "grey40"),
    plot.caption = element_text(size = 12, hjust = 0, margin = margin(20, 0, 0, 0), color = "grey40"),
    plot.margin = margin(10, 10, 10, 20)
  )

l <- ggplot() +
  geom_segment(aes(x = 0.2, xend = 1.1, y = 7, yend = 7), size = 5) +
  xlim(0, 10) +
  ylim(0, 7) +
  coord_cartesian(expand = FALSE) +
  theme_void()

ggdraw(p) +
  draw_plot(l)
  