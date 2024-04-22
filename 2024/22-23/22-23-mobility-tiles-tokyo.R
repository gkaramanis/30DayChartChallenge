library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

mobility_raw <- read_csv("https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_asia_africa.csv")
  
mobility <- mobility_raw %>% 
  # janitor::clean_names() %>% 
  filter(country == "Japan") %>% 
  filter(between(date, as.Date("2021-07-17"), as.Date("2021-08-16"))) %>% 
  pivot_longer(`retail and recreation`:residential) %>% 
  filter(`sub region 1` == "Tokyo") %>% 
  mutate(name = fct_reorder(str_wrap(name, 10), -value))

scale_y_date2 <- function(..., rescaler) {
  y <- scale_y_date(...)
  y$rescaler <- rescaler
  y
}

invert_scale <- function(y, to = c(1, 0), from = range(y)) {
  scales::rescale(y, to, from)
}

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2b <- "Produkt Medium"

bg <- colorspace::desaturate("#C70025", 0.5)
col <- "grey99"

ggplot(mobility) +
  geom_tile(aes(name, date, fill = value), color = bg) +
  geom_hline(yintercept = c(as.Date("2021-07-24"), as.Date("2021-08-09")), linewidth = 1, linetype = "dotted") +
  colorspace::scale_fill_binned_diverging(palette = "Tropic", n_interp = 5, rev = TRUE) +
  scale_y_date2(rescaler = invert_scale, breaks = as.Date(c("2021-08-09", "2021-07-24")), date_labels = "%d %b") +
  coord_cartesian(expand = FALSE) +
  labs(
    title = toupper("Google Mobility data"),
    subtitle = toupper("before, during and after\nthe Tokyo 2020 games"),
    caption = toupper("Source: Google · Graphic: Georgios Karamanis"),
    fill = toupper("% change compared to baseline")
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, color = "white", family = f2b),
    legend.text = element_text(color = "white", family = f2b),
    legend.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = bg, color = NA),
    axis.title = element_blank(),
    axis.text = element_text(family = f1b, face = "bold", color = col, size = 10, lineheight = 0.8),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold", margin = margin(20, 0, 5, 0), color = col),
    plot.subtitle = element_text(hjust = 0.5, size = 24, face = "bold", color = col, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(10, 0, 20, 0), color = col, family = f2b),
    plot.margin = margin(0, 30, 0, 20),
    # plot.title.position = "plot"
  )
  
