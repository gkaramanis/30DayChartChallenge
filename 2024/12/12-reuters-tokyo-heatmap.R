library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-12/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

tokyo_weather <- read_csv(here::here("2024/data/tokyo-weather.csv"))


# https://www.reuters.com/graphics/OLYMPICS-2020/SUMMER-HEAT/bdwvkogrzvm/

# https://www.ncei.noaa.gov/cdo-web/search
# Custom GHCN-Daily CSV

tokyo_max <- tokyo_weather %>% 
  mutate(
    y = year(date),
    x = decimal_date(date) - y
    ) %>% 
  filter(name == "TOKYO, JA") %>% 
  filter(between(y, 1964, 2021))

tokyo_max <- jsonlite::fromJSON("~/Desktop/tokyo-tmax.json") %>% 
  mutate(x = day_of_365, y = year, tmax = max_temp)

pal <- MetBrewer::met.brewer("Hiroshige", direction = -1)

f1 <- "Source Sans 3"

x_breaks <- data.frame(breaks = seq(1, 365, length.out = 25)) %>%
  mutate(labels = ifelse(row_number() %% 2 == 0, month.abb[row_number() / 2], ""))
# 
annot <- tribble(
  ~start_date, ~end_date,
  as.Date("1964-10-10"), as.Date("1964-10-24"),
  as.Date("2021-07-23"), as.Date("2021-08-08")
  ) %>%
  mutate(
    ymin = year(start_date),
    xmin = yday(start_date),
    ymax = year(end_date),
    xmax = yday(end_date),
    xmid = xmin + (xmax - xmin) / 2
  )

ggplot(tokyo_max) +
  geom_tile(aes(x, y, fill = tmax),width = 1, height = 1, color = NA) +
  geom_rect(data = annot, aes(xmin = xmin, ymin = ymin - 0.5, xmax = xmax, ymax = ymax + 0.5, group = 1L), linewidth = 0.5, fill = NA, color = "black") +
  annotate("text", x = annot$xmid[1], y = annot$ymax[1] + 1.5, label = "1964", family = f1) +
  annotate("text", x = annot$xmid[2], y = annot$ymax[2] - 1.5, label = "2021", family = f1) +
  annotate("label", x = 0.5 * 365, y = 1993, label = "Hot and humid\nOlympic summer", family = f1, fontface = "bold", fill = alpha("white", 0.65), size = 8, lineheight = 0.9, label.size = 0, label.padding = unit(1.7, "lines"), label.r = unit(0, "lines")) +
  annotate("text", x = 0.5 * 365, y = 1997, label = "Tokyo brings a double whammy of heat and humidity to the 2020 Summer Olympic\nathletes, posing a high risk of heat illness.", family = f1, lineheight = 0.9, size = 2.5) +
  ggimage::geom_image(aes(x = 0.5 * 365, y = 1988.8, image = here::here("2024/data/img/ring-icon.png")), stat = "unique", size = 0.1) +
  scale_y_reverse(breaks = c(2021, 1964)) +
  scale_x_continuous(breaks = x_breaks$breaks, labels = x_breaks$labels) +
  scale_fill_gradientn(colors = pal) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    caption = "Data and original visualization: Reuters Graphics · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )
    

