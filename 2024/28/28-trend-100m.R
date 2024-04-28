library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-28"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

times <- tribble(
                       ~games,  ~men, ~women,
                "Athens 1896",    12,     NA,
                 "Paris 1900",    11,     NA,
             "St Louis 1904",    11,     NA,
                "London 1908",  10.80,     NA,
             "Stockholm 1912",  10.80,     NA,
               "Antwerp 1920",  10.80,     NA,
                 "Paris 1924",  10.60,     NA,
             "Amsterdam 1928",  10.80,   12.20,
           "Los Angeles 1932",  10.30,   11.90,
                "Berlin 1936",  10.30,   11.50,
                "London 1948",  10.30,   11.90,
              "Helsinki 1952",  10.40,   11.50,
             "Melbourne 1956",  10.50,   11.50,
                  "Rome 1960",  10.20,     11,
                 "Tokyo 1964",    10,   11.40,
           "Mexico City 1968",   9.95,     11,
                "Munich 1972",  10.14,   11.07,
              "Montreal 1976",  10.06,   11.08,
                "Moscow 1980",  10.25,   11.06,
           "Los Angeles 1984",   9.99,   10.97,
                 "Seoul 1988",   9.92,   10.62,
             "Barcelona 1992",   9.96,   10.82,
               "Atlanta 1996",   9.84,   10.94,
                "Sydney 2000",   9.87,     NA,
                "Athens 2004",   9.85,   10.93,
               "Beijing 2008",   9.69,   10.78,
                "London 2012",   9.63,   10.75,
                   "Rio 2016",   9.81,   10.71,
                 "Tokyo 2020",   9.80,   10.61
           ) %>% 
  mutate(year = parse_number(games)) %>% 
  pivot_longer(men:women, values_to = "time", names_to = "sex")


f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "DIN Condensed"
f2b <- "Produkt Medium"

secs <- data.frame(
  x = 1896,
  y = seq(9.5, 12, 0.5),
  sex = "women"
)

pal <- c("#A3D8FF", "#94FFD8", "#FDFFC2", "#FF76CE", "black")

ggplot(times, aes(year, time, group = sex)) +
  geom_smooth(fill = pal[3], color = pal[4], alpha = 1, linewidth = 2) +
  geom_point(size = 2, color = pal[5], shape = 21, stroke = 3) +
  shadowtext::geom_shadowtext(data = secs, aes(x, y, label = if_else(y %in% c(9.5, 12), paste(y, "seconds"), as.character(y))), hjust = 0, vjust = 0, nudge_y = 0.05, family = f2, size = 11, fontface = "bold", color = "white", bg.color = pal[5], bg.r = 0.05) +
  scale_x_continuous(breaks = c(1896, 2020)) +
  scale_y_continuous(minor_breaks = seq(9.5, 12, 0.1)) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    title = toupper("Women's and men's\n100m winning times"),
    caption = toupper("Source: Statista · Graphic: Georgios Karamanis")
  ) +
  theme_minimal(base_family = f1, base_size = 30) +
  theme(
    plot.background = element_rect(fill = pal[1], color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = pal[5], linewidth = 0.4),
    panel.grid.minor.y = element_line(color = pal[5], linewidth = 0.1),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(hjust = c(0, 1), color = pal[4], face = "bold", size = 35) ,
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 0.5)
  )
  
