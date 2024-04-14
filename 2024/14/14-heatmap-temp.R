library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-14/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

# https://qz.com/2040947/how-tokyos-temperatures-compare-with-past-summer-olympics

temp <- tribble(
  ~city, ~tmax,
  "Tokyo 2020", 34.7,
  "Rio 2016", 33.2,
  "London 2012", 23.5,
  "Beijing 2008", 30.6,
  "Athens 2004", 34.2,
  "Sydney 2000", 22.8,
  "Atlanta 1996", 33,
  "Barcelona 1992", 28.4,
  "Seoul 1988", 26.5,
  "Los Angeles 1984", 25.8
) %>% 
  mutate(year = parse_number(city))

f1 <- "Graphik"
f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

pal <- c("#008080",
         "#a4debf",
         "#ef0041",
         "#F2E3A6",
         "#ff5bd7")


ggplot(temp, aes(year, 0, fill = tmax, label = tmax, color = ifelse(tmax < 28, "black", "white"))) +
  geom_tile(color = pal[4], linewidth = 1) +
  geom_text(family = f1b, fontface = "bold", size = 7) +
  geom_text(aes(y = -0.48, label = city), lineheight = 0.95, family = f1b, hjust = 0, angle = 90, fontface = "bold", size = 7) +
  scale_color_identity() +
  scale_fill_fermenter(palette = "YlOrRd", direction = 1) +
  labs(
    title = toupper("Max temperature (°C)\nduring the Olympics"),
    caption = toupper("Source: qz & cnn · Graphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f1b) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = pal[4], color = NA),
    plot.title = element_text(size = 36, hjust = 0.5, color = pal[3], face = "bold"),
    plot.caption = element_text(hjust = 0.5, color = pal[3], face = "bold"),
    plot.margin = margin(20, 0, 20, 0)
  )
  
