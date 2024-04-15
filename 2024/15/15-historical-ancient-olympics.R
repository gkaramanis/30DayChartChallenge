library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-15/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

years <- c(seq(-776, -1, 4), seq(1, 393, 4))

col1 <- "#2E4073"
col2 <- "#9FC3DB"
col3 <- "#D9E5F1"

zz <- data.frame(year = years) %>% 
  mutate(
    x = (row_number() - 1) %% 12,
    y = (row_number() - 1) %/% 12
  ) %>% 
  mutate(
    x = if_else(y %% 2 == 1, x, 11 - x),
    i = row_number(),
    label = if_else(year < 0, paste(abs(year), "BCE"), paste(year, "CE")),
    color = if_else(year < 0, col1, col2)
  )

f1b <- "Graphik Compact"
f2 <- "Produkt"
f2b <- "Produkt Medium"

ggplot(zz %>% arrange(i, y, x), aes(x, y, label = label, color = I(color))) +
  geom_path(linewidth = 7, lineend = "square", linejoin = "mitre") +
  geom_text(data = . %>% filter(year %in% c(min(years), -4, 1, max(years))), family = f1b, fontface = "bold", size = 4.5, color = "grey99", hjust = c(1, 0, 1, 0)) +
  labs(
    title = toupper("The Ancient Olympic Games\nwere held every 4 years\nfrom 776 BCE to 393 CE"),
    caption = toupper("Graphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f2b) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5, size = 24, color = col1),
    plot.caption = element_text(hjust = 0.5, size = 11, color = col1)
  )
  