library(tidyverse)
library(camcorder)
library(MetBrewer)
library(ggalluvial)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 8, height = 10, units = "in", dpi = 320)

marital <- read_csv(here::here("2022/data/marital-status-of-lgbt-adults.csv")) %>% 
  pivot_longer(4:11) %>% 
  mutate(name = str_remove(name, "% LGBT "))

pal <- c(met.brewer("Johnson"), met.brewer("Isfahan2"))

f1 <- "Georama"

ggplot(marital, aes(x = Year, y = value, alluvium = name, fill = name)) +
  geom_alluvium(color = "grey40", decreasing = FALSE, alpha = 0.8) +
  geom_text(aes(label = if_else(Year == 2016, name, NULL)), stat = "alluvium", decreasing = FALSE, family = f1, fontface = "bold", color = "black") +
  scale_x_continuous(breaks = 2015:2017) +
  # scale_fill_manual(values = pal) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA)
  )
