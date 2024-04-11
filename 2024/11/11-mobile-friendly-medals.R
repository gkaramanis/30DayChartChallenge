library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2024/30daychart-temp-11/"), device = "png", width = 1080 * 2, height = 1350 * 2, units = "px", dpi = 320)

results <- tribble(
  ~"country", ~"total", ~"gold", ~"silver", ~"bronze",
  "United States", 113,	39,	41,	33,
  "China", 88,	38,	32,	18,
  "ROC", 71,	20,	28,	23,
  "Great Britain", 65, 22, 21, 22,
  "Japan",	58,	27,	14,	17
)

tokyo <- results %>% 
  pivot_longer(total:bronze, names_to = "medal_type", values_to = "n") %>% 
  filter(medal_type != "total") %>% 
  mutate(
    medal_type = toupper(medal_type),
    medal_type = fct_inorder(medal_type)
    ) %>% 
  group_by(medal_type) %>% 
  slice_max(order_by = n, n = 3) %>% 
  ungroup() %>% 
  arrange(medal_type, -n) %>% 
  mutate(country = fct_reorder(country, n))

f1b <- "Graphik Compact"

pal <- c(
  "#C05780",
  "#E7C582"
  )

x0 <- -0.1

ggplot(tokyo) +
  geom_text(aes(x0 - 0.3, country, label = country), hjust = 1, family = f1b, size = 7, fontface = "bold") +
  geom_text(aes(x0 - 0.1, country, label = n), hjust = 0, family = f1b, size = 7, fontface = "bold") +
  geom_segment(aes(x = x0 + 0.9, xend = x0 + 0.9 + n / 30, y = country, yend = country), lineend = "square", linewidth = 6) +
  scale_x_continuous(limits = c(-7, 5.5)) +
  scale_y_discrete(expand = expansion(mult = 0.7)) +
  facet_wrap(vars(medal_type), ncol = 1, scales = "free_y") +
  labs(
    title = "TOKYO 2020",
    subtitle = "MEDAL TABLE",
    caption = toupper("Source: nbcolympics.com · Graphic: Georgios Karamanis")
  ) +
  theme_void(base_family = f1b) +
  theme(
    plot.background = element_rect(fill = pal[2], color = NA),
    strip.background = element_rect(fill = pal[1], color = NA),
    strip.text = element_text(color = "white", size = 30),
    plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 30, hjust = 0.5, margin = margin(5, 0, 30, 0)),
    plot.caption = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(25, 5, 15, 5)
  )
  
