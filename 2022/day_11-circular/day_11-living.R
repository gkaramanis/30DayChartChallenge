library(tidyverse)
library(camcorder)
library(ggforce)
library(patchwork)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 10, height = 10, units = "in", dpi = 320)

living <- tribble(
  ~identity, ~"Never or occasionally", ~"(Almost) always",
  "Other*", 23, 77,
  "Trans women", 29, 71,
  "Trans men", 24, 76,
  "Non-binary", 58, 42,
  "Total", 38, 63
) %>% 
  select(-2) %>% 
  mutate(
    x = c(2, 1, 3, 2, 2),
    y = c(3, 2, 2, 1, 2),
    r = list(0:25)
    ) %>% 
  unnest(r)

f1 <- "Publico Headline"
f2 <- "Montserrat"
f2b <- "Montserrat Black"

ggplot(living) +
  geom_circle(aes(x0 = x * 60, y0 = y * 60, r = r, color = 
                    case_when(
                      r < `(Almost) always`/4 & identity == "Total" ~ "darkgreen",
                      r < `(Almost) always`/4 & identity != "Total" ~ "purple3",
                      TRUE ~ "grey80"
                    )), size = 1.5) +
  geom_text(aes(x * 60 + 4, y * 60 -1.2, label = paste0(`(Almost) always`, "%")), stat = "unique", size = 12, family = f1, fontface = "bold",  color = "grey97", hjust = 1) +
  geom_text(aes(x * 60, y * 60 + 30, label = identity), family = f1, size = 6, fontface = "bold", stat = "unique") +
  annotate("text", x = c(60, 180, 60, 155), y = c(180, 180, 60, 60), label = c("Ability\nto live", "according\nto one's", "gender\nidentity", str_wrap("Source: 'In society I don't exist, so it's impossible to be who I am.' – Trans people’s health and experiences of healthcare in Sweden, RFSL, 2017 · Graphic: Georgios Karamanis", 28)), size = c(14, 14, 14, 4.5), hjust = c(0.5, 0.5, 0.5, 0), family = c(f2b, f2b, f2b, f2), color = "darkorange1") +
  scale_color_identity() +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey94", color = NA)
  )
