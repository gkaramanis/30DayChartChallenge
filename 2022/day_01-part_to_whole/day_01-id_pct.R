library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 8, height = 8, units = "in", dpi = 320)

id_pct <- data.frame(
  trans_id_sv = c("Transperson", "Transsexuell", "Före detta transsexuell", "Transvestit", "Intergender", "Annat", "Osäker"),
  trans_id = c("Transgender", "Transsexual", "Former transsexual", "Transvestite", "Intergender", "Other", "Not sure"),
  pct = c(47, 37, 6, 16, 31, 5, 7)
) %>% 
  mutate(trans_id = fct_reorder(trans_id, -pct))

gender_pct <- data.frame(
  gender_sv = c("Man", "Kvinna", "Bade kvinna och man/mitt\nemellan kvinna och man", "Queer", "Inget/varken kvinna eller man", "Osäker"),
  gender = c("Man", "Woman", "Both woman and man/\nBetween woman and man", "Queer", "No gender/Neither\nwoman nor man", "Not sure"),
  pct = c(26, 36, 26, 26, 23, 8) 
) %>% 
  mutate(gender = fct_reorder(gender, -pct))

f1 <- "Outfit"

col <- "#D54288"

ggplot() +
  # id
  geom_bar(data = id_pct, aes(x = 0, y = pct, fill = trans_id), stat = "identity", color = "grey97", position = "fill", width = 0.5) +
  geom_text(data = id_pct, aes(x = 0, y = pct, fill = trans_id, label = paste0(trans_id, " ", pct, "%")), position = position_fill(vjust = 0.5), color = "grey97", family = f1) +
  # gender
  geom_bar(data = gender_pct, aes(x = 1, y = pct, fill = gender), stat = "identity", color = "grey97", position = "fill", width = 0.5) +
  geom_text(data = gender_pct, aes(x = 1, y = pct, fill = gender, label = paste0(gender, " ", pct, "%")), position = position_fill(vjust = 0.5), color = "grey97", family = f1) +
  scale_fill_manual(values = rep(col, 12)) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Transgender\nidentity", "Gender\nidentity"), position = "top") +
  coord_cartesian(expand = FALSE) +
  labs(
    title = "Identities of transgender people in Sweden",
    subtitle = "Multiple replies possible, totals exceed 100%",
    caption = "Source: Health and health determinants among transgender persons - a report on the\nhealth status of transgender persons in Sweden (2015) · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    axis.text.x = element_text(margin = margin(0, 0, 10, 0), color = col),
    plot.title = element_text(color = col, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = col, hjust = 0.5, margin = margin(5, 0, 20, 0)),
    plot.caption = element_text(color = col, hjust = 0.5, size = 10, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(20, 40, 10, 40)
  )

