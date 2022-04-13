library(tidyverse)
library(camcorder)
library(ggtext)
library(glue)

gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 8, height = 10, units = "in", dpi = 320)

# https://ajp.psychiatryonline.org/doi/10.1176/appi.ajp.2019.19010080?url_ver=Z39.88-2003&rfr_id=ori:rid:crossref.org&rfr_dat=cr_pub%20%200pubmed
mhealth <- tribble(
  ~time, ~type, ~pct,
  0, "treatment", 45.3,
  0, "hospitalization", 2.8,
  10, "treatment", 21.1,
  10, "hospitalization", 0
)


f1 <- "Source Serif Pro"
f2 <- "Graphik Compact"

col1 <- MetBrewer::met.brewer("Archambault")[6]
col2 <- MetBrewer::met.brewer("Archambault")[1]

ggplot(mhealth, aes(x = time, y = pct, group = type, color = type)) +
  annotate("segment", x = 0, xend = 10, y = 12.5, yend = 12.5, color = col2, linetype = "dashed", size = 0.75) +
  annotate("segment", x = 0, xend = 10, y = 0.1, yend = 0.1, color = col1, linetype = "dashed", size = 0.75) +
  annotate("text", x = 5, y = c(11, -1.6), color = c(col2, col1), label = "General population", family = f2) +
  geom_line(size = 1.5) +
  geom_point(size = 4, shape = 21, stroke = 2, fill = "grey97") +
  scale_x_continuous(limits = c(-2, 12), breaks = c(0, 10), labels = c("Perioperative", "≥10 years")) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_color_manual(values = c(col1, col2)) +
  labs(
    title = glue("Prevalence of <span style = 'color:{col2};'>**treatment for mood or anxiety disorders**</span> (health care visit or<br>antidepressant or anxiolytic prescription) and <span style = 'color:{col1};'>**hospitalization after suicide attempt**</span><br>in 2015 among individuals with a gender incongruence diagnosis, by number of<br>years since last gender-affirming surgery"),
    caption = '*Source*: Bränström, Richard, and John E. Pachankis. "Reduction in Mental Health Treatment Utilization Among Transgender<br>Individuals AfterGender-Affirming Surgeries: A Total Population Study." American Journal of Psychiatry, 2019, appi.ajp.2019.1.<br>https:&#47;&#47;doi.org&#47;10.1176&#47;appi.ajp.2019.19010080 · *Graphic*: Georgios Karamanis'
  ) +
  theme_minimal(base_family = f1, base_size = 11) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = NA),
    plot.title = element_markdown(lineheight = 1.1),
    plot.caption = element_markdown(size = 8, hjust = 0, margin = margin(30, 0, 0, 0), lineheight = 1.1),
    axis.title = element_blank(),
    axis.text = element_text(size = 18),
    panel.grid.minor.x  = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )
