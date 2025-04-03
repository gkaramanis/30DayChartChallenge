library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 8, height = 9, dpi = 320)

mothers_age_raw <- readxl::read_xlsx(here::here("2025/data/2024-12-9361-tables.xlsx"), sheet = "1.2 Ålder - kategorier", skip = 4) %>% 
  janitor::clean_names() 

mothers_age <- mothers_age_raw %>% 
  select(1, contains("ar")) %>%
  # Get total number of mothers per year
  select(year = 1, 14:19) %>% 
  mutate(across(1:7, parse_number)) %>%
  filter(!is.na(year)) %>% 
  pivot_longer(-year, names_to = "age_group", values_to = "n") %>% 
  mutate(
    age_group = str_remove(age_group, "_ar_.+"),
    age_group = str_remove(age_group, "x"),
    age_group = str_replace(age_group, "_", "-"),
    age_group = str_replace(age_group, "19", "-19"),
    age_group = str_replace(age_group, "40", "40+"),
    age_group = fct_rev(age_group)
  )

f1 <- "Asap Semi Expanded"

ggplot(mothers_age, aes(year, n, fill = age_group)) +
  annotate("label", x = -Inf, y = c(0, 60e3, 90e3, 120e3), label = c("", "60 000", "90 000", "120 000"), family = f1, size = 3, label.size = 0, fill = "grey99") +
  geom_col() +
  scale_x_continuous(breaks = c(1973, seq(1975, 2020, 5), 2023), expand = c(0.03, 0), minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0, 60e3, 90e3, 120e3)) +
  colorspace::scale_fill_discrete_diverging("Purple-Green", rev = TRUE) +
  coord_radial() +
  guides(
    theta = guide_axis_theta(angle = 0),
    fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1, reverse = TRUE)
    ) +
  labs(
    title = "Sweden's 50-year motherhood cycles",
    subtitle = str_wrap("In 2023, approximately 97 800 women gave birth in Sweden, a decrease of 5.8% compared to 2022. Birth rates have fluctuated in 10-15 year cycles since 1973, with peaks in 1990-1992 (121 000+ mothers) and 2016 (118 600 mothers), and a lowest point in the late 1990s (84 400 mothers in 1999). Throughout these cycles, the dominant maternal age group shifted from 25-29 years in 1973 to 30-34 years by 2023, with mothers under 25 decreasing from 43 400 to 7 600.", 95),
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
    fill = NULL
  ) +
  theme_minimal(base_family = f1) +
  theme(
    legend.position = "top",
    legend.text.position = "top",
    legend.key.height = unit(0.7, "lines"),
    legend.key.width = unit(2, "lines"),
    legend.margin = margin(10, 0, -30, 0),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.x = element_text(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25, color = "grey70"),
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(hjust = 0, margin = margin(-15, 0, 0, 0)),
    plot.margin = margin(10, 0, 10, 0)
  )
  
# The chart shows mothers, not children! Not accounting for multiple births!
# Keep in mind the above before reading below:
#
# In 2023, around 100 000 children were born in Sweden, a decrease of 5
# per cent compared with the previous year. The average age of both the
# primiparous and multiparous women has continued to increase.
#
# Continued marked decline in the total
# number of births
# In 2023, around 99 6001 births took place, of which around 1.3 per cent were
# multiple births. In total, around 100 65011 babies were born. First-time
# mothers made up 44 per cent of women giving birth.
# Since 1973, when data began to be collected for the Medical Birth Register,
# birth rates have fluctuated in cycles of 10 to 15 years (see Figure 1). After a
# downward trend in the 1970s, there was a turnaround in 1983, followed by a
# sharp increase that peaked in the period 1990-1992, when more than 120
# 000 children were born. The number of births hit rock bottom in the late 1990s, with
# fewer than 86 000 births. After that, the number of births increased until
# 2016, with the exception of 2011. Year 2016 marked the last peak, when
# 121 700 children were born.
# After 2016, the number of births has decreased every year, with a
# particularly large decrease in 2022 and 2023. Compared with the previous
# year, around 4 700 fewer babies were born in 2023. Not since 2005 have so