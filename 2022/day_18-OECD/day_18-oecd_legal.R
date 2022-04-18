library(tidyverse)
library(camcorder)
library(ggtext)

# 2019
incl_gen_2019 <- readxl::read_xlsx(here::here("2022/data/004-0ahtgn-Figure 1.4. Legal LGBTI inclusivity is improving in all O....xlsx"), sheet = 2, range = "A3:AK5") %>% 
  mutate(OECD = rowMeans(select(., 3:last_col()))) %>% 
  rename(provisions = 1) %>% 
  add_row(provisions = "All provisions", !!! colMeans(.[-1], na.rm = TRUE)) %>% 
  pivot_longer(2:last_col(), names_to = "country") %>% 
  mutate(year = 2019, .before = 1)

incl_spec_2019 <- readxl::read_xlsx(here::here("2022/data/004-0ahtgn-Figure 1.4. Legal LGBTI inclusivity is improving in all O....xlsx"), sheet = 2, range = "A12:AK14") %>% 
  mutate(OECD = rowMeans(select(., 3:last_col()))) %>% 
  rename(provisions = 1) %>% 
  add_row(provisions = "Average", !!! colMeans(.[-1], na.rm = TRUE)) %>% 
  pivot_longer(2:last_col(), names_to = "country") %>% 
  mutate(year = 2019, .before = 1)

# 1999
incl_gen_1999 <- readxl::read_xlsx(here::here("2022/data/004-0ahtgn-Figure 1.4. Legal LGBTI inclusivity is improving in all O....xlsx"), sheet = 2, range = "A34:AL36") %>% 
  mutate(OECD = rowMeans(select(., 3:last_col()), na.rm = TRUE)) %>% 
  janitor::remove_empty("cols") %>% 
  select(1, OECD, everything()) %>% 
  rename(provisions = 1) %>% 
  add_row(provisions = "All provisions", !!! colMeans(.[-1], na.rm = TRUE)) %>% 
  pivot_longer(2:last_col(), names_to = "country") %>% 
  mutate(year = 1999, .before = 1)

# Datasets to plot
# incl <- rbind(incl_gen_1999, incl_gen_2019, incl_spec_2019)

all_2019 <- incl_gen_2019 %>% 
  filter(startsWith(provisions, "All")) %>% 
  mutate(country = fct_reorder(country, value))

# gen_2019 <- incl_gen_2019 %>% 
#   filter(startsWith(provisions, "General"))

# group_2019 <- incl_gen_2019 %>% 
#   filter(startsWith(provisions, "Group"))

all_1999 <- incl_gen_1999 %>% 
  filter(startsWith(provisions, "All")) 

# Plot
gg_record(dir = here::here("2022/30daychart-temp/"), device = "png", width = 10, height = 12, units = "in", dpi = 320)

f1 <- "Golos UI"

ggplot() +
  geom_col(data = all_2019, aes(value, country, fill = if_else(country != "OECD", "purple4", "orange3")), width = 0.8) +
  geom_col(data = all_1999, aes(value, country, fill = if_else(country != "OECD", "#BC83FD", "#FCB651")), width = 0.3) +
  scale_x_continuous(sec.axis = dup_axis(), breaks = seq(0, 1, 0.2), limits = c(0, 1), labels = scales::label_percent(scale = 100)) +
  scale_fill_identity() +
  labs(
    title = "Legal LGBTI inclusivity is improving in all OECD countries",
    subtitle = " % of LGBTI-inclusive laws that have been passed as of <span style = 'color:#BC83FD;'>**1999**</span> and <span style = 'color:purple4;'>**2019**</span>, by OECD country",
    caption = "Source: OECD questionnaire on LGBTI-inclusive laws and policies (2019) · Graphic: Georgios Karamanis"
  ) +
  theme_minimal(base_family = f1, base_size = 18) +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(20, 30, 20, 30),
    plot.subtitle = element_markdown(size = 15, margin = margin(0, 0, 20, 0)),
    plot.caption = element_text(margin = margin(30, 0, 0, 0), size = 12, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
