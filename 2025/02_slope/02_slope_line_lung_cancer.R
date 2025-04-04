lc <- lung_cancer_raw %>% 
  mutate(year = parse_number(ar)) %>% 
  filter(!is.na(year)) %>% 
  select(year, starts_with(c("individer", "doda"))) %>% 
  # filter(year == 1973 | year == 2023) %>% 
  pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>% 
  filter(year >= 1973) %>% 
  separate(variable, into = c("type", "sex"), sep = "_") %>% 
  mutate(
    type = if_else(type == "individer", "diagnosed", "deceased"),
    sex = if_else(sex == "kvinnor", "Women", "Men")
  )

ggplot(lc, aes(year, value, color = sex, linetype = type)) +
  geom_line() +
  geom_line(data = . %>% filter(year == 1973 | year == 2023), linewidth = 1) +
  scale_color_manual(values = c("#E96E4E", "#405D93")) +
  facet_wrap(vars(paste(sex, type))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14, base_family = f1) +
  labs(
    title = "Women's lung cancer rates now exceed men's in Sweden",
    subtitle = "In 1973, {.#E96E4E **men**} had nearly 4x higher rates than {.#405D93 **women**} for both diagnosis (40.0 vs 10.5) and deaths (38.4 vs 10.8) per 100 000. By 2023, {.#405D93 **women**} surpassed {.#E96E4E **men**} in both categories—48.6 vs 37.1 for diagnosis and 37.1 vs 30.3 for mortality.",
    caption = "Source: National Board of Health and Welfare (Socialstyrelsen) · Graphic: Georgios Karamanis",
    y = "per 100 000",
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, margin = margin(0, 0, 0, 0)),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_marquee(margin = margin(0, 0, 30, 0), lineheight = 1.1, family = f2, width = 1.05),
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(10, 0, 0, 0)),
    plot.margin = margin(10, 30, 10, 30)
  )
