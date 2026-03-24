library(tidyverse)
library(camcorder)

gg_record(dir = here::here("2025/30daychart-temp/"), device = "png", width = 10, height = 8, dpi = 320)

# https://data.who.int/dashboards/global-progress/triple-billion?m49=752
who_3bil <- read_csv("https://xmart-api-public.who.int/DATA_/RELAY_3B_DATA?$select=IND_ID,IND_CODE,IND_UUID,IND_PER_CODE,DIM_TIME,DIM_TIME_TYPE,DIM_GEO_CODE_M49,DIM_GEO_CODE_TYPE,IND_NAME,GEO_NAME_SHORT,TRIPLE_BILLION,TRIPLE_BILLION_TRACER,RATE_PER_100_N,COUNT_N,RATE_PER_100_NL,COUNT_NL,RATE_PER_100_NU,COUNT_NU&$format=csv") %>% 
  janitor::clean_names()

# https://gpw13.github.io/billionaiRe
indicators_raw <- rio::import(here::here("2025/data/indicator_df.rda"))
  
indicators <- indicators_raw %>% 
  filter(uhc) %>% 
  select(ind, contains("name"), contains("scenario"))

who_3bil_swe <- who_3bil %>%
  filter(geo_name_short == "Sweden") %>% 
  select(year = dim_time, triple_billion, triple_billion_tracer, starts_with("count_"))

who_3bil_uhc <- who_3bil_swe %>% 
  filter(triple_billion == "UHC") %>%
  group_by(triple_billion_tracer) %>%
  mutate(median_count = median(count_n, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(median_count != 0) %>% 
  mutate(ind = str_split_i(tolower(triple_billion_tracer), "_", 1)) %>% 
  left_join(indicators_df)
  
ggplot(who_3bil_uhc, aes(x = year, group = triple_billion_tracer)) +
  geom_ribbon(aes(ymin = count_nl, ymax = count_nu), fill = "purple", alpha = 0.5) +
  geom_line(aes(y = count_n), color = "black") +
  scale_x_continuous(breaks = seq(2000, 2030, by = 5)) +
  scale_y_continuous(labels = scales::number) +
  facet_wrap(vars(transformed_name), scales = "free_y") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
  
