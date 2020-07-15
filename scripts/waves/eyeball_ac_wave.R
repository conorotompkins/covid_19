library(tidyverse)
library(janitor)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(modeest)
library(tidymodels)
library(ggpmisc)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

df <- read_csv("output/ac_timeline/data/ac_rolling_data_cleaned.csv") %>% 
  mutate(metric = fct_inorder(metric)) %>% 
  mutate(flag_max = case_when(metric == "New cases" ~ ymd("2020-04-10"),
                              metric == "New hospitalizations" ~ ymd("2020-04-08"),
                              metric == "New deaths" ~ ymd("2020-04-30")))

df %>% 
  ggplot(aes(date, value_new_rolling_14)) +
  geom_point() +
  geom_vline(aes(xintercept = flag_max), linetype = 2, color = "red") +
  facet_wrap(~metric, scales = "free_y", ncol = 1)

last_updated <- last(df$date)

df %>%
  select(state, county, date, metric, value_new_rolling_14) %>% 
  arrange(state, county, metric, desc(value_new_rolling_14)) %>% 
  mutate(day_difference = date - lag(date)) %>% 
  View()


df %>% 
  distinct(state, county, metric, flag_max) %>% 
  filter(!is.na(flag_max)) %>% 
  pivot_wider(names_from = metric, names_prefix = "max_", values_from = flag_max) %>% 
  clean_names() %>% 
  mutate(cases_to_deaths_lag = max_new_deaths - max_new_cases)


df %>% 
  ggplot(aes(date, value_new_rolling_14)) +
  geom_point() +
  geom_vline(data = df, aes(xintercept = flag_max)) +
  facet_wrap(~metric, ncol = 1, scales = "free_y")
