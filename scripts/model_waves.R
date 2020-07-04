library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(modeest)
library(tidymodels)
library(ggpmisc)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  ungroup() %>% 
  filter(state == "Pennsylvania") %>% 
  filter(county != "Unknown County") %>% 
  group_by(state, county) %>% 
  tq_mutate(
    # tq_mutate args
    select     = cases_new,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 14,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "cases_new_rolling_14"
  ) %>% 
  ungroup() %>% 
  mutate(id = str_c(state, county, sep = ", "))

last_updated <- last(df$date)

df %>% 
  distinct(id)

top_counties <- df %>% 
  group_by(id) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  arrange(-cases_new_rolling_14) %>% 
  select(id) %>% 
  slice(1:nrow(distinct(df, id)))

top_counties
 
df %>% 
  ggplot(aes(date, cases_new_rolling_14, group = id)) +
  geom_line()

#probably need to find optimum span per county. based on max rolling cases value?
span_value <- 7*5

waves <- df %>% 
  semi_join(top_counties) %>% 
  select(id, date, cases_new_rolling_14) %>%
  filter(!is.na(cases_new_rolling_14)) %>%  
  group_by(id) %>% 
  mutate(peaks = ggpmisc:::find_peaks(cases_new_rolling_14, span = span_value, strict = TRUE),
         valleys = ggpmisc:::find_peaks(-cases_new_rolling_14, span = span_value, strict = TRUE)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(wave = sum(peaks, valleys)) %>%
  ungroup() %>% 
  group_by(id) %>% 
  mutate(wave = (cumsum(wave) %/% 2),
         wave = wave + 1) %>% 
  ungroup()

waves <- waves %>% 
  group_by(id, wave) %>%
  mutate(wave_start_date = first(date),
         days_since_wave_started = date - wave_start_date,
         days_since_wave_started = as.numeric(days_since_wave_started)) %>% 
  ungroup()

filter_counties <- waves %>% 
  distinct(id, wave) %>% 
  group_by(id) %>% 
  mutate(wave_max = max(wave)) %>% 
  filter(wave == max(wave)) %>% 
  arrange(-wave) %>% 
  select(id, wave_max)

waves %>% 
  group_by(id) %>% 
  summarize(cases_max = max(cases_new_rolling_14)) %>% 
  left_join(filter_counties) %>% 
  ggplot(aes(wave_max, cases_max)) +
  geom_jitter()

waves %>% 
  #semi_join(filter_counties) %>% 
  ggplot(aes(date, cases_new_rolling_14, color = as.factor(wave), group = id)) +
  geom_line()

waves %>% 
  inner_join(filter_counties) %>% 
  ggplot(aes(date, cases_new_rolling_14, color = as.factor(wave), group = id)) +
  geom_line() +
  facet_wrap(~wave_max, scales = "free", ncol = 1)

waves %>% 
  ggplot(aes(date, id, fill = as.factor(wave))) +
  geom_tile() +
  coord_equal()

waves %>% 
  slice(1) %>% 
  pull(id)

waves %>% 
  filter(id == "Florida, Miami-Dade County") %>% 
  #count(county)
  mutate(wave = as.factor(wave)) %>% 
  ggplot(aes(date, cases_new_rolling_14, group = id)) +
  geom_line(size = 2, show.legend = FALSE) +
  stat_peaks(size = 4, span = 17, color = "red") +
  stat_valleys(size = 4, span = 17, color = "blue")

waves <- waves %>% 
  group_by(wave) %>%
  mutate(wave_start_date = first(date),
         days_since_wave_started = date - wave_start_date,
         days_since_wave_started = as.numeric(days_since_wave_started)) %>% 
  ungroup()

waves <- waves %>%
  group_by(wave) %>% 
  mutate(pct_change = (cases_new_rolling_14 - lag(cases_new_rolling_14)) / cases_new_rolling_14) %>% 
  ungroup() %>% 
  replace_na(list(pct_change = 0))

waves <- waves %>% 
  mutate(wave = as.factor(wave))