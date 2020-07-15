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
  select(state, county, date, cases:deaths)

df <- df %>% 
  pivot_longer(cases:deaths, names_to = "metric") %>% 
  arrange(state, county, metric, date) %>% 
  group_by(state, county, metric) %>% 
  mutate(value_new = value - lag(value)) %>% 
  ungroup() %>% 
  mutate(value_new = case_when(value_new < 0 ~ NA_real_,
                               value_new >= 0 ~ value_new)) %>% 
  filter(state == "Pennsylvania") %>% 
  filter(county != "Unknown County") %>% 
  group_by(state, county, metric) %>% 
  tq_mutate(
    # tq_mutate args
    select     = value_new,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 14,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "value_new_rolling_14"
  ) %>% 
  ungroup() %>% 
  mutate(id = str_c(state, county, sep = ", "))

last_updated <- last(df$date)

df %>% 
  filter(county == "Allegheny County") %>% 
  ggplot(aes(date, value_new_rolling_14, group = id)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_y", ncol = 1)

df %>% 
  distinct(id)

top_counties <- df %>% 
  filter(metric == "cases") %>% 
  group_by(state, county) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  arrange(-value_new_rolling_14) %>% 
  select(state, county) %>% 
  slice(1:nrow(distinct(df, state, county))) %>% 
  slice(1:2)

top_counties
 
df %>% 
  semi_join(top_counties) %>% 
  ggplot(aes(date, value_new_rolling_14, group = id)) +
  geom_line() +
  facet_wrap(~metric, scales = "free", ncol = 1)

#probably need to find optimum span per county. based on max rolling cases value?
span_value <- 7*3

#need to define what a wave is

# definition 1
# find peak 1
# find peak 2
# find minimum between peaks 1 and 2

#definition 2
# find peak 1
# find minimum after peak 1

waves <- df %>% 
  semi_join(top_counties) %>% 
  select(id, date, metric, value_new_rolling_14) %>%
  arrange(id, metric, date) %>% 
  filter(!is.na(value_new_rolling_14)) %>%  
  group_by(id, metric) %>% 
  mutate(peak = ggpmisc:::find_peaks(value_new_rolling_14, 
                                      span = span_value,
                                      #ignore_threshold = 0,
                                      strict = TRUE)) %>%
  ungroup()

waves %>% 
  ggplot(aes(date, value_new_rolling_14, group = id)) +
  geom_line() +
  geom_point(aes(color = peak)) +
  facet_wrap(~metric, scales = "free", ncol = 1) +
  scale_color_manual(values = c("black", "red"))

waves <- waves %>% 
  group_by(id, metric) %>% 
  mutate(peak_count = cumsum(peak == TRUE),
         max_peak = max(peak_count),
         between_peaks = peak_count > 0 & peak_count < max_peak) %>% 
  ungroup() %>% 
  group_by(id, metric, between_peaks) %>% 
  mutate(valley = case_when(between_peaks == TRUE ~ min(value_new_rolling_14),
                            between_peaks == FALSE ~ NA_real_),
         valley = value_new_rolling_14 == valley)%>% 
  ungroup()

#calculate valley for when there is only 1 peak

waves <- waves %>% 
  mutate(after_only_peak = max_peak == 1 & peak_count == max_peak) %>% 
  group_by(id, metric, after_only_peak) %>% 
  mutate(min_after_first_peak = case_when(after_only_peak == TRUE ~ min(value_new_rolling_14),
                            after_only_peak == FALSE ~ NA_real_),
         flag_min_after_first_peak = case_when(after_only_peak == TRUE & value_new_rolling_14 == min_after_first_peak ~ TRUE,
                            after_only_peak == FALSE ~ valley,
                            TRUE ~ FALSE),
         valley = case_when(after_only_peak == TRUE & flag_min_after_first_peak == TRUE ~ cumsum(flag_min_after_first_peak) == 1,
                            TRUE ~ valley)) %>% 
  ungroup()


waves 
    


waves %>% 
  ggplot(aes(date, value_new_rolling_14, group = id)) +
  geom_line() +
  geom_point(data = filter(waves, peak), color = "red") +
  geom_point(data = filter(waves, valley), color = "blue") +
  facet_wrap(id~metric, scales = "free")
  
  

# waves_pad <- waves %>% 
#   group_by(id, metric) %>% 
#   filter(date == last(date))
# 
# waves <- waves %>% 
#   bind_rows(waves_pad)

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  group_by(id, metric) %>% 
  mutate(flag_between_peaks = cumsum(peaks == TRUE)) %>% 
  ungroup() %>% 
  group_by(id, metric, flag_between_peaks) %>% 
  mutate(valley = min(value_new_rolling_14)) %>% 
  View()

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  ggplot(aes(date, value_new_rolling_14, group = id)) +
  geom_line() +
  geom_point(data = filter(waves, id == "Pennsylvania, Allegheny County", peaks == TRUE), color = "red") +
  #geom_point(data = filter(waves, id == "Pennsylvania, Allegheny County", valleys == TRUE), color = "blue") +
  facet_wrap(~metric, scales = "free_y", ncol = 1)


  rowwise() %>% 
  mutate(wave = sum(peaks, valleys)) %>%
  ungroup() %>% 
  group_by(id, metric) %>% 
  mutate(wave = (cumsum(wave) %/% 2),
         wave = wave + 1) %>% 
  ungroup()

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  count(metric, valleys)

waves <- waves %>% 
  group_by(id, metric, wave) %>%
  mutate(wave_start_date = first(date),
         days_since_wave_started = date - wave_start_date,
         days_since_wave_started = as.numeric(days_since_wave_started)) %>% 
  ungroup()

filter_counties <- waves %>% 
  distinct(id, metric, wave) %>% 
  group_by(id, metric) %>% 
  mutate(wave_max = max(wave)) %>% 
  filter(wave == max(wave)) %>% 
  arrange(id, metric, wave) %>% 
  select(id, metric, wave_max)

waves %>% 
  group_by(id, metric) %>% 
  summarize(value_max = max(value_new_rolling_14)) %>% 
  left_join(filter_counties) %>% 
  ggplot(aes(wave_max, value_max)) +
  geom_jitter() +
  facet_wrap(~metric, scales = "free")

waves %>% 
  #semi_join(filter_counties) %>% 
  ggplot(aes(date, value_new_rolling_14, color = as.factor(wave), group = id)) +
  geom_line() +
  facet_wrap(~metric, scales = "free", ncol = 1)

waves %>% 
  inner_join(filter_counties) %>% 
  ggplot(aes(date, value_new_rolling_14, color = as.factor(wave), group = id)) +
  geom_line() +
  facet_grid(metric~wave_max, scales = "free")

waves %>% 
  ggplot(aes(date, id, fill = as.factor(wave), alpha = value_new_rolling_14)) +
  geom_tile() +
  facet_wrap(~metric) +
  coord_equal() +
  theme(axis.text.y = element_blank())

waves %>% 
  slice(1) %>% 
  pull(id)

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  count(metric, valleys)

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  count(metric, peaks)

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  View()

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  ggplot(aes(date, value_new_rolling_14, color = as.factor(wave), group = id)) +
  geom_line() +
  geom_point(data = filter(waves, id == "Pennsylvania, Allegheny County",
                           peaks == TRUE)) +
  facet_wrap(~metric, ncol = 1, scales = "free_y")

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>% 
  #count(county)
  mutate(wave = as.factor(wave)) %>% 
  ggplot(aes(date, value_new_rolling_14, group = id)) +
  geom_line(size = 1, show.legend = FALSE) +
  geom_point(aes(color = peaks != TRUE), size = 2) +
  #stat_peaks(size = 4, span = span_value, color = "red") +
  #stat_valleys(size = 4, span = span_value, color = "blue") +
  facet_wrap(~metric, scales = "free_y", ncol = 1)

waves %>% 
  filter(id == "Pennsylvania, Allegheny County") %>%
  select(id, )
