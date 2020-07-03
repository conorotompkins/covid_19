library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(modeest)
library(tidymodels)

options(scipen = 999, digits = 2)

theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  ungroup() %>% 
  filter(state == "Pennsylvania",
         county == "Allegheny County") %>% 
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
  ungroup()

df %>% 
  ggplot(aes(date, cases)) +
  geom_point()

df %>% 
  ggplot(aes(date, cases_new)) +
  geom_point()

df %>% 
  ggplot(aes(date, cases_new_rolling_14)) +
  geom_point() +
  geom_vline(xintercept = ymd("2020-06-18"))

waves <- df %>% 
  select(date, cases_new_rolling_14) %>%
  filter(!is.na(cases_new_rolling_14)) %>% 
  mutate(wave = case_when(date < "2020-06-18" ~ 1,
                          date >= "2020-06-18" ~ 2)) %>% 
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


### find days until peak for wave 1

waves %>% 
  select(wave, days_since_wave_started, cases_new_rolling_14, pct_change) %>% 
  filter(wave == 1) %>% 
  mutate(cases_max = max(cases_new_rolling_14),
         max_flag = cases_new_rolling_14 == cases_max,
         flag_after_max = cumsum(max_flag) %>% as.logical) %>% 
  View()

#extrapolate when wave 2 will peak

#model how long wave 2 will last


####


waves %>% 
  ggplot(aes(days_since_wave_started, cases_new_rolling_14, color = as.factor(wave))) +
  geom_point() +
  facet_wrap(~wave, ncol = 1, scales = "free_y")

waves %>% 
  ggplot(aes(days_since_wave_started, pct_change, color = as.factor(wave))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~wave, ncol = 1, scales = "free_y")

waves %>% 
  filter(wave == 1)

waves <- waves %>% 
  select(wave, days_since_wave_started, pct_change) %>% 
  arrange(wave, days_since_wave_started) %>% 
  complete(days_since_wave_started, wave) %>% 
  arrange(wave, days_since_wave_started) 

waves %>% 
  filter(wave == 2)

waves %>% 
  filter(wave == 1) %>% 
  select(days_since_wave_started, pct_change) %>% 
  lm(pct_change ~ ., data = .) %>% 
  tidy()

waves_1 <- waves %>% 
  filter(wave == 1)

waves_2 <- waves %>% 
  filter(wave == 2)

waves_fitted <- waves %>% 
  filter(wave == 1) %>% 
  select(days_since_wave_started, pct_change) %>% 
  #lm(pct_change ~ ., data = .) %>% 
  loess(pct_change ~ days_since_wave_started, data = .) %>% 
  augment(newdata = waves_2) %>% 
  bind_rows(waves_1) %>% 
  mutate(flag_fitted = is.na(pct_change),
         .fitted = case_when(!is.na(pct_change) ~ as.double(NA),
                             is.na(pct_change) ~ .fitted))

waves_fitted %>% 
  ggplot(aes(days_since_wave_started, pct_change, color = as.factor(wave), fill = as.factor(wave))) +
  geom_point() +
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit),
              alpha = .5) +
  #geom_point(aes(y = .fitted)) +
  facet_wrap(~wave, scales = "free_y", ncol = 1)

waves %>% 
  select(wave, days_since_wave_started, pct_change) %>% 
  arrange(wave, days_since_wave_started) %>% 
  complete(days_since_wave_started, wave) %>% 
  arrange(wave, days_since_wave_started) %>% 
  View()



waves %>% 
  ggplot(aes(days_since_wave_started, cases_new_rolling_14, color = wave)) +
  geom_point() +
  facet_wrap(~wave, ncol = 1, scales = "free_y")

#compare peaks
cases <- df %>% 
  select(date, cases_new, cases_new_rolling_14) %>% 
  filter(!is.na(cases_new_rolling_14))


cases %>%
  select(cases_new_rolling_14) %>% 
  kmeans(centers = 2) %>% 
  augment(cases) %>% 
  ggplot(aes(date, cases_new_rolling_14, color = .cluster)) +
  geom_point()

cases %>% 
  count(cases_new, sort = TRUE)

mlv(cases$cases_new, method = "mfv")

d <- density(cases$cases_new_rolling_14)

d$x
d$y