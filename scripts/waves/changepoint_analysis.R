library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(modeest)
library(tidymodels)
library(ggpmisc)
library(changepoint)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  arrange(state, county, date) %>% 
  select(state, county, date, cases:deaths)

top_counties <- df %>% 
  filter(state == "Pennsylvania") %>% 
  group_by(state, county) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  arrange(desc(cases)) %>%
  slice(1:5) %>% 
  select(state, county)

df <- df %>% 
  pivot_longer(cases:deaths, names_to = "metric") %>% 
  arrange(state, county, metric, date) %>% 
  group_by(state, county, metric) %>% 
  mutate(value_new = value - lag(value)) %>% 
  ungroup() %>% 
  mutate(value_new = case_when(value_new < 0 ~ NA_real_,
                               value_new >= 0 ~ value_new)) %>% 
  filter(state == "Pennsylvania") %>% 
  filter(county != "Unknown County")

df <- df %>% 
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
  ungroup()
  
  

df_change <- df %>% 
  semi_join(top_counties) %>% 
  filter(state == "Pennsylvania",
         #county %in% c("Allegheny County", "Philadelphia County"),
         metric == "cases") %>% 
  filter(!is.na(value_new_rolling_14)) %>% 
  group_by(state, county, metric) %>% 
  mutate(pct_change = (value_new_rolling_14 / lag(value_new_rolling_14)) - 1,
         #pct_change = round(pct_change, 2)
         ) %>% 
  ungroup()


df_change %>% 
  ggplot(aes(date, pct_change, color = county)) +
  geom_point() +
  geom_smooth(span = .1) +
  facet_wrap(~county, ncol= 1, scales = "free_y")

#smooth pct change with loess
pct_change_model <- function(df) {
  loess(pct_change ~ date_numeric, data = df, span = .66)
}

df_change <- df_change %>% 
  filter(!is.na(pct_change),
         #county != "Allegheny County"
         ) %>%
  select(state, county, date, pct_change) %>% 
  group_by(state, county) %>% 
  mutate(date_numeric = row_number()) %>% 
  ungroup() %>% 
  group_by(state, county) %>% 
  nest() %>% 
  mutate(model = map(data, pct_change_model),
         fitted_data = map(model, augment)) %>% 
  unnest(fitted_data) %>% 
  select(-c(data, model)) %>% 
  rename(pct_change_fitted = .fitted)

df_change %>% 
  #unnest_longer(data) %>% 
  #janitor::clean_names()
  ggplot(aes(date_numeric, .fitted)) +
  geom_line() +
  facet_wrap(~county)

  
df_change <- df_change %>% 
  select(state, county, date_numeric, pct_change_fitted) %>% 
  mutate(sign = pct_change_fitted > 0,
         change_point = sign != lag(sign, 1) & sign != lag(sign, 3) & sign != lag(sign, 5))

df_change_points <- df_change %>% 
  filter(change_point == TRUE) %>% 
  select(state, county, date_numeric)


df_change %>% 
  #pivot_longer(cols = c(value_new_rolling_14, pct_change_rolling_14)) %>%
  ggplot(aes(date_numeric, pct_change_fitted)) +
  geom_line() +
  #geom_point(aes(color = change_point_flag)) +
  geom_vline(data = df_change_points, aes(xintercept = date_numeric), linetype = 2, color = "red") +
  facet_grid(~county, scales = "free_y")













####old code

df %>% 
  filter(state == "Pennsylvania",
         county == "Allegheny County",
         metric == "cases") %>% 
  filter(!is.na(value_new_rolling_14)) %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(index, value_new_rolling_14)) +
  geom_point() +
  geom_vline(xintercept = 98)
  
df_change <- df %>% 
  filter(state == "Pennsylvania",
         county == "Allegheny County",
         metric == "cases") %>% 
  filter(!is.na(value_new_rolling_14)) %>% 
  group_by(id, metric) %>% 
  mutate(pct_change = deriv(value_new_rolling_14))
  #mutate(pct_change = (value_new_rolling_14 / lag(value_new_rolling_14)) / 100) %>% 
  ungroup() %>% 
  group_by(id, metric) %>% 
  tq_mutate(
    # tq_mutate args
    select     = pct_change,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 14,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "pct_change_rolling_14"
  ) %>% 
  ungroup() %>% 
  filter(!is.na(pct_change_rolling_14))

df_change <- df_change %>% 
  group_by(id, metric) %>% 
  mutate(change_in_change = pct_change_rolling_14 - lag(pct_change_rolling_14)) %>% 
  ungroup()

df_change %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(index, pct_change_rolling_14)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 95)

changepoint::cpt.mean(df_change$pct_change_rolling_14, class = FALSE)

x=c(rnorm(50,0,1),rnorm(50,5,1),rnorm(50,10,1),rnorm(50,3,1))

plot(x)
cpt.mean(x,penalty="Manual",pen.value="2*log(n)",method="BinSeg",Q=5,class=FALSE) 
