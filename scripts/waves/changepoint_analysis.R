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
  # mutate(pct_change = (value_new_rolling_14 / lag(value_new_rolling_14)) - 1,
  #        d2 = (pct_change / lag(pct_change)) - 1
  #        ) %>% 
  mutate(d1 = value_new_rolling_14 - lag(value_new_rolling_14),
         d2 = (d1 - lag(d1)) - 1) %>% 
  ungroup()


df_change %>% 
  ggplot(aes(date, value_new_rolling_14, color = county)) +
  geom_line()

df_change %>% 
  ggplot(aes(date, d1, color = county)) +
  geom_smooth(se = FALSE)# +
  #facet_wrap(~county, ncol = 1, scales = "free_y")

df_change %>% 
  ggplot(aes(date, d2, color = county)) +
  geom_smooth(se = FALSE)# +
  
df_change %>% 
  ggplot(aes(d1, d2, color = county)) +
  geom_point()
  
  #smooth pct change with loess
d2_model <- function(df) {
  loess(d2 ~ date_numeric, data = df, span = .66)
}

#df_change_nested <- 
df_change %>% 
  filter(!is.na(d2),
         #county != "Allegheny County"
         ) %>%
  select(state, county, date, value_new_rolling_14, d2) %>% 
  drop_na() %>% 
  group_by(state, county) %>% 
  mutate(date_numeric = row_number()) %>% 
  ungroup() %>% 
  group_by(state, county) %>% 
  nest() %>% 
  mutate(model = map(data, d2_model),
         fitted_data = map(model, augment))

df_change <- df_change_nested %>% 
  unnest(c(data, fitted_data), names_repair = "universal") %>% 
  select(-contains("..."), -model) %>% 
  rename(d2_fitted = .fitted)



df_change %>% 
  #unnest_longer(data) %>% 
  #janitor::clean_names()
  ggplot(aes(date, d2_fitted)) +
  geom_line() +
  facet_wrap(~county)

  
df_change <- df_change %>% 
  select(state, county, date, value_new_rolling_14, d2_fitted) %>% 
  mutate(sign = d2_fitted > 0,
         change_point = sign != lag(sign, 1))

df_change_points <- df_change %>% 
  filter(change_point == TRUE) %>% 
  select(state, county, date)


df_change %>% 
  pivot_longer(cols = c(value_new_rolling_14, d2_fitted)) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  #geom_point(aes(color = change_point_flag)) +
  geom_vline(data = df_change_points, aes(xintercept = date), linetype = 2, color = "red") +
  facet_grid(name~county, scales = "free_y")













#changepoint library
library(changepoint)

changepoints <- df %>% 
  filter(county == "Allegheny County",
         metric == "cases") %>% 
  select(value_new_rolling_14) %>% 
  drop_na(value_new_rolling_14) %>% 
  pull(value_new_rolling_14) %>% 
  changepoint::cpt.var(class = FALSE, method = "PELT", 
                       penalty='Manual', 
                       pen.value='2*log(n)',
                       minseglen = 10) %>% 
  enframe()

changepoints

df %>% 
  filter(county == "Allegheny County",
         metric == "cases") %>%  
  mutate(index = row_number()) %>% 
  ggplot(aes(index, value_new_rolling_14)) +
  geom_point() +
  geom_vline(data = changepoints, aes(xintercept = value))



df %>% 
  filter(metric == "cases",
         !is.na(value_new_rolling_14)) %>% 
  add_count(state, county) %>% 
  filter(n > 4) %>% 
  group_by(state, county) %>% 
  mutate(changepoints = map_dbl(.x = value_new_rolling_14, ~cpt.var(data = ., 
                                                               class = FALSE, 
                                                               method = "PELT", 
                                                               penalty='Manual',
                                                               pen.value='2*log(n)',
                                                               minseglen = 10)))


