library(tidyverse)
library(tidycensus)
library(sf)
library(janitor)
library(geogrid)
library(doParallel)
library(hrbrthemes)
library(tidyquant)
library(gganimate)
library(tidymodels)

options(scipen = 999, digits = 2)

theme_set(theme_ipsum(base_size = 15,
                      axis_title_size = 15))

df_covid_raw <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  select(-fips) %>% 
  filter(county != "Unknown County") %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  mutate(cases_new = cases - lag(cases),
         cases_new = case_when(cases_new < 0 ~ lag(cases_new),
                               cases_new >= 0 ~ cases_new)) %>% 
  ungroup() %>% 
  replace_na(list(cases_new = 0)) %>% 
  add_count(state, county) %>% 
  filter(n > 30) %>% 
  select(-n)

df_covid <- df_covid_raw %>% 
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
    col_rename = "cases_new_rolling_14") %>% 
  ungroup()

df_covid %>% 
  count(state) %>% 
  arrange(n)

df_covid <- df_covid %>% 
  group_by(state, county) %>% 
  mutate(cases_new_rolling_14_lead_2_weeks = lead(cases_new_rolling_14, 14)) %>% 
  ungroup() %>% 
  mutate(id = str_c(state, county, sep = "_"))

df_covid <- df_covid %>% 
  filter(!is.na(cases_new_rolling_14),
         !is.na(cases_new_rolling_14_lead_2_weeks)) %>% 
  select(state, id, date, cases_new_rolling_14, cases_new_rolling_14_lead_2_weeks)

df_covid %>% 
  count(state) %>% 
  arrange(n)

df_covid <- df_covid %>% 
  add_count(state) %>% 
  filter(n >= 500)

#split data
covid_split <- initial_split(df_covid, strata = state)
covid_train <- training(covid_split)
covid_test <- testing(covid_split)

model_lm <- linear_reg(mode = "regression") %>% 
  set_engine("lm")

covid_recipe <- recipe(cases_new_rolling_14 ~ ., data = covid_train) %>% 
  update_role(id, new_role = "ID") %>% 
  update_role(date, new_role = "ID") %>% 
  prep(strings_as_factors = FALSE)

covid_wf <- workflow() %>% 
  add_recipe(covid_recipe)

covid_wf

covid_wf <- covid_wf %>% 
  add_model(model_lm)

covid_wf

covid_wf %>% 
  fit(covid_train) %>% 
  pull_workflow_fit() %>% 
  tidy() %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(estimate, term)) +
  geom_point()

covid_train_fitted <- covid_wf %>% 
  fit(covid_train) %>% 
  predict(covid_train) %>% 
  bind_cols(covid_train)

covid_train_fitted %>% 
  ggplot(aes(cases_new_rolling_14, .pred)) +
  geom_point()

covid_train_fitted %>% 
  metrics(truth = cases_new_rolling_14, estimate = .pred)

covid_train_fitted %>% 
  #distinct(id)
  ggplot(aes(x = date)) +
  geom_point(aes(y = cases_new_rolling_14)) +
  geom_point(aes(y = .pred), alpha = .5, color = "red")


df_allegheny <- df_covid %>% 
  filter(state == "Pennsylvania",
         county == "Allegheny County") %>% 
  mutate(id = str_c(state, county))
  ggplot(aes(cases_new_rolling_14_lead_2_weeks, cases_new_rolling_14)) +
  geom_point(aes(size = date == last(date),
                 color = date == last(date)),
             alpha = .5) +
  geom_label(aes(label = date)) +
  #geom_smooth(group = 1) +
  scale_color_manual(values = c("black", "red"))
  #geom_path()

df_covid %>% 
  filter_all(all_vars(!is.na(.))) %>%
  summarize(rsq = cor(cases_new_rolling_14, cases_new_rolling_14_lead_2_weeks)^2)



