library(tidyverse)
library(tidycensus)
library(sf)
library(broom)
library(lubridate)
library(janitor)
library(hrbrthemes)
library(ggrepel)
library(tune)

theme_set(theme_ipsum())

options(scipen = 999, digits = 4)

#https://github.com/govex/COVID-19/blob/master/data_tables/vaccine_data/us_data/data_dictionary.csv

vacc_data <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv") %>% 
  clean_names()

glimpse(vacc_data)

vacc_data %>% 
  summarize(max(date))

vacc_data %>% 
  select(combined_key, date, stage_one_doses) %>% 
  drop_na(stage_one_doses) %>% 
  #replace_na(list(stage_one_doses = 0)) %>% 
  ggplot(aes(date, stage_one_doses, group = combined_key)) +
  geom_line()


vacc_data %>% 
  select(combined_key, date, stage_one_doses) %>% 
  replace_na(list(stage_one_doses = 0)) %>% 
  group_by(combined_key) %>% 
  summarize(stage_one_doses_total = max(stage_one_doses)) %>% 
  ungroup() %>% 
  mutate(combined_key = fct_reorder(combined_key, stage_one_doses_total)) %>% 
  arrange(desc(combined_key))
ggplot(aes(stage_one_doses_total, combined_key)) +
  geom_col()

vacc_data %>% 
  distinct(combined_key)

census_vars <- load_variables(2018, "acs5", cache = TRUE) %>% 
  mutate(across(.cols = c(label, concept), str_to_lower))

census_vars %>% 
  filter(str_detect(concept, "median age")) %>% 
  #distinct(concept) %>% 
  View()

census_vars %>% 
  filter(str_detect(concept, "total population")) %>% 
  #distinct(concept) %>% 
  View()

census_vars %>% 
  filter(str_detect(concept, "sex by age")) %>% 
  #distinct(concept) %>% 
  View()

census_vars %>% 
  filter(str_detect(concept, "race")) %>% 
  #distinct(concept) %>% 
  View()

census_vars_age <- census_vars %>% 
  filter(name %in% c("B01001_020",
                     "B01001_021",
                     "B01001_022",
                     "B01001_023",
                     "B01001_024",
                     "B01001_025",
                     "B01001_044",
                     "B01001_045",
                     "B01001_046",
                     "B01001_047",
                     "B01001_048",
                     "B01001_049")) %>% 
  pull(name, label)

census_age <- get_acs(geography = "state",
                      variables = census_vars_age,
                      summary_var = "B01003_001",
                      output = "tidy",
                      year = 2018, geometry = T, shift_geo = T)  %>%
  rename(total_pop = summary_est)

#census_race

census_65_over <- census_age %>% 
  group_by(NAME) %>% 
  mutate(pct_of_pop = estimate / total_pop) %>% 
  summarize(pct_65_over = sum(pct_of_pop),
            total_pop = mean(total_pop))

census_65_over %>% 
  ggplot(aes(fill = pct_65_over)) +
  geom_sf() +
  scale_fill_viridis_c()

vacc_age <- vacc_data %>% 
  select(province_state, stage_one_doses) %>% 
  group_by(province_state) %>% 
  summarize(stage_one_doses = max(stage_one_doses, na.rm = T)) %>% 
  arrange(desc(stage_one_doses)) %>% 
  ungroup() %>% 
  inner_join(census_65_over, by = c("province_state" = "NAME")) %>% 
  st_sf() %>% 
  mutate(pct_pop_stage_one_dose = stage_one_doses / total_pop)

vacc_age %>% 
  st_drop_geometry() %>% 
  summarize(pct_pop_stage_one_dose = sum(pct_pop_stage_one_dose))

vacc_age %>% 
  ggplot(aes(pct_pop_stage_one_dose, pct_65_over)) +
  geom_point() +
  geom_smooth(method = "lm")

vacc_age %>% 
  ggplot(aes(pct_pop_stage_one_dose, total_pop)) +
  geom_point() +
  geom_label(#data = filter(vacc_age, province_state == "Michigan"),
    aes(label = province_state)) +
  geom_smooth(method = "lm")

vacc_age %>%
  ggplot(aes(fill = pct_pop_stage_one_dose)) +
  geom_sf() +
  scale_fill_viridis_c()

vacc_model <- lm(pct_pop_stage_one_dose ~ total_pop + pct_65_over, data = vacc_age)


vacc_model %>% 
  tidy()

vacc_model %>% 
  glance()

vacc_model %>% 
  augment(st_drop_geometry(vacc_age)) %>% 
  View()

vacc_model %>% 
  augment(vacc_age) %>% 
  ggplot(aes(pct_pop_stage_one_dose, .fitted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel(data = vacc_model %>% 
               augment(st_drop_geometry(vacc_age)) %>% 
               arrange(desc(abs(.resid))) %>% 
               slice(1:5),
             aes(label = province_state)) +
  tune::coord_obs_pred()

vacc_model %>% 
  augment(vacc_age) %>% 
  mutate(province_state = fct_reorder(province_state, .resid)) %>% 
  ggplot(aes(.resid, province_state)) +
  geom_col()
