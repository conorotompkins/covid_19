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

#check out https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations
vacc_data <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv") %>% 
  clean_names() %>% 
  filter(vaccine_type == "All")

glimpse(vacc_data)

vacc_data %>% 
  summarize(max(date))

vacc_data %>% 
  select(combined_key, date, stage_one_doses) %>%
  drop_na() %>% 
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
  arrange(desc(combined_key)) %>%  
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

census_age_data <- get_acs(geography = "state",
                           variables = census_vars_age,
                           summary_var = "B01003_001",
                           output = "tidy",
                           year = 2018, geometry = F)  %>%
  rename(total_pop = summary_est) %>% 
  select(NAME, variable, estimate, total_pop)

census_map <- get_acs(geography = "state",
                      variables = "B01003_001",
                      output = "tidy",
                      year = 2018, geometry = T, shift_geo = T) %>% 
  rename(total_pop = estimate) %>% 
  select(NAME, total_pop)

census_map %>%
  ggplot() +
  geom_sf(aes(fill = total_pop)) +
  scale_fill_viridis_c()

census_age_bins <- census_age_data %>% 
  mutate(variable = str_remove(variable, "^estimate!!total!!")) %>% 
  separate(variable, into = c("sex", "age"), sep = "!!") %>% 
  group_by(NAME, age) %>% 
  summarize(estimate = sum(estimate),
            total_pop = mean(total_pop)) %>% 
  ungroup() %>% 
  mutate(pct_of_pop = estimate / total_pop)

census_age_bins_wide <- census_age_bins %>% 
  select(NAME, age, pct_of_pop, total_pop) %>% 
  pivot_wider(names_from = age, values_from = pct_of_pop) %>% 
  clean_names()

census_age_bins_wide %>% 
  glimpse()

census_age_bins %>% 
  distinct(age)

#census_race

census_age_bins %>% 
  left_join(census_map, by = c("NAME")) %>% 
  st_as_sf() %>% 
  #filter(age == "65 and 66 years") %>% 
  ggplot(aes(fill = pct_of_pop)) +
  geom_sf() +
  scale_fill_viridis_c() +
  facet_wrap(~age)

vacc_age <- vacc_data %>% 
  select(province_state, date, stage_one_doses) %>% 
  group_by(province_state) %>% 
  filter(date == max(date)) %>% 
  ungroup() %>% 
  arrange(desc(stage_one_doses)) %>% 
  inner_join(census_age_bins_wide, by = c("province_state" = "name")) %>% 
  #st_sf() %>% 
  mutate(pct_pop_stage_one_dose = stage_one_doses / total_pop)

vacc_age %>% 
  #st_drop_geometry() %>% 
  summarize(pct_pop_stage_one_dose = sum(pct_pop_stage_one_dose))

vacc_age %>% 
  #st_drop_geometry() %>% 
  View()

vacc_age %>% 
  pivot_longer(starts_with("x"), names_to = "age", values_to = "pct_of_pop") %>% 
  select(province_state, pct_pop_stage_one_dose, age, pct_of_pop) %>% 
  ggplot(aes(pct_pop_stage_one_dose, pct_of_pop)) +
  geom_point() +
  geom_smooth(aes(color = age), method = "lm")

vacc_age %>% 
  ggplot(aes(pct_pop_stage_one_dose, total_pop)) +
  geom_point() +
  geom_label(aes(label = province_state)) +
  geom_smooth(method = "lm")

vacc_age %>%
  #distinct(province_state, pct_pop_stage_one_dose) %>% 
  left_join(census_map, by = c("province_state" = "NAME")) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = pct_pop_stage_one_dose)) +
  geom_sf() +
  scale_fill_viridis_c()

glimpse(vacc_age)

#political data
pres_results <- read_csv("data/1976-2020-president.csv") %>% 
  clean_names() %>% 
  filter(year == 2020,
         party_detailed %in% c("DEMOCRAT", "REPUBLICAN")) %>% 
  mutate(two_party_pct = candidatevotes / totalvotes,
         state = str_to_title(state)) %>% 
  select(year, state, party_detailed, two_party_pct) %>% 
  pivot_wider(names_from = party_detailed, values_from = two_party_pct) %>% 
  mutate(biden_margin = DEMOCRAT - REPUBLICAN)

pres_results %>% 
  left_join(census_map, by = c("state" = "NAME")) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = biden_margin)) +
  scale_fill_viridis_c()



vacc_model_data <- vacc_age %>% 
  left_join(pres_results, by = c("province_state" = "state")) %>% 
  select(-c(year, DEMOCRAT, REPUBLICAN)) %>% 
  drop_na(biden_margin)

vacc_model_data %>% 
  ggplot(aes(pct_pop_stage_one_dose, biden_margin)) +
  geom_point()

glimpse(vacc_model_data)

vacc_model <- lm(pct_pop_stage_one_dose ~ x65_and_66_years + x67_to_69_years + x70_to_74_years + x75_to_79_years + x80_to_84_years + x85_years_and_over + biden_margin,
                 data = vacc_model_data)

vacc_model %>% 
  tidy() %>% 
  View()

vacc_model %>% 
  glance()

vacc_model %>% 
  augment(vacc_model_data) %>% 
  View()

vacc_model %>% 
  augment(vacc_model_data) %>% 
  ggplot(aes(pct_pop_stage_one_dose, .fitted)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_label_repel(data = vacc_model %>% 
                     #augment(st_drop_geometry(vacc_age)) %>%
                     augment(vacc_model_data) %>% 
                     arrange(desc(abs(.resid))) %>% 
                     slice(1:5),
                   aes(label = province_state)) +
  tune::coord_obs_pred() +
  labs(y = "Predicted") +
  scale_x_percent() +
  scale_y_percent()

vacc_model %>% 
  augment(vacc_model_data) %>% 
  mutate(province_state = fct_reorder(province_state, .resid)) %>% 
  ggplot(aes(.resid, province_state)) +
  geom_col()

vacc_model_geo <- census_map %>% 
  inner_join(vacc_model %>% 
               augment(vacc_model_data), by = c("NAME" = "province_state")) %>% 
  st_as_sf()

vacc_model_geo %>% 
  ggplot() +
  geom_sf(aes(fill = .resid)) +
  scale_fill_viridis_c()
