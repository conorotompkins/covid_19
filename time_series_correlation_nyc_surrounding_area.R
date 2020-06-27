library(tidyverse)
library(tidycensus)
library(sf)
library(janitor)
library(geogrid)
library(doParallel)
library(hrbrthemes)
library(tidyquant)
library(gganimate)

options(scipen = 999, digits = 2)

theme_set(theme_ipsum(base_size = 15,
                      axis_title_size = 15))

us_states <- df_us_counties <- get_decennial(geography = "state", variables = "P001001", 
                                             shift_geo = TRUE, geometry = TRUE)
ny <- us_states %>% 
  filter(NAME == "New York")

ri <- us_states %>% 
  filter(NAME == "Rhode Island")

df <- us_states %>% 
  st_filter(x = ., y = ny,  .predicate = st_touches) %>% 
  bind_rows(ny) %>% 
  bind_rows(ri)

states <- df %>% 
  st_drop_geometry() %>% 
  distinct(NAME)

df_us_counties <- get_decennial(geography = "county", variables = "P001001", 
                                shift_geo = TRUE, geometry = TRUE) %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim)) %>% 
  semi_join(states, by = c("state" = "NAME"))

nyc_county_list <- c("New York County", "Kings County", "Queens County", "Bronx County", "Richmond County")

nyc_counties <- df_us_counties %>% 
  st_drop_geometry() %>% 
  filter(state == "New York") %>% 
  filter(county %in% nyc_county_list) %>% 
  select(state, county) %>% 
  mutate(type = "NYC counties")

nyc_counties_combined <- df_us_counties %>% 
  semi_join(nyc_counties) %>% 
  summarize(value = sum(value)) %>% 
  mutate(state = "New York",
         county = "New York City County")

df_us_counties <- df_us_counties %>% 
  anti_join(nyc_counties) %>% 
  bind_rows(nyc_counties_combined)

df_covid_raw <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  select(-fips) %>% 
  semi_join(states, by = c("state" = "NAME")) %>% 
  filter(county != "Unknown County")

df_covid <- df_covid_raw %>% 
  complete(state, county, date) %>% 
  replace_na(list(cases = 0, deaths = 0)) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  mutate(cases_new = cases - lag(cases),
         cases_new = case_when(cases_new < 0 ~ lag(cases_new),
                               cases_new >= 0 ~ cases_new)) %>% 
  ungroup() %>% 
  replace_na(list(cases_new = 0))

df_covid <- df_covid %>% 
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

df_us_states <- df_us_counties %>% 
  group_by(state) %>% 
  summarize()

surrounding_ny_counties <- df_us_counties %>% 
  left_join(df_covid) %>% 
  #mutate(cases_new_rolling_14_per_capita = (cases_new_rolling_14 / value) * 100000) %>% 
  arrange(state, county, date)

surrounding_ny_counties

nyc_covid <- surrounding_ny_counties %>% 
  filter(state == "New York",
         county == "New York City County") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(date, cases_new_rolling_14) %>% 
  rename(cases_new_rolling_14_nyc = cases_new_rolling_14)

surrounding_counties_covid <- surrounding_ny_counties %>% 
  filter(county != "New York City County") %>% 
  st_drop_geometry() %>% 
  as_tibble()

surrounding_counties_covid %>% 
  filter(state == "New York")
  

time_series <- surrounding_counties_covid %>% 
  select(state, county, date, cases_new_rolling_14) %>% 
  # group_by(date) %>% 
  # summarize(cases_new_rolling_14_per_capita = mean(cases_new_rolling_14_per_capita)) %>% 
  # ungroup() %>% 
  left_join(nyc_covid, by = c("date"))

time_series_nested <- time_series %>% 
  #select(-c(state, county)) %>% 
  filter(!is.na(cases_new_rolling_14),
         !is.na(cases_new_rolling_14_nyc)) %>% 
  group_by(state, county) %>%
  nest() %>% 
  mutate(rsq = map_dbl(data, ~cor(.x$cases_new_rolling_14, .x$cases_new_rolling_14_nyc))^2) %>% 
  #hoist(data, date = "date")
  unnest_wider(data)

time_series_nested %>% 
  ggplot(aes(rsq)) +
  geom_histogram()

df_us_counties %>% 
  left_join(time_series_nested %>% select(state, county, rsq)) %>% 
  ggplot() +
  geom_sf(aes(fill = rsq)) +
  geom_sf(data = df_us_states, color = "white", fill = NA, inherit.aes = FALSE) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))

time_series_nested %>% 
  ungroup() %>% 
  select(state, county, rsq) %>% 
  mutate(id = str_c(state, county, sep = ", "),
         id = tidytext::reorder_within(x = id, by = rsq, within = state)) %>% 
  arrange(id) %>% 
  ggplot(aes(rsq, id)) +
  geom_point() +
  facet_wrap(~state, scales = "free_y") +
  tidytext::scale_y_reordered()