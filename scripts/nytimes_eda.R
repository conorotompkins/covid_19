library(tidyverse)
library(hrbrthemes)
library(tidycensus)
library(sf)
library(tidyquant)
library(gganimate)
library(gifski)

options(scipen = 999, digits = 2)

theme_set(theme_ipsum())

us_not_shifted <- get_decennial(geography = "state", variables = "P001001", geometry = TRUE) %>%
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2]) %>% 
  arrange(lon, lat) %>% 
  mutate(NAME = fct_inorder(NAME))

state_fct <- us_not_shifted %>% 
  pull(NAME) %>% 
  levels()

us_shifted <- get_decennial(geography = "state", variables = "P001001", geometry = TRUE, shift_geo = TRUE) %>%
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2]) %>% 
  arrange(lon, lat) %>% 
  mutate(NAME = fct_inorder(NAME))

us_shifted

us_shifted %>% 
  ggplot() +
  geom_sf() +
  geom_sf(aes(geometry = centroid))
  
df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  arrange(state, date)

df_date_10_cases <- df %>% 
  select(state, date, cases) %>% 
  arrange(state, date) %>% 
  filter(cases >= 10) %>% 
  distinct(state, date) %>% 
  group_by(state) %>% 
  filter(date == first(date)) %>% 
  rename(date_at_10_cases = date)

df <- df %>% 
  left_join(df_date_10_cases) %>% 
  mutate(days_since_10_cases = date - date_at_10_cases)

df <- df %>% 
  group_by(state) %>% 
  mutate(cases_new = cases - lag(cases),
         cases_new = case_when(cases_new < 0 ~ lag(cases_new),
                               cases_new >= 0 ~ cases_new)) %>% 
  ungroup()


#line charts
df %>% 
  ggplot(aes(date, cases, group = state)) +
  geom_line(alpha = .5) +
  scale_y_comma()

df %>% 
  filter(days_since_10_cases > 0) %>% 
  ggplot(aes(days_since_10_cases, cases, group = state)) +
  geom_line(alpha = .5) +
  scale_y_comma()

df %>% 
  filter(days_since_10_cases >= 0) %>% 
  ggplot(aes(days_since_10_cases, cases_new, group = state)) +
  geom_line(alpha = .5) +
  scale_y_comma()

#tile charts
df %>% 
  mutate(state = factor(state, levels = state_fct)) %>% 
  complete(state, date, fill = list(cases = NA)) %>% 
  semi_join(us_shifted %>% distinct(NAME), by = c("state" = "NAME")) %>% 
  left_join(us_shifted %>% select(NAME, value) %>% st_drop_geometry(), by = c("state" = "NAME")) %>% 
  mutate(cases_new_per_capita = (cases_new / value) * 100000) %>% 
  complete(state, days_since_10_cases, fill = list(cases_new_per_capita = NA)) %>% 
  filter(state != "NA",
         state != "Puerto Rico",
         days_since_10_cases > 0) %>% 
  ggplot(aes(days_since_10_cases, state, fill = cases_new_per_capita)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_fixed(ratio = 5) +
  labs(x = "Days since 10th COVID-19 case",
       y = NULL,
       fill = "New cases per 100,000") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#map
df_recent <- df %>% 
  group_by(state) %>% 
  filter(date == last(date))

us_shifted %>% 
  filter(NAME %in% state.name) %>% 
  left_join(df_recent, by = c("NAME" = "state")) %>% 
  ggplot(aes(fill = cases_new)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void()