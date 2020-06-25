library(tidyverse)
library(hrbrthemes)
library(tidycensus)
library(sf)

options(scipen = 999, digits = 2)

theme_set(theme_ipsum())

us <- get_decennial(geography = "state", variables = "P001001", geometry = TRUE, shift_geo = TRUE) %>% 
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(lon = st_coordinates(centroid)[, 1],
         lat = st_coordinates(centroid)[, 2]) %>% 
  arrange(lon, lat) %>% 
  mutate(NAME = fct_inorder(NAME))


us

us %>% 
  ggplot() +
  geom_sf() +
  geom_sf(aes(geometry = centroid))
  
us %>% 
  arrange(lon, lat) %>% 
  mutate(NAME = fct_inorder(NAME)) %>% 
  ggplot(aes(x = runif(52), y = NAME)) +
  geom_point()


df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

df %>% 
  arrange(state, date) %>% 
  group_by(date) %>% 
  mutate(cases_cumulative = cumsum(cases)) %>% 
  ggplot(aes(date, cases_cumulative, group = state)) +
  geom_line(alpha = .1)

df %>% 
  ggplot(aes(date, cases, group = state)) +
  geom_line(alpha = .3) +
  scale_y_comma()

df %>% 
  distinct(state) %>% 
  full_join(us, by = c("state" = "NAME")) %>% 
  View()

state_fct <- us %>% 
  pull(NAME)


#calculate 14 day rolling average of cases_diff


df %>% 
  mutate(state = factor(state, levels = state_fct)) %>% 
  complete(state, date, fill = list(cases = NA)) %>% 
  group_by(state) %>% 
  mutate(cases_diff = cases - lag(cases)) %>% 
  ggplot(aes(date, state, fill = cases_diff)) +
  geom_tile() +
  scale_fill_viridis_c() +
  scale_x_date(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))

df_recent <- df %>% 
  group_by(state) %>% 
  mutate(cases_diff = cases - lag(cases)) %>% 
  filter(date == last(date))

us %>% 
  filter(NAME %in% state.name) %>% 
  left_join(df_recent, by = c("NAME" = "state")) %>% 
  ggplot(aes(fill = cases_diff)) +
  geom_sf() +
  scale_fill_viridis_c()


df %>% 
  select(state, date, cases) %>% 
  arrange(state, date) %>% 
  View()
