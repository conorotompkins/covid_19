library(tidyverse)
library(tidycensus)
library(sf)
library(janitor)

options(scipen = 999, digits = 2)

theme_set(theme_void())

#steps
#identify counties that need to be merged
#create new df with summarized geometry for NYC counties
#create new national county df with one geometry for NYC counties

us_states <- df_us_counties <- get_decennial(geography = "state", variables = "P001001", 
                                             shift_geo = TRUE, geometry = TRUE)

us_states %>% 
  ggplot() +
  geom_sf()

us_counties <- get_decennial(geography = "county", variables = "P001001", 
                             shift_geo = TRUE, geometry = TRUE) %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim))

us_counties %>% 
  ggplot() +
  geom_sf()

#identify counties that need to be merged
nyc_county_list <- c("New York County", "Kings County", "Queens County", "Bronx County", "Richmond County")

nyc_counties <- us_counties %>% 
  st_drop_geometry() %>% 
  filter(state == "New York") %>% 
  filter(county %in% nyc_county_list) %>% 
  select(state, county) %>% 
  mutate(type = "NYC counties")

us_counties %>% 
  semi_join(nyc_counties) %>% 
  ggplot() +
  geom_sf(data = us_counties %>% filter(state == "New York")) +
  geom_sf(aes(fill = county))

#demo what it looks like post-merge
us_counties %>% 
  semi_join(nyc_counties) %>% 
  summarize(value = sum(value)) %>% 
  ggplot() +
  geom_sf(data = us_counties %>% filter(state == "New York")) +
  geom_sf(fill = "red")



#create new df with summarized geometry
nyc_counties_combined <- us_counties %>% 
  semi_join(nyc_counties) %>% 
  summarize(value = sum(value)) %>% 
  mutate(state = "New York",
         county = "New York City County")

nyc_counties_combined

nyc_counties_combined %>% 
  ggplot() +
  geom_sf()

#remove individual NYC counties from the main county df, replace with single merged NYC county
us_counties <- us_counties %>% 
  anti_join(nyc_counties) %>% 
  bind_rows(nyc_counties_combined)


us_counties %>% 
  filter(state == "New York") %>% 
  ggplot() +
  geom_sf(aes(color = county == "New York City County", fill = value)) +
  scale_fill_viridis_c() +
  scale_color_manual(values = c(NA, "red")) +
  theme_void()
