library(tidyverse)
library(tidycensus)
library(sf)
library(janitor)
library(geogrid)
library(doParallel)

options(scipen = 999, digits = 2)

df_us_counties <- get_decennial(geography = "county", variables = "P001001", 
                                shift_geo = TRUE, geometry = TRUE) %>% 
  mutate(polygon_type = "normal") %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim))

df_us_counties_test <- df_us_counties %>% 
  select(GEOID)

df_us_counties_test_hex <- calculate_grid(shape = df_us_counties_test, grid_type = "hexagonal", seed = 3)

df_us_counties_hex <- assign_polygons(df_us_counties_test, df_us_counties_test_hex)

sf::write_sf(df_us_counties_hex, "output/df_us_counties_hex.shp")
