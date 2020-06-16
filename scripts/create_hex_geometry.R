library(tidyverse)
library(tidycensus)
library(janitor)
library(geogrid)

options(scipen = 999, digits = 2)

df_us_counties <- get_acs(geography = "county", variables = "B19013_001", 
                            shift_geo = TRUE, geometry = TRUE)

df_us_counties %>%
  ggplot() +
    geom_sf()

df_wv_counties <- df_us_counties %>% 
  filter(str_detect(NAME, "West Virginia"))

df_wv_counties %>% 
  ggplot(aes(fill = estimate)) +
    geom_sf() +
    #coord_sf() +
    theme_void()

new_cells_hex <- calculate_grid(shape = df_wv_counties, grid_type = "hexagonal", seed = 3)
resulthex <- assign_polygons(df_wv_counties, new_cells_hex)

resulthex %>% 
  ggplot(aes(fill = estimate)) +
    geom_sf() +
    theme_void()


