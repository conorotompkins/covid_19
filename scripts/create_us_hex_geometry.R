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

df_wv_counties <- df_us_counties %>% 
  filter(state == "West Virginia")

df_wv_counties %>% 
  ggplot(aes(fill = log10(value))) +
    geom_sf(color = NA) +
    scale_fill_viridis_c() +
    theme_void()


#create test set
df_us_counties %>% 
  slice(1:100)
  

#calculate grid
df_us_counties_hex <- calculate_grid(shape = df_us_counties, grid_type = "hexagonal", seed = 3)


class(df_us_counties_hex)

df_hex_centroids <- df_us_counties_hex[[1]]@coords %>% 
  as_tibble()

df_us_counties %>% 
  bind_cols(df_hex_centroids) %>% 
  ggplot(aes(x, y, color = log10(value))) +
    geom_point() +
    scale_color_viridis_c() +
    theme_void()

df_hex_centroids %>% 
  ggplot(aes(x, y)) +
    geom_point()

df_us_counties_hex[[1]] %>% 
  str()

df_us_counties_hex[[2]] %>% 
  .[1]

str(df_us_counties_hex)

df_us_counties_hex[[2]]$

str(df_us_counties_hex[[2]])

df_us_counties_hex[[2]]@ID2




#reassign polygons
df_us_counties_hex <- assign_polygons(df_us_counties, df_us_counties_hex) %>% 
  mutate(polygon_type = "hex")





