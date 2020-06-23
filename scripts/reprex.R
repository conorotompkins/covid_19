library(tidyverse)
library(tidycensus)
library(janitor)
library(geogrid)
library(tilemaps)

options(scipen = 999, digits = 2)

df_ny <- get_decennial(geography = "county", state = "New York", variables = "P001001", geometry = TRUE)

df_ny %>% 
  ggplot() +
  geom_sf()

ny_new_cells_hex <- calculate_grid(shape = df_ny, grid_type = "hexagonal", seed = 3)
df_ny_hex <- assign_polygons(df_ny, ny_new_cells_hex) 

df_ny_hex %>% 
  ggplot() +
  geom_sf()

#tilemap
df_pa <- get_decennial(geography = "county", state = "Pennsylvania", variables = "P001001", geometry = TRUE)

df_pa %>% 
  ggplot() +
  geom_sf() +
  theme_void()

df_pa %>% 
  mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE)) %>% 
  ggplot() +
  geom_sf(aes(geometry = tile_map)) +
  theme_void()
