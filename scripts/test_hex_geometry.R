library(tidyverse)
library(tidycensus)
library(janitor)
library(geogrid)

options(scipen = 999, digits = 2)

df_us_counties <- get_decennial(geography = "county", variables = "P001001", 
                            shift_geo = TRUE, geometry = TRUE) %>% 
  mutate(polygon_type = "normal") %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim))

df_us_counties %>%
  ggplot(aes(fill = value)) +
    geom_sf(color = NA) +
    scale_fill_viridis_c() +
    theme_void()

#wv
df_wv_counties <- df_us_counties %>% 
  filter(state == "West Virginia")

df_wv_counties %>% 
  ggplot(aes(fill = value)) +
    geom_sf() +
    scale_fill_viridis_c() +
    theme_void()

wv_new_cells_hex <- calculate_grid(shape = df_wv_counties, grid_type = "hexagonal", seed = 3)
df_wv_counties_hex <- assign_polygons(df_wv_counties, wv_new_cells_hex) %>% 
  mutate(polygon_type = "hex")

df_wv_counties_hex %>% 
  ggplot(aes(fill = value)) +
    geom_sf() +
    scale_fill_viridis_c() +
    theme_void()

df_wv_counties %>% 
  bind_rows(df_wv_counties_hex) %>% 
  ggplot(aes(fill = value)) +
    geom_sf() +
    facet_wrap(~str_c("Polygon type:", polygon_type, sep = " ")) +
    scale_fill_viridis_c() +
    theme_void()

#us
df_northeast <- df_us_counties %>% 
  filter(str_detect(state, "Maine|New Hampshire|Vermont|Massachusetts|New York|Conne|Rhode"))

df_northeast %>% 
  ggplot(aes(fill = value)) +
    geom_sf(color = NA) +
    scale_fill_viridis_c() +
    theme_void()

northeast_new_cells_hex <- calculate_grid(shape = df_northeast, grid_type = "hexagonal", seed = 3)

df_northeast_hex <- assign_polygons(df_northeast, northeast_new_cells_hex) %>% 
  mutate(polygon_type = "hex")

df_northeast_hex %>% 
  ggplot(aes(fill = value)) +
    geom_sf(color = NA) +
    scale_fill_viridis_c() +
    theme_void()


df_top_pop <- df_northeast %>% 
  #filter(state == "New York") %>% 
  arrange(-value) %>% 
  sf::st_drop_geometry() %>% 
  slice(1:10) %>% 
  select(state, county) %>% 
  mutate(top_pop_flag = TRUE)

df_northeast %>% 
  bind_rows(df_northeast_hex) %>% 
  left_join(df_top_pop, by = c("state", "county")) %>% 
  replace_na(list(top_pop_flag = FALSE)) %>% 
  # mutate(nyc_flag = case_when(state == "New York" & str_detect(county, "New York County|Kings|Queens|Suffolk|Bronx|Nassau") ~ TRUE,
  #                             TRUE ~ FALSE)) %>% 
  ggplot(aes(fill = value, color = top_pop_flag, size = top_pop_flag)) +
    geom_sf() +
    facet_wrap(~polygon_type) +
    scale_color_manual(values = c("black", "red")) +
    scale_size_discrete(range = c(.5, 1)) +
    scale_fill_viridis_c() +
    theme_void()


#calculate grid
df_us_counties_test_hex <- calculate_grid(shape = df_us_counties_test, grid_type = "hexagonal", seed = 3)

#reassign polygons
df_us_counties_hex <- assign_polygons(df_us_counties_test, df_us_counties_test_hex) %>% 
  mutate(polygon_type = "hex")

df_us_counties_hex %>% 
  ggplot() +
  geom_sf() +
  theme_void()


#test wv and PA
df_wv <- df_us_counties %>% 
  filter(state == "West Virginia")

df_pa <- df_us_counties %>% 
  filter(state == "Pennsylvania")

#wv
df_wv_hex <- calculate_grid(shape = df_wv, grid_type = "hexagonal", seed = 3)

df_wv_hex <- assign_polygons(df_wv, df_wv_hex) %>% 
  mutate(polygon_type = "hex")

#pa
df_pa_hex <- calculate_grid(shape = df_pa, grid_type = "hexagonal", seed = 3)

df_pa_hex <- assign_polygons(df_pa, df_pa_hex) %>% 
  mutate(polygon_type = "hex")

df_wv_hex %>% 
  bind_rows(df_pa_hex) %>% 
  ggplot(aes(fill = state)) +
  geom_sf()

#inspect
class(df_us_counties_test_hex)

df_hex_centroids <- df_us_counties_test_hex[[1]]@coords %>% 
  as_tibble()

df_us_counties_test %>% 
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
