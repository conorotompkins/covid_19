library(tidyverse)
library(tidycensus)
library(sf)
library(janitor)
library(tilemaps)
library(rmapshaper)
library(lwgeom)

options(scipen = 999, digits = 2)

df_us_counties <- get_decennial(geography = "county", variables = "P001001", 
                                shift_geo = TRUE, geometry = TRUE) %>% 
  mutate(polygon_type = "normal") %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim),
         id = row_number())

df_test_states <- tibble(state = c("Pennsylvania", "West Virginia", "Ohio", "Virginia", "Maryland",
                                   "New York"))

df_index <- df_us_counties %>% 
  select(id, state, county, GEOID) %>% 
  st_drop_geometry() %>% 
  semi_join(df_test_states)

df_us_counties %>% 
  filter(state != "Hawaii",
         state != "Alaska") %>% 
  filter(state == "New York") %>% 
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = .001, keep_shapes = TRUE)) %>%
  mutate(geometry = st_buffer(geometry, 50000)) %>%
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = .001, keep_shapes = TRUE)) %>% 
  #mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE)) %>% 
  #filter(flag == TRUE) %>% 
  ggplot() +
  geom_sf() +
  #geom_sf(aes(geometry = tile_map)) +
  theme_void()
  
df_test <- df_us_counties %>% 
  filter(state == "New York",
         county != "Richmond County")

df_richmond <- df_us_counties %>% 
  filter(state == "New York",
         county == "Richmond County")


df_test %>% 
  mutate(flag = county == "Richmond County") %>% 
  ggplot() +
  geom_sf(aes(fill = flag))

df_test %>% 
  bind_rows(df_richmond) %>% 
  mutate(flag = county == "Richmond County") %>% 
  st_snap(df_richmond, tolerance = .01) %>% 
  st_difference(.) %>% 
  #mutate(geometry = rmapshaper::ms_simplify(geometry, keep = .001, keep_shapes = TRUE)) %>% 
  #mutate(geometry = st_make_valid(geometry)) %>% 
  #mutate(tile_map = generate_map(geometry, square = FALSE, flat_topped = TRUE)) %>% 
  #filter(flag == TRUE)
  ggplot() +
  geom_sf(aes(fill = flag))


  summarize(geometry_new = rmapshaper::ms_simplify(geometry, keep = .001, keep_shapes = FALSE))

df_us_counties %>% 
  bind_cols(df_index)
  ggplot() +
  geom_sf(aes(geometry = geometry_new))

df_hex <- df_us_counties %>% 
  mutate(tile_map = generate_map(geometry_new, square = FALSE, flat_topped = TRUE))

df_hex %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry_new))

df_hex %>% 
  ggplot() +
  geom_sf(aes(geometry = tile_map))
  

df_us_counties %>% 
  filter(state == "New York") %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  select(GEOID, area) %>% 
  arrange(-area)


df_us_counties %>% 
  filter(state == "New York") %>% 
  ggplot() +
  geom_sf(aes(fill = county), color = NA, show.legend = FALSE)
  
df_hex
  

df_hex %>% 
  ggplot() +
  geom_sf(aes(geometry = tile_map, fill = state)) +
  theme_void()

df_hex %>% 
  st_drop_geometry() %>% 
  group_by(state) %>% 
  summarize(tile_map = st_union(tile_map)) %>% 
  ggplot() +
  geom_sf(aes(geometry = tile_map)) +
  theme_void()
  


####snap hawaii to california


df_us_counties %>% 
  filter(state == "Hawaii") %>% 
  ms_dissolve("county") %>% 
  group_by(county) %>% 
  st_union() %>% 
  ggplot() +
  geom_sf()
  
  
  #st_cast(., "POLYGON") %>% 
  group_by(county) %>%
  summarize(geometry = ms_dissolve(geometry, field = "county")) %>% 
  #summarize(geometry = ms_simplify(geometry, keep = .1, snap = TRUE, keep_shapes = FALSE)) %>%
  ggplot() +
  geom_sf(aes(fill = county))
  
  
df_us_counties %>% 
  filter(state == "Hawaii") %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  mutate(geometry = st_snap_to_grid(geometry, size = 10000)) %>% 
  ggplot() +
  geom_sf()

df_us_counties %>% 
  filter(state == "New York") %>% 
  st_cast("POLYGON") %>%
  mutate(polygon_area = st_area(geometry) %>% str_remove(., "$ [m^2]") %>% as.numeric) %>%
  group_by(state, county) %>%
  filter(polygon_area == max(polygon_area)) %>%
  ungroup() %>%
  #fails here
  mutate(geometry = rmapshaper::ms_simplify(geometry, keep = .001, keep_shapes = TRUE, snap = TRUE, snap_interval = 1000)) %>% 
  # #mutate(geometry = st_buffer(geometry, 10000, joinStyle = "MITRE")) %>% 
  mutate(geometry = st_make_valid(geometry)) %>% 
  View()
  #st_snap(x = ., y = ., tolerance = 1) %>% 
  #mutate(tile_map = generate_map(geometry)) %>% 
  ggplot() +
  #geom_sf(aes(geometry = tile_map), color = NA) +
  geom_sf(show.legend = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"))
