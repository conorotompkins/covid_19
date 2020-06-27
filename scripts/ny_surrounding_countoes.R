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

theme_set(theme_ipsum(base_size = 15))

us_states <- df_us_counties <- get_decennial(geography = "state", variables = "P001001", 
                                             shift_geo = TRUE, geometry = TRUE)
ny <- us_states %>% 
  filter(NAME == "New York")

ny

ri <- us_states %>% 
  filter(NAME == "Rhode Island")

#st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

df <- us_states %>% 
  st_filter(x = ., y = ny,  .predicate = st_touches) %>% 
  bind_rows(ny) %>% 
  bind_rows(ri)

df %>% 
  ggplot() +
  geom_sf() +
  theme_void()

states <- df %>% 
  st_drop_geometry() %>% 
  distinct(NAME)

df_us_counties <- get_decennial(geography = "county", variables = "P001001", 
                                shift_geo = TRUE, geometry = TRUE) %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim)) %>% 
  semi_join(states, by = c("state" = "NAME"))

df_us_counties %>% 
  filter(str_detect(county, "New York"))

nyc_county_list <- c("New York County", "Kings County", "Queens County", "Bronx County", "Richmond County")

df_us_counties %>% 
  filter(state == "New York") %>% 
  filter(county %in% nyc_county_list) %>% 
  ggplot() +
  geom_sf(data = df_us_counties %>% filter(state == "New York")) +
  geom_sf(aes(fill = county))
  
df_us_counties %>% 
  filter(state == "New York") %>% 
  filter(county %in% nyc_county_list) %>% 
  summarize(value = sum(value)) %>% 
  ggplot() +
  geom_sf(data = df_us_counties %>% filter(state == "New York")) +
  geom_sf(fill = "red")

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

nyc_counties_combined

nyc_counties_combined %>% 
  ggplot() +
  geom_sf(aes(fill = value))
  
df_us_counties <- df_us_counties %>% 
  anti_join(nyc_counties) %>% 
  bind_rows(nyc_counties_combined)

df_us_counties %>% 
  ggplot() +
  geom_sf(aes(color = county == "New York City County", fill = value)) +
  scale_fill_viridis_c() +
  scale_color_manual(values = c("black", "red"))
  

# us_counties_hex_grid <- calculate_grid(shape = df_us_counties, grid_type = "hexagonal", seed = 1)
# 
# us_counties_hex <- assign_polygons(df_us_counties, us_counties_hex_grid)
# 
# us_counties_hex %>% 
#   ggplot(aes(fill = state)) +
#   geom_sf()

df_covid_raw <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  select(-fips) %>% 
  semi_join(states, by = c("state" = "NAME")) %>% 
  filter(county != "Unknown County")

df_covid_raw %>% 
  filter(state == "New York") %>% 
  distinct(county) %>% 
  View()

df_covid_raw %>% 
  filter(state == "New York") %>% 
  distinct(county)
  
ny_counties_census <- df_us_counties %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  filter(state == "New York") %>% 
  distinct(county)

ny_counties_covid <- df_covid_raw %>% 
  filter(state == "New York") %>% 
  distinct(county)

ny_counties_census %>% 
  anti_join(ny_counties_covid)

ny_counties_covid %>% 
  anti_join(ny_counties_census)

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

df_covid %>% 
  filter(is.na(date))
  
df_covid %>% 
  mutate(id = str_c(state, county, sep = ", ")) %>% 
  ggplot(aes(date, cases_new_rolling_14, color = state, group = id)) +
  geom_line(alpha = .5) +
  facet_wrap(~state, scales = "free_y")

df_us_states <- df_us_counties %>% 
  group_by(state) %>% 
  summarize()

df_us_states %>% 
  ggplot() +
  geom_sf()

df_us_counties %>% 
  left_join(df_covid) %>% 
  mutate(cases_new_rolling_14_per_capita = (cases_new_rolling_14 / value) * 100000) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  filter(county == "New York City County")
  
  
df_us_counties %>% 
  left_join(df_covid) %>% 
  mutate(cases_new_rolling_14_per_capita = (cases_new_rolling_14 / value) * 100000) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  filter(date == last(date)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = cases_new_rolling_14_per_capita)) +
  geom_sf(data = df_us_states, inherit.aes = FALSE, aes(geometry = geometry), fill = NA, color = "red") +
  scale_fill_viridis_c() +
  theme_void()
  
surrounding_ny_counties <- df_us_counties %>% 
  left_join(df_covid) %>% 
  mutate(cases_new_rolling_14_per_capita = (cases_new_rolling_14 / value) * 100000) %>% 
  arrange(state, county, date)

surrounding_ny_counties %>% 
  st_drop_geometry() %>% 
  arrange(!is.na(date)) %>% 
  View()

surrounding_ny_counties

surrounding_ny_counties %>% 
  anti_join(df_us_counties %>% st_drop_geometry(), by = c("state", "county"))

df_us_counties %>% 
  anti_join(surrounding_ny_counties %>% st_drop_geometry(), by = c("state", "county"))

surrounding_ny_counties_anim <- surrounding_ny_counties %>%
  filter(!is.na(cases_new_rolling_14_per_capita)) %>% 
  ggplot() +
  geom_sf(aes(fill = cases_new_rolling_14_per_capita)) +
  geom_sf(data = df_us_states, color = "white", fill = NA) +
  scale_fill_viridis_c() +
  labs(title = "Date: {frame_time}",
       subtitle = "New daily COVID-19 cases, adjusted per 100,000 population (2010 US Census)",
       fill = "14-day rolling average",
       caption = "@conor_tompkins, data from NYTimes") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 12)) +
  transition_time(date)

surrounding_ny_counties_anim %>% 
  anim_save(filename = "output/surounding_ny_animated_map.gif", renderer = gifski_renderer(),
            fps = 20, end_pause = 20, duration = 15, width = 1200, height = 800)
