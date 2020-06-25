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
  
df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

df %>% 
  ggplot(aes(date, cases, group = state)) +
  geom_line(alpha = .5)

df %>% 
  distinct(state) %>% 
  full_join(us_shifted %>% distinct(NAME) %>% st_drop_geometry(), by = c("state" = "NAME")) %>% 
  View()

state_fct <- us_not_shifted %>% 
  pull(NAME) %>% 
  levels()

#calculate 7 day rolling average of cases_diff
df_rolling <- df %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  tq_mutate(
    # tq_mutate args
    select     = cases_new,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 7,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "cases_new_rolling_7"
  ) %>% 
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
    col_rename = "cases_new_rolling_14"
  ) %>%
  ungroup() %>% 
  select(state, date, cases_new, contains("cases_new_rolling"))

df_rolling %>% 
  filter(state %in% c("New York", "Florida", "Arizona", "South Carolina", "Arkansas")) %>% 
  ggplot(aes(x = date, color = state, group = state)) +
  geom_point(aes(y = cases_new), alpha = .3) +
  geom_line(aes(y = cases_new_rolling_14), linetype = 2) +
  geom_line(aes(y = cases_new_rolling_7))

rolling_average <- df_rolling %>% 
  left_join(df_date_10_cases) %>% 
  mutate(days_since_10_cases = date - date_at_10_cases,
         days_since_10_cases = as.integer(days_since_10_cases)) %>% 
  filter(state == "New York") %>% 
  filter(state %in% c("New York", "Florida", "Arizona", "South Carolina", "Arkansas", "Pennsylvania")) %>% 
  filter(days_since_10_cases > 0)

line_7 <- rolling_average %>% 
  summarize(line_7 = max(cases_new_rolling_7, na.rm = TRUE) / 2) %>% 
  pull(line_7)

line_14 <- rolling_average %>% 
  summarize(line_14 = max(cases_new_rolling_14, na.rm = TRUE) / 2) %>% 
  pull(line_14)

rolling_average_anim <- rolling_average %>% 
  ggplot(aes(cases_new_rolling_7, cases_new_rolling_14, color = state)) +
  geom_vline(xintercept = line_7, linetype = 2) +
  geom_hline(yintercept = line_14, linetype = 2) +
  #geom_abline(linetype = 2) +
  geom_line() +
  geom_point() +
  facet_wrap(~state, scales = "free") +
  labs(title = "{frame_along} days since 10th case",
       x = "7-day rolling average of new cases",
       y = "14-day rolling average of new cases") +
  guides(color = FALSE) +
  transition_reveal(days_since_10_cases)

rolling_average_anim %>% 
  anim_save(filename = "output/rolling_average_animation.gif", renderer = gifski_renderer())
  

df %>% 
  mutate(state = factor(state, levels = state_fct)) %>% 
  complete(state, date, fill = list(cases = NA)) %>% 
  group_by(state) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  ungroup() %>% 
  filter(state != "NA") %>% 
  ggplot(aes(date, state, fill = cases_new)) +
  geom_tile(aes(color = state == "Hawaii")) +
  scale_fill_viridis_c() +
  scale_x_date(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_color_manual(values = c(NA, "red"))

df_recent <- df %>% 
  group_by(state) %>% 
  mutate(cases_diff = cases - lag(cases)) %>% 
  filter(date == last(date))

us_shifted %>% 
  filter(NAME %in% state.name) %>% 
  left_join(df_recent, by = c("NAME" = "state")) %>% 
  ggplot(aes(fill = cases_diff)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c()


df_date_10_cases <- df %>% 
  select(state, date, cases) %>% 
  filter(cases >= 10) %>% 
  distinct(state, date) %>% 
  group_by(state) %>% 
  filter(date == first(date)) %>% 
  rename(date_at_10_cases = date)

df %>% 
  left_join(df_date_10_cases) %>% 
  mutate(days_since_10_cases = date - date_at_10_cases) %>% 
  filter(days_since_10_cases >= 0) %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  ungroup() %>% 
  mutate(state = factor(state, levels = state_fct)) %>% 
  filter(state != "NA") %>% 
  ggplot(aes(days_since_10_cases, state, fill = cases_new)) +
  geom_tile() +
  scale_fill_viridis_c()

df %>% 
  left_join(df_date_10_cases) %>% 
  mutate(days_since_10_cases = date - date_at_10_cases) %>% 
  filter(days_since_10_cases >= 0) %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  mutate(cases_new = case_when(cases_new < 0 ~ lag(cases_new),
                               cases_new >= 0 ~ cases_new)) %>% 
  ungroup() %>% 
  mutate(state = factor(state, levels = state_fct)) %>% 
  filter(state != "NA") %>%
  ggplot(aes(days_since_10_cases, cases_new, group = state)) +
  geom_line(alpha = .5)
