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

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  arrange(state, date)

df_date_10_cases <- df %>% 
  select(state, date, cases) %>% 
  arrange(state, date) %>% 
  filter(cases >= 10) %>% 
  distinct(state, date) %>% 
  group_by(state) %>% 
  filter(date == first(date)) %>% 
  rename(date_at_10_cases = date)

df <- df %>% 
  left_join(df_date_10_cases) %>% 
  mutate(days_since_10_cases = date - date_at_10_cases)

df <- df %>% 
  group_by(state) %>% 
  mutate(cases_new = cases - lag(cases),
         cases_new = case_when(cases_new < 0 ~ lag(cases_new),
                               cases_new >= 0 ~ cases_new)) %>% 
  ungroup()

#calculate 7 day rolling average of cases_diff
df_rolling <- df %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
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
  select(state, date, days_since_10_cases, cases_new, contains("cases_new_rolling"))

df_rolling %>% 
  select(state, days_since_10_cases, cases_new, cases_new_rolling_14) %>% 
  filter(state %in% c("New York", "Florida", "Arizona", "South Carolina", "Arkansas")) %>% 
  ggplot(aes(x = days_since_10_cases, color = state, group = state)) +
  geom_point(aes(y = cases_new), alpha = .3, show.legend = FALSE) +
  geom_line(aes(y = cases_new_rolling_14), linetype = 1, size = 1.5) +
  labs(color = "7-day rolling average of new cases")


#animated
rolling_average <- df_rolling %>%  
  filter(days_since_10_cases > 0) %>% 
  group_by(state) %>% 
  mutate(line_7 = max(cases_new_rolling_7, na.rm = TRUE) / 2,
         line_14 = max(cases_new_rolling_14, na.rm = TRUE) / 2) %>% 
  ungroup()

rolling_average %>% 
  group_by(state) %>% 
  filter(days_since_10_cases == max(days_since_10_cases, na.rm = TRUE)) %>% 
  ggplot(aes(cases_new_rolling_7, cases_new_rolling_14, color = state)) +
  geom_label(aes(label = state), show.legend = FALSE)

rolling_average %>% 
  filter(state %in% c("New York", "Florida")) %>% 
  ggplot(aes(cases_new_rolling_7, cases_new_rolling_14, color = state)) +
  geom_path() +
  facet_wrap(~state, scales = "free") +
  labs(title = "{frame_along} days since 10th case",
       x = "7-day rolling average of new cases",
       y = "14-day rolling average of new cases") +
  guides(color = FALSE) +
  scale_x_comma() +
  scale_y_comma() +
  theme(panel.border = element_rect(color = "black", fill = NA))

rolling_average_anim <- rolling_average %>% 
  filter(state %in% c("New York", "Florida")) %>% 
  ggplot(aes(cases_new_rolling_7, cases_new_rolling_14, color = state)) +
  geom_vline(aes(xintercept = line_7), linetype = 2) +
  geom_hline(aes(yintercept = line_14), linetype = 2) +
  geom_path(size = 1.5) +
  geom_point() +
  facet_wrap(~state, scales = "free") +
  scale_x_comma() +
  scale_y_comma() +
  labs(title = "{frame_along} days since 10th COVID-19 case",
       x = "7-day rolling average of new cases",
       y = "14-day rolling average of new cases") +
  guides(color = FALSE) +
  transition_reveal(days_since_10_cases)

rolling_average_anim %>% 
  anim_save(filename = "output/rolling_average_animation.gif", end_pause = 20, renderer = gifski_renderer(),
            width = 1200, height = 800)