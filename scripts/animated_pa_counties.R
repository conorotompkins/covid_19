library(tidyverse)
library(tidycensus)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(gganimate)
library(gifski)

theme_set(theme_ipsum(base_size = 15, axis_title_size = 15, caption_size = ))

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  filter(state == "Pennsylvania",
         !str_detect(county, "Unknown")) %>% 
  select(-fips)

df <- df %>% 
  complete(state, county, date) %>% 
  replace_na(list(cases = 0, deaths = 0)) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  ungroup() %>% 
  replace_na(list(cases_new = 0))

df <- df %>% 
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

df

df_recent <- df %>% 
  filter(state == "Pennsylvania") %>% 
  group_by(county) %>% 
  filter(date == last(date))

pa_counties <- get_decennial(geography = "county", state = "Pennsylvania", variables = "P001001", geometry = TRUE) %>% 
  separate(NAME, into = c("county", "state"), sep = ", ")

pa_counties %>% 
  left_join(df_recent, by = c("county" = "county")) %>% 
  ggplot() +
  geom_sf(aes(fill = cases_new_rolling_14), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "New COVID-19 cases in Pennsylvania",
    fill = "14-day rolling average") +
  theme_void()

pa_counties %>% 
  left_join(df) %>% 
  ggplot(aes(date, cases, group = county)) +
  geom_line(alpha = .5)

pa_counties %>% 
  left_join(df) %>% 
  ggplot(aes(date, cases_new, group = county)) +
  geom_line(alpha = .5)

pa_counties %>% 
  ggplot(aes(fill = value)) +
  geom_sf() +
  scale_fill_viridis_c()

pa_counties %>% 
  left_join(df, by = c("county" = "county", "state" = "state")) %>% 
  mutate(cases_new_rolling_14_per_capita = (cases_new_rolling_14 / value) * 100000) %>% 
  View()

pa_animated_map <- pa_counties %>% 
  left_join(df, by = c("county" = "county", "state" = "state")) %>%
  mutate(cases_new_rolling_14_per_capita = (cases_new_rolling_14 / value) * 100000) %>%
  ggplot() +
  geom_sf(aes(fill = cases_new_rolling_14_per_capita), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Date: {frame_time}",
       subtitle = "New COVID-19 cases per 100,000 (2010 Census)",
       fill = "14-day rolling average",
       caption = "@conor_tompkins, data from NYTimes") +
  theme_void(base_size = 15) +
  transition_time(date)

#pa_animated_map

pa_animated_map %>% 
  anim_save(filename = "output/pa_animated_map.gif", end_pause = 20, renderer = gifski_renderer(),
            width = 1200, height = 800)

