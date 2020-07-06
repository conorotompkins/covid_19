library(tidyverse)
library(janitor)
library(hrbrthemes)
library(janitor)

options(scipen = 999, digits = 2)

theme_set(theme_ipsum(base_size = 20, axis_title_size = 18, caption_size = 16, subtitle_size = 16))


covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  arrange(state, county, date)

covid_unknowns <- covid %>% 
  mutate(flag_unknown = case_when(county == "Unknown County" ~ TRUE,
                                  county != "Unknown County" ~ FALSE)) %>% 
  group_by(state, flag_unknown) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup()


covid_unknowns %>% 
  group_by(state) %>% 
  mutate(pct_unknown = cases / sum(cases)) %>% 
  #filter(state == "Pennsylvania") %>% 
  ggplot(aes(pct_unknown, state, fill = flag_unknown)) +
  geom_col()

bar_chart <- covid_unknowns %>% 
  group_by(state) %>% 
  mutate(pct_unknown = cases / sum(cases)) %>% 
  ungroup() %>% 
  filter(flag_unknown == TRUE,
         pct_unknown < 1) %>% 
  mutate(state = fct_reorder(state, pct_unknown)) %>% 
  ggplot(aes(pct_unknown, state)) +
  geom_col() +
  scale_x_percent() +
  labs(title = "NYTimes COVID-19 case database",
       subtitle = "Cases with unknown county of residence",
       x = "Percent of cases",
       y = NULL,
       caption = "@conor_tompkins, data from NYTimes")

bar_chart %>% 
  ggsave(filename = "output/unknown_bar_chart.png", height = 14, width = 8)

covid_unknowns %>% 
  filter(state %in% state.name) %>% 
  group_by(state) %>% 
  mutate(pct_unknown = cases / sum(cases)) %>% 
  pivot_wider(id_cols = state, names_from = flag_unknown, values_from = pct_unknown) %>% 
  clean_names() %>% 
  ggplot(aes(false, true)) +
  geom_point()
