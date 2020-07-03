library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)

theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

df <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  mutate(county = str_c(county, "County", sep = " ")) %>% 
  arrange(state, county, date) %>% 
  group_by(state, county) %>% 
  mutate(cases_new = cases - lag(cases)) %>% 
  ungroup() %>% 
  filter(state == "Pennsylvania",
         county == "Allegheny County") %>% 
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
    col_rename = "cases_new_rolling_14"
  ) %>% 
  ungroup()

df %>% 
  ggplot(aes(date, cases)) +
  geom_line()

df %>% 
  ggplot(aes(date, cases_new)) +
  geom_line()

last_updated <- last(df$date)

allegheny_county_timeline <- df %>% 
  ggplot(aes(date, cases_new_rolling_14)) +
  annotate(geom = "rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-05-15"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "red", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-05-15"), xmax = ymd("2020-06-05"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "yellow", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-06-05"), xmax = ymd("2020-06-28"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "green", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-06-28"), xmax = as.Date(Inf), ymin = as.Date(-Inf), ymax = as.Date(Inf),
           fill = "#aaff00", alpha = .3) +
  #geom_vline(xintercept = last_updated, linetype = 2) +
  geom_point(aes(y = cases_new), alpha = .6) +
  geom_line(size = 1.5) +
  facet_wrap(~str_c(county, state, sep = ", ")) +
  labs(title = str_c("COVID-19 response timeline (last updated ", last_updated, ")"),
       x = NULL,
       y = "New cases",
       subtitle = "Black line indicates 14-day rolling average",
       caption = "@conor_tompkins, data from NYTimes")

allegheny_county_timeline

allegheny_county_timeline %>% 
  ggsave(filename = str_c("output/ac_timeline/allegheny_county_timeline_", last_updated, ".png"), width = 12, height = 10)
