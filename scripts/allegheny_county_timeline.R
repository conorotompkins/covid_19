library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)

theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

df <- read_csv("https://raw.githubusercontent.com/FranklinChen/covid-19-allegheny-county/master/covid-19-allegheny-county.csv") %>% 
  mutate(state = "Pennsylvania",
         county = "Allegheny County") %>% 
  mutate(cases = case_when(cases < 0 ~ 0,
                           cases >= 0 ~ cases),
         hospitalizations = case_when(hospitalizations < 0 ~ 0,
                                      hospitalizations >= 0 ~ hospitalizations),
         deaths = case_when(deaths < 0 ~ 0,
                            deaths >= 0 ~ deaths)) %>% 
  mutate(cases_new = cases - lag(cases),
         hospitalizations_new = hospitalizations - lag(hospitalizations),
         deaths_new = deaths - lag(deaths))

df <- df %>% 
  mutate(cases_new = case_when(cases_new < 0 ~ NA_real_,
                               cases_new >= 0 ~ cases_new),
         hospitalizations_new = case_when(hospitalizations_new < 0 ~ NA_real_,
                                          hospitalizations_new >= 0 ~ hospitalizations_new),
         deaths_new = case_when(deaths_new < 0 ~ NA_real_,
                                deaths_new >= 0 ~ deaths_new))
  
df <- df %>% 
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
  tq_mutate(
    # tq_mutate args
    select     = hospitalizations_new,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 14,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "hospitalizations_new_rolling_14"
  ) %>% 
  tq_mutate(
    # tq_mutate args
    select     = deaths_new,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 14,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "deaths_new_rolling_14"
  ) %>% 
  select(state, county, date, contains("_new"), contains("rolling"))

df



df_rolling <- df %>% 
  select(state, county, date, contains("rolling")) %>% 
  pivot_longer(cols = contains("rolling"), names_to = "metric") %>% 
  mutate(metric = case_when(str_detect(metric, "cases") ~ "New cases",
                            str_detect(metric, "deaths") ~ "New deaths",
                            str_detect(metric, "hospitalizations") ~ "New hospitalizations")) %>% 
  mutate(metric = factor(metric, levels = c("New cases", "New hospitalizations", "New deaths")))


df_new <- df %>% 
  select(state, county, date, !contains("rolling")) %>% 
  pivot_longer(cols = contains("_new"), names_to = "metric") %>% 
  mutate(metric = case_when(str_detect(metric, "cases") ~ "New cases",
                            str_detect(metric, "deaths") ~ "New deaths",
                            str_detect(metric, "hospitalizations") ~ "New hospitalizations")) %>% 
  mutate(metric = factor(metric, levels = c("New cases", "New hospitalizations", "New deaths")))
        
df_new <- df_new %>% 
  arrange(state, county, metric, date) %>% 
  group_by(state, county, metric) %>% 
  filter(row_number() != 1) %>% 
  mutate(first_non_zero_value = cumsum(coalesce(value, 0) > 0) >= 1) %>% 
  ungroup() %>% 
  filter(first_non_zero_value == TRUE)

df_rolling %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~metric, ncol = 1, scales = "free_y")

df_new %>% 
  ggplot(aes(date, value)) +
  geom_point() +
  facet_wrap(~metric, ncol = 1, scales = "free_y")

last_updated <- last(df_rolling$date)

df_max_values <- df_rolling %>% 
  group_by(metric) %>% 
  summarize(value_max = last(value)) %>% 
  ungroup()

allegheny_county_timeline <- df_rolling %>% 
  filter(!is.na(value)) %>% 
  left_join(df_max_values) %>% 
  ggplot(aes(date, value)) +
  annotate(geom = "rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-05-15"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "red", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-05-15"), xmax = ymd("2020-06-05"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "yellow", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-06-05"), xmax = ymd("2020-06-28"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "green", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-06-28"), xmax = as.Date(Inf), ymin = as.Date(-Inf), ymax = as.Date(Inf),
           fill = "#aaff00", alpha = .3) +
  geom_point(data = df_new, aes(y = value), alpha = .3)+
  geom_line(size = 1.5) +
  #geom_hline(aes(yintercept = value_max), linetype = 2) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  labs(title = str_c("Allegheny County COVID-19 response timeline (last updated ", last_updated, ")"),
       x = NULL,
       y = NULL,
       subtitle = "14-day rolling average",
       caption = "@conor_tompkins, data from Allegheny County via Franklin Chen")

allegheny_county_timeline

allegheny_county_timeline %>% 
  ggsave(filename = str_c("output/ac_timeline/allegheny_county_timeline_", last_updated, ".png"), width = 12, height = 10)



