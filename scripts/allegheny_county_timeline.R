#load libraries
library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(patchwork)

#settings
theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

options(scipen = 999, digits = 4)

bad_data <- tribble(
  ~metric, ~date,
  "deaths", "2020-07-07",
  "deaths", "2020-06-04",
  "deaths", "2020-05-13",
  "deaths", "2020-04-16",
  "deaths", "2020-06-24",
  "cases", "2020-07-14",
  "cases", "2020-07-13"
) %>% 
  mutate(metric = str_c(metric, "_new"),
         date = ymd(date),
         flag_bad_data = TRUE)

#read in data
df <- read_csv("https://raw.githubusercontent.com/FranklinChen/covid-19-allegheny-county/master/covid-19-allegheny-county.csv") %>% 
  mutate(state = "Pennsylvania",
         county = "Allegheny County") 

#glimpse(df)

#replace deaths on July 7th with NA
#The deaths reported today are from the state’s use of the Electronic Data Reporting System (EDRS) and include #deaths from April 5 – June 13, all decedents were 65 or older.
#https://twitter.com/HealthAllegheny/status/1280517051589722117?s=20




# df <- df %>% 
#   mutate(deaths = case_when(date == "2020-07-07" ~ NA_real_,
#                             date != "2020-07-07" ~ deaths))

#replace deaths on June 4th with NA
# This is the COVID-19 Daily Update or June 4, 2020. 
# The data reflected in these updates include information reported to the department
# in the past 24 hours as well as data since March 14 when the first case was reported in the county.
#https://twitter.com/HealthAllegheny/status/1268558422242463748?s=20
# df <- df %>% 
#   mutate(deaths = case_when(date == "2020-06-04" ~ NA_real_,
#                             date != "2020-06-04" ~ deaths))


#The data reflected in these updates include info reported in the past 24 hours
#as well as data since March 14 when the first case was reported in the county.
#https://twitter.com/HealthAllegheny/status/1260587935289704449?s=20
# df <- df %>% 
#   mutate(deaths = case_when(date == "2020-05-13" ~ NA_real_,
#                             date != "2020-05-13" ~ deaths))

#https://twitter.com/HealthAllegheny/status/1250801169989074944?s=20
# df <- df %>% 
#   mutate(deaths = case_when(date == "2020-04-16" ~ NA_real_,
#                             date != "2020-04-16" ~ deaths))

#https://twitter.com/HealthAllegheny/status/1275805982522855424?s=20
# df <- df %>% 
#   mutate(deaths = case_when(date == "2020-06-24" ~ NA_real_,
#                             date != "2020-06-24" ~ deaths))


df <- df %>% 
  ##replace negative case/hospitalization/death values with 0
  mutate(cases = case_when(cases < 0 ~ 0,
                           cases >= 0 ~ cases),
         hospitalizations = case_when(hospitalizations < 0 ~ 0,
                                      hospitalizations >= 0 ~ hospitalizations),
         deaths = case_when(deaths < 0 ~ 0,
                            deaths >= 0 ~ deaths)) %>% 
  ##calculate new cases/hospitalizations/deaths
  mutate(cases_new = cases - lag(cases),
         hospitalizations_new = hospitalizations - lag(hospitalizations),
         deaths_new = deaths - lag(deaths))

#when new cases/hospitalizations/deaths is negative, replace with NA
df <- df %>% 
  mutate(cases_new = case_when(cases_new < 0 ~ NA_real_,
                               cases_new >= 0 ~ cases_new),
         hospitalizations_new = case_when(hospitalizations_new < 0 ~ NA_real_,
                                          hospitalizations_new >= 0 ~ hospitalizations_new),
         deaths_new = case_when(deaths_new < 0 ~ NA_real_,
                                deaths_new >= 0 ~ deaths_new))

#calculate rolling 14 day averages for cases/hospitalizations/deaths
df <- df %>% 
  select(state, county, date, contains("new")) %>% 
  pivot_longer(cols = contains("new"), names_to = "metric", values_to = "value_new") %>% 
  left_join(bad_data) %>% 
  replace_na(list(flag_bad_data = FALSE)) %>% 
  mutate(value_new_clean = case_when(flag_bad_data == TRUE ~ NA_real_,
                                     flag_bad_data == FALSE ~ value_new)) %>% 
  group_by(state, county, metric) %>% 
  tq_mutate(
    # tq_mutate args
    select     = value_new_clean,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 14,
    align      = "right",
    FUN        = mean,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "value_new_rolling_14"
  ) %>% 
  ungroup()

#glimpse(df)


#pivot rolling average data longer
df_rolling <- df %>% 
  select(state, county, metric, date, contains("rolling")) %>% 
  #pivot_longer(cols = contains("rolling"), names_to = "metric") %>% 
  mutate(metric = case_when(str_detect(metric, "cases") ~ "New cases",
                            str_detect(metric, "deaths") ~ "New deaths",
                            str_detect(metric, "hospitalizations") ~ "New hospitalizations")) %>% 
  mutate(metric = factor(metric, levels = c("New cases", "New hospitalizations", "New deaths")))

df_rolling %>% 
  write_csv("output/ac_timeline/data/ac_rolling_data_cleaned.csv")

#glimpse(df_rolling)

#pivot daily data longer
df_new <- df %>% 
  select(state, county, date, !contains("rolling")) %>% 
  #pivot_longer(cols = contains("_new"), names_to = "metric") %>% 
  mutate(metric = case_when(str_detect(metric, "cases") ~ "New cases",
                            str_detect(metric, "deaths") ~ "New deaths",
                            str_detect(metric, "hospitalizations") ~ "New hospitalizations")) %>% 
  mutate(metric = factor(metric, levels = c("New cases", "New hospitalizations", "New deaths")))

#first(df_new$date)

#identify first non-zero value in each metric.
##filter out rows before first non-zero value
df_new <- df_new %>% 
  arrange(state, county, metric, date) %>% 
  mutate(value_new_clean = value_new) %>% 
  group_by(state, county, metric) %>% 
  mutate(value_new_clean = case_when(row_number() == 1 & is.na(value_new_clean) ~ 0,
                           TRUE ~ value_new_clean),
         after_first_non_zero_value = cumsum(coalesce(value_new_clean, 0) > 0) >= 1,
         value_new_clean = case_when(after_first_non_zero_value == FALSE ~ 0,
                           TRUE ~ value_new_clean)) %>% 
  ungroup()

#first(df_new$date)

df_new <- df_new %>% 
  group_by(metric) %>% 
  mutate(cumulative_max = cummax(coalesce(value_new_clean, 0)),
         new_record = value_new_clean == cumulative_max,
         new_record = case_when(after_first_non_zero_value == FALSE ~ FALSE,
                                after_first_non_zero_value == TRUE & flag_bad_data == TRUE ~ FALSE,
                                after_first_non_zero_value == TRUE & flag_bad_data == FALSE ~ new_record)) %>% 
  ungroup()

df_new <- df_new %>% 
  mutate(value_new_clean = case_when(after_first_non_zero_value == FALSE ~ NA_real_,
                                 TRUE ~ value_new_clean))

# first(df_new$date)

#preview rolling data
# df_rolling %>% 
#   ggplot(aes(date, value)) +
#   geom_line() +
#   facet_wrap(~metric, ncol = 1, scales = "free_y")
# 
# #preview daily data
# df_new %>% 
#   ggplot(aes(date, value_clean)) +
#   geom_point() +
#   facet_wrap(~metric, ncol = 1, scales = "free_y")

#find most recent date
last_updated <- last(df_rolling$date)

#make graph
allegheny_county_timeline <- df_rolling %>% 
  filter(!is.na(value_new_rolling_14)) %>% 
  ggplot(aes(date, value_new_rolling_14)) +
  #create colored rectangles showing various government intervention timelines
  annotate(geom = "rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-05-15"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "red", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-05-15"), xmax = ymd("2020-06-05"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "yellow", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-06-05"), xmax = ymd("2020-06-28"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
           fill = "green", alpha = .3) +
  annotate(geom = "rect", xmin = ymd("2020-06-28"), xmax = as.Date(Inf), ymin = as.Date(-Inf), ymax = as.Date(Inf),
           fill = "#aaff00", alpha = .3) +
  #plot daily data as points, rolling average as lines
  geom_point(data = df_new, aes(y = value_new_clean, alpha = flag_bad_data)) +
  geom_line(size = 1.5) +
  #facet by metric
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  scale_alpha_manual(values = c(.3, .05), guide = FALSE) +
  labs(title = str_c("Allegheny County COVID-19 response timeline (last updated ", last_updated, ")"),
       x = NULL,
       y = NULL,
       subtitle = "14-day rolling average",
       caption = "@conor_tompkins, data from Allegheny County via Franklin Chen")

allegheny_county_timeline

allegheny_county_timeline %>% 
  ggsave(filename = str_c("output/ac_timeline/allegheny_county_timeline_", last_updated, ".png"), width = 12, height = 10)


# df_rolling %>% 
#   filter(metric == "New cases") %>% 
#   summarize(first(date))
# 
# df_new %>% 
#   filter(metric == "New cases") %>% 
#   summarize(first(date))
# 
# df_rolling %>% 
#   filter(metric == "New cases") %>% 
#   summarize(last(date))
# 
# df_new %>% 
#   filter(metric == "New cases") %>% 
#   summarize(last(date))
# 
# df_rolling %>% 
#   filter(metric == "New cases") %>% 
#   nrow()
# 
# df_new %>% 
#   filter(metric == "New cases") %>% 
#   nrow()



#need to make functions to make one combined chart per metric
#need to make one function to knit 3 charts together
###cases
make_combined_graph <- function(choose_metric){
  
  main_graph <- df_rolling %>% 
    filter(metric == choose_metric) %>% 
    ggplot(aes(date, value_new_rolling_14)) +
    #create colored rectangles showing various government intervention timelines
    annotate(geom = "rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-05-15"), ymin = as.Date(-Inf), ymax = as.Date(Inf),
             fill = "red", alpha = .3) +
    annotate(geom = "rect", xmin = ymd("2020-05-15"), xmax = ymd("2020-06-05"), ymin = as.Date(-Inf), ymax = as.Date(Inf),
             fill = "yellow", alpha = .3) +
    annotate(geom = "rect", xmin = ymd("2020-06-05"), xmax = ymd("2020-06-28"), ymin = as.Date(-Inf), ymax = as.Date(Inf),
             fill = "green", alpha = .3) +
    annotate(geom = "rect", xmin = ymd("2020-06-28"), xmax = as.Date(Inf), ymin = as.Date(-Inf), ymax = as.Date(Inf),
             fill = "#aaff00", alpha = .3) +
    #plot daily data as points, rolling average as lines
    geom_point(data = filter(df_new, metric == choose_metric), aes(y = value_new_clean, alpha = flag_bad_data)) +
    geom_line(size = 1.5) +
    #geom_vline(data = filter(df_new, metric == choose_metric, new_record == TRUE), aes(xintercept = date), color = "red") +
    #facet by metric
    facet_wrap(~metric, ncol = 1, scales = "free_y") +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_alpha_manual(values = c(.3, .05), guide = FALSE) +
    labs(x = NULL,
         y = NULL) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.text.x = element_blank())
  
  sub_graph <- df_new %>% 
    filter(metric == choose_metric) %>% 
    ggplot(aes(date, y = 1, fill = new_record)) +
    geom_tile(color = "white") +
    coord_equal() +
    scale_fill_manual(values = c("white", "black"), guide = FALSE) +
    theme_void() +
    theme(strip.text = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.text.x = element_text())
  
  layout <- c(
    area(t = 1, l = 1, b = 5, r = 5),
    area(t = 4, l = 1, b = 7.5, r = 5)
  )
  
  
combined_graph <- main_graph + sub_graph +
  plot_layout(design = layout)
}

make_combined_graph(choose_metric = "New cases") %>% 
  print()

plots <- df_rolling %>% 
  mutate(metric = as.character(metric)) %>% 
  distinct(metric) %>% 
  pull(metric) %>% 
  as.list() %>% 
  set_names() %>% 
  map(~make_combined_graph(choose_metric = .x))


final_plot <- wrap_plots(plots, ncol = 1) +
  plot_annotation(title = str_c("Allegheny County COVID-19 response timeline (last updated ", last_updated, ")"), 
                  subtitle = "14-day rolling average",
                  caption = "@conor_tompkins, data from Allegheny County via Franklin Chen")

final_plot %>% 
  ggsave(filename = str_c("output/ac_timeline/combined/combo_graph_", last_updated , ".png"), width = 12, height = 8)
