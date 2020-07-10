#load libraries
library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(patchwork)

#settings
theme_set(theme_ipsum(base_size = 15, strip_text_size = 15, axis_title_size = 15))

options(scipen = 999, digits = 4)

#read in data
df <- read_csv("https://raw.githubusercontent.com/FranklinChen/covid-19-allegheny-county/master/covid-19-allegheny-county.csv") %>% 
  mutate(state = "Pennsylvania",
         county = "Allegheny County") 

#glimpse(df)

#replace deaths on July 7th with NA
#The deaths reported today are from the state’s use of the Electronic Data Reporting System (EDRS) and include #deaths from April 5 – June 13, all decedents were 65 or older.
#https://twitter.com/HealthAllegheny/status/1280517051589722117?s=20
df <- df %>% 
  mutate(deaths = case_when(date == "2020-07-07" ~ NA_real_,
                            date != "2020-07-07" ~ deaths))

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

#glimpse(df)


#pivot rolling average data longer
df_rolling <- df %>% 
  select(state, county, date, contains("rolling")) %>% 
  pivot_longer(cols = contains("rolling"), names_to = "metric") %>% 
  mutate(metric = case_when(str_detect(metric, "cases") ~ "New cases",
                            str_detect(metric, "deaths") ~ "New deaths",
                            str_detect(metric, "hospitalizations") ~ "New hospitalizations")) %>% 
  mutate(metric = factor(metric, levels = c("New cases", "New hospitalizations", "New deaths")))

#glimpse(df_rolling)

#pivot daily data longer
df_new <- df %>% 
  select(state, county, date, !contains("rolling")) %>% 
  pivot_longer(cols = contains("_new"), names_to = "metric") %>% 
  mutate(metric = case_when(str_detect(metric, "cases") ~ "New cases",
                            str_detect(metric, "deaths") ~ "New deaths",
                            str_detect(metric, "hospitalizations") ~ "New hospitalizations")) %>% 
  mutate(metric = factor(metric, levels = c("New cases", "New hospitalizations", "New deaths")))

#first(df_new$date)

#identify first non-zero value in each metric.
##filter out rows before first non-zero value
df_new <- df_new %>% 
  arrange(state, county, metric, date) %>% 
  group_by(state, county, metric) %>% 
  mutate(value = case_when(row_number() == 1 ~ 0,
                           TRUE ~ value),
         after_first_non_zero_value = cumsum(coalesce(value, 0) > 0) >= 1,
         value = case_when(after_first_non_zero_value == FALSE ~ 0,
                           TRUE ~ value)) %>% 
  ungroup()

#first(df_new$date)

df_new <- df_new %>% 
  group_by(metric) %>% 
  mutate(cumulative_max = cummax(coalesce(value, 0)),
         new_record = value == cumulative_max,
         new_record = case_when(after_first_non_zero_value == FALSE ~ FALSE,
                                after_first_non_zero_value == TRUE ~ new_record)) %>% 
  ungroup()

df_new <- df_new %>% 
  mutate(value_clean = case_when(after_first_non_zero_value == FALSE ~ NA_real_,
                                 TRUE ~ value))

# first(df_new$date)

#preview rolling data
df_rolling %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~metric, ncol = 1, scales = "free_y")

#preview daily data
df_new %>% 
  ggplot(aes(date, value_clean)) +
  geom_point() +
  facet_wrap(~metric, ncol = 1, scales = "free_y")

#find most recent date
last_updated <- last(df_rolling$date)

#make graph
allegheny_county_timeline <- df_rolling %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(date, value)) +
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
  geom_point(data = df_new, aes(y = value_clean), alpha = .3)+
  geom_line(size = 1.5) +
  #facet by metric
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
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
make_combined_graph <- function(choose_metric){
  
  main_graph <- df_rolling %>% 
    filter(metric == choose_metric) %>% 
    ggplot(aes(date, value)) +
    #create colored rectangles showing various government intervention timelines
    # annotate(geom = "rect", xmin = ymd("2020-03-23"), xmax = ymd("2020-05-15"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
    #          fill = "red", alpha = .3) +
    # annotate(geom = "rect", xmin = ymd("2020-05-15"), xmax = ymd("2020-06-05"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
    #          fill = "yellow", alpha = .3) +
    # annotate(geom = "rect", xmin = ymd("2020-06-05"), xmax = ymd("2020-06-28"), ymin = as.Date(-Inf), ymax = as.Date(Inf), 
    #          fill = "green", alpha = .3) +
    # annotate(geom = "rect", xmin = ymd("2020-06-28"), xmax = as.Date(Inf), ymin = as.Date(-Inf), ymax = as.Date(Inf),
    #          fill = "#aaff00", alpha = .3) +
    #plot daily data as points, rolling average as lines
    geom_point(data = filter(df_new, metric == choose_metric),  aes(y = value), alpha = .3)+
    geom_line(size = 1.5) +
    geom_vline(data = filter(df_new, metric == choose_metric, new_record == TRUE), aes(xintercept = date), color = "red") +
    #facet by metric
    #facet_wrap(~metric, ncol = 1, scales = "free_y") +
    labs(title = str_c("Allegheny County COVID-19 response timeline (last updated ", last_updated, ")"),
         x = NULL,
         y = NULL,
         subtitle = "14-day rolling average") +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          axis.text.x = element_blank())
  
  sub_graph <- df_new %>% 
    filter(metric == choose_metric) %>% 
    ggplot(aes(date, y = 1, fill = new_record)) +
    geom_tile(color = "black") +
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
  plot_layout(design = layout) +
  plot_annotation(caption = "@conor_tompkins, data from Allegheny County via Franklin Chen",
                  #theme = theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
                  )
  

print(combined_graph)
}

make_combined_graph(choose_metric = "New cases") %>% 
  ggsave(filename = "output/ac_timeline/combined/test.png")

