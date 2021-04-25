library(tidyverse)
library(tidycensus)
library(sf)
library(broom)
library(lubridate)
library(janitor)
library(hrbrthemes)
library(ggrepel)
library(tune)
library(slider)
library(tsibble)
library(fable)
library(broom)
library(gganimate)

theme_set(theme_ipsum())

options(scipen = 999, digits = 4)

#look into bloomberg data
#https://github.com/BloombergGraphics/covid-vaccine-tracker-data

#https://github.com/govex/COVID-19/blob/master/data_tables/vaccine_data/us_data/data_dictionary.csv

#set date range to examine.
date_seq <- seq.Date(from = ymd("2020-12-01"), to = ymd("2022-08-01"), by = "day")

#download data from JHU

vacc_data_raw <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/vaccine_data_us_timeline.csv") %>% 
  clean_names() %>% 
  #filter to only keep vaccine type All, which will show the cumulative sum of doses for all vaccine types
  filter(vaccine_type == "All") %>% 
  #select state, date, vaccine type, and stage one doeses
  select(province_state, date, stage_one_doses) %>% 
  #for each state and vaccine type, pad the time series to include all the dates in date_seq
  complete(date = date_seq, province_state) %>% 
  #sort by state and date
  arrange(province_state, date)

#there are cases where the cumulative sum of vaccinations for a given state actually decreases. 
#i think this is because of interruptions due to the nation-wide winter storm in February
#other instances could be attributed to changes in methodology for tracking vaccine distribution
vacc_data_raw %>% 
  group_by(province_state) %>% 
  mutate(less_than_prev = stage_one_doses < lag(stage_one_doses, 1)) %>% 
  ungroup() %>% 
  filter(less_than_prev == T) %>% 
  count(date, less_than_prev) %>% 
  ggplot(aes(date, n)) +
  geom_point()

#this shows that most of the inconsistent data comes from federal agencies, US territories, and Freely Associated States
vacc_data_raw %>% 
  group_by(province_state) %>% 
  mutate(less_than_prev = stage_one_doses < lag(stage_one_doses, 1)) %>% 
  ungroup() %>% 
  filter(less_than_prev == T) %>% 
  count(province_state, less_than_prev, sort = T) 

#this is an example from alabama
vacc_data_raw %>% 
  mutate(less_than_prev = stage_one_doses < lag(stage_one_doses, 1)) %>% 
  filter(date >= "2021-02-10", date < "2021-02-13",
         province_state == "Alabama") %>% 
  arrange(province_state, date) %>% 
  View()

#replace NA values of stage_one_doses with 0 if it is before the current date
vacc_data <- vacc_data_raw %>% 
  mutate(stage_one_doses = case_when(date < ymd(Sys.Date()) & is.na(stage_one_doses) ~ 0,
                                     !is.na(stage_one_doses) ~ stage_one_doses,
                                     TRUE ~ NA_real_)) %>% 
  arrange(province_state, date)

#this shows the cumulative sum of first doses by province_state
#there is a spike in Pennsylvania around april 18th that went back down
vacc_data %>% 
  filter(date <= ymd(Sys.Date())) %>% 
  ggplot(aes(date, stage_one_doses, group = province_state)) +
  geom_line(alpha = .5, size = .3)

#calculate the total sum of first doses
vacc_data <- vacc_data %>% 
  group_by(date) %>% 
  summarize(stage_one_doses = sum(stage_one_doses, na.rm = F)) %>%
  ungroup()

#we can see that the inconsistent data reporting issues bubble up to the national level
vacc_data %>% 
  filter(date <= ymd(Sys.Date())) %>% 
  ggplot(aes(date, stage_one_doses)) +
  geom_line()

#this calculates the number of new doses given out by day
#if the difference between day 1 and day 0 is negative, i replace it with 0
vacc_data <- vacc_data %>% 
  mutate(stage_one_doses_new = stage_one_doses - lag(stage_one_doses, n = 1),
         stage_one_doses_new = case_when(stage_one_doses_new < 0 ~ 0,
                                         TRUE ~ stage_one_doses_new))

#this calculates the 7 day trailing average of new doses
vacc_data_rolling <- vacc_data %>% 
  mutate(stage_one_doses_new_rolling = slide_index_dbl(.i = date,
                                                       .x = stage_one_doses_new,
                                                       .f = mean,
                                                       .before = 6,
                                                       .complete = FALSE))




#this recalculates the cumulative sum of first doses using the trailing average instead of the raw data
vacc_forecast <- vacc_data_rolling %>% 
  fill(stage_one_doses_new_rolling, .direction = "down") %>%
  mutate(future_flag = date >= ymd(Sys.Date())) %>%
  mutate(stage_one_doses_new_rolling_forecast = cumsum(coalesce(stage_one_doses_new_rolling, 0)))

vacc_forecast %>% 
  filter(future_flag == F) %>% 
  ggplot(aes(date, stage_one_doses_new_rolling_forecast)) +
  geom_line() +
  scale_y_comma()

#this calculates the total first dose vaccination pct
vacc_forecast <- vacc_forecast %>% 
  mutate(total_pop = 332410303) %>% 
  mutate(vacc_pct = stage_one_doses_new_rolling_forecast / total_pop)

#at the current rate we should hit 90% around July 13th
vacc_forecast %>% 
  filter(vacc_pct > .9) %>% 
  slice(1)

vacc_forecast %>% 
  filter(date <= ymd(Sys.Date()) + 120) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = vacc_pct, color = future_flag)) +
  geom_hline(yintercept = .9, lty = 2) +
  scale_y_percent(limits = c(0, 1), breaks = c(0, .25, .5, .75, .9, 1)) +
  labs(y = "Pct with 1 vaccination")

#create a list of dates to filter the data by
month_filters <- c(seq(from = ymd("2021-02-01"), to = ymd("2021-04-01"), by = "month"), Sys.Date())

#for each date in the list, filter the data against it
#this creates 4 dataframes each only containing data up until the given filter date
vaccine_forecast_data <- month_filters %>% 
  set_names() %>% 
  map(~filter(vacc_data_rolling, date <= .x)) %>% 
  enframe(name = "last_date", value = "historical_data") %>% 
  mutate(last_date = ymd(last_date),
         current_week = last_date == max(last_date))

vaccine_forecast_data

vaccine_forecast_data <- vaccine_forecast_data %>% 
  unnest(historical_data) %>% 
  group_by(last_date) %>% 
  #for each filter date table, create rows for the rest of the date range
  complete(date = date_seq) %>% 
  fill(stage_one_doses_new_rolling, current_week, .direction = "down") %>% 
  #create a flag for whether a row is observed or predicted
  #create a flag for whether a row is after the current date
  mutate(prediction_flag = date >= last_date,
         future_flag = date > Sys.Date()) %>%
  #for each filter date, roll the 7 day moving average of vaccination rate forward
  mutate(stage_one_doses_new_rolling_forecast = cumsum(coalesce(stage_one_doses_new_rolling, 0))) %>% 
  mutate(total_pop = 332410303) %>% 
  #calculate vaccination %
  mutate(vacc_pct = stage_one_doses_new_rolling_forecast / total_pop,
         vacc_pct = round(vacc_pct, 3)) %>% 
  filter(vacc_pct <= 1.1) %>% 
  ungroup()

vaccine_forecast_data

vaccine_forecast_data <- vaccine_forecast_data %>% 
  mutate(total_vacc_flag = vacc_pct >= 1) %>% 
  group_by(last_date) %>% 
  mutate(total_vacc_date = case_when(cumsum(total_vacc_flag) >= 1 ~ date,
                                     TRUE ~ NA_Date_)) %>% 
  filter(cumsum(!is.na(total_vacc_date)) <= 1) %>% 
  ungroup()
  
vaccine_forecast_data <- vaccine_forecast_data %>% 
  mutate(current_week_fct = case_when(current_week == F ~ "Past hypothetical rate",
                                      current_week == T ~ "Current rate"))

#secondary tables for labeling
current_vacc_percent <- vaccine_forecast_data %>% 
  filter(current_week == T, date == ymd(Sys.Date())) %>% 
  select(last_date, date, current_week, vacc_pct)

current_vacc_percent_label <- current_vacc_percent %>% 
  mutate(text_label = str_c("Current", scales::percent(vacc_pct), sep = ": "))

#for gif
  #create labels for current vaccination rate line, NA for last row
  mutate(current_rate_label = case_when(current_week == T & date != max(date) ~ str_c("Current rate:", scales::percent(vacc_pct, 1), sep = " "),
                                        TRUE ~ "")) %>% 
  #create labels for lines where current_week == F








filter(vaccine_forecast_data, prediction_flag == F, current_week == T, future_flag == F) %>% 
  ggplot(aes(date, vacc_pct)) +
  geom_line()

#static graph
vaccine_forecast_graph <- vaccine_forecast_data %>% 
  ggplot(aes(x = date, y = vacc_pct, group = last_date)) +
  #90% line
  geom_hline(yintercept = .9, lty = 2) +
  annotate(x = ymd("2020-10-15"), y = .92,
           label = "Herd Immunity Threshhold", geom = "text", size = 3) +
  #past cumulative line
  geom_line(data = filter(vaccine_forecast_data, current_week == T, date <= Sys.Date()),
            color = "black", lty = 1, size = .7) +
  #future cumulative lines
  geom_line(data = filter(vaccine_forecast_data, prediction_flag == T),
            aes(color = as.factor(last_date)),
            size = 1.3)  +
  # horizontal line showing current vacc rate
  geom_hline(data = current_vacc_percent,
             aes(yintercept = vacc_pct),
             size = .1) +
  #add labels for date of 100% vaccination for the first and last filter dates
  geom_label(data = filter(vaccine_forecast_data,
                           last_date == min(last_date) | last_date == max(last_date)),
             aes(label = total_vacc_date,
                 color = as.factor(last_date)),
             show.legend = FALSE,
             fill = "grey",
             position = position_nudge(y = .05),
             size = 3) +
  # label for horizontal line showing current vacc rate
  geom_text(data = current_vacc_percent_label,
            aes(label = text_label),
            position = position_nudge(x = -220, y = .02),
            size = 3) +
  scale_y_percent(limits = c(0, 1.1), breaks = c(0, .25, .5, .75, .9, 1)) +
  scale_alpha_manual(values = c(1, .5)) +
  scale_color_viridis_d(labels = c("February 1", "March 1", "April 1", "April 20")) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  labs(title = "Historic and Current U.S. Vaccination Forecasts",
       x = NULL,
       y = "Single Dose Vaccination %",
       color = "Projection start date")

vaccine_forecast_graph

ggsave(vaccine_forecast_graph, filename = "scripts/model_vaccination_drive/output/vaccination_drive_forecast.png")


#animation
vaccine_forecast_data %>% 
  ggplot(aes(x = date, y = vacc_pct, group = last_date)) +
  #90% line
  geom_hline(yintercept = .9, lty = 2) +
  #past cumulative line
  geom_line(data = filter(vaccine_forecast_data, future_flag == F, current_week == T),
            color = "black", lty = 1, size = .7) +
  #future cumulative line
  geom_line(data = filter(vaccine_forecast_data, prediction_flag == T),
            aes(size = current_week_fct,
                alpha = current_week_fct),
            color = "blue", lty = 2)  +
  geom_hline(data = current_vacc_percent,
             aes(yintercept = vacc_pct)) +
  geom_point(data = filter(vaccine_forecast_data, current_week == T),
             aes(color = future_flag),
             size = 3) +
  geom_text(data = filter(vaccine_forecast_data,
                          last_date == min(last_date) | last_date == max(last_date)),
            aes(label = total_vacc_date),
            position = position_nudge(y = .05),
            size = 6) +
  #horizontal line showing current vacc rate
  geom_text(data = current_vacc_percent_label,
            aes(label = text_label),
            position = position_nudge(x = -80, y = .01),
            size = 6) +
  scale_y_percent(limits = c(0, 1.1), breaks = c(0, .25, .5, .75, .9, 1)) +
  scale_color_manual(values = c("black", "blue"),
                     #labels = c("Past hypothetical", "Current rate")
  ) +
  scale_size_manual(values = c(.8, .3),
                    #labels = c("Past hypothetical", "Current rate")
  ) +
  scale_alpha_manual(values = c(1, .7),
                     #labels = c("Past hypothetical", "Current rate")
  ) +
  guides(color = FALSE) +
  labs(title = "Historic and Current U.S. Vaccination Forecasts",
       x = NULL,
       y = "Single Dose Vaccination %",
       size = NULL,
       alpha = NULL
  )

vaccine_forecast_graph_anim <- vaccine_forecast_graph +
  #current rate % label
  geom_text(data = filter(vaccine_forecast_data, 
                          current_week == T,
                          vacc_pct >= .25),
            aes(label = current_rate_label),
            position = position_nudge(x = -70, y = 0),
            size = 6) +
  theme_ipsum(base_size = 15,
              axis_title_size = 20) +
  transition_reveal(date)

gif_fps <- 30

anim_save(filename = "scripts/model_vaccination_drive/output/vaccination_drive_forecast.gif",
          vaccine_forecast_graph_anim,
          duration = 15,
          fps = gif_fps,
          height = 800,
          width = 1000,
          end_pause = gif_fps * 4
)




#model stage_one_doses_new_7day
vacc_ts <- vacc_data_rolling %>% 
  drop_na(stage_one_doses_new_7day) %>% 
  as_tsibble(index = date)

vacc_model_fit <- vacc_ts %>% 
  model(ETS(box_cox(stage_one_doses_new_7day, lambda = .5) ~ error("A") + trend("A") + season("N")))

vacc_model_fit %>% 
  components() %>% 
  autoplot()

vacc_model_fit %>%
  forecast(h = 180) %>%
  autoplot(vacc_ts) +
  scale_y_comma()

vacc_model_fit %>%
  forecast(new_data = vacc_ts) %>% 
  View()

vacc_model_fit %>% 
  forecast(h = 30) %>%
  View()

vacc_ts %>% 
  as_tibble() %>% 
  bind_rows(forecast(vacc_model_fit, h = 180) %>%
              as_tibble() %>% 
              select(date, .mean) %>% 
              rename(stage_one_doses_new_7day = .mean)) %>% 
  mutate(future_flag = date >= ymd(Sys.Date())) %>%
  mutate(cum_vacc_forecast = cumsum(stage_one_doses_new_7day)) %>% 
  mutate(total_pop = 332410303) %>% 
  mutate(vacc_pct = cum_vacc_forecast / total_pop) %>% 
  ggplot(aes(date, vacc_pct, color = future_flag)) +
  geom_line() +
  geom_hline(yintercept = .9, lty = 2) +
  scale_y_percent(limits = c(0, 1), breaks = c(0, .25, .5, .75, .9, 1)) +
  labs(y = "Pct with 1 vaccination")

#model cumulative sum of stage_one_doses

dput(vacc_ts)

vacc_cumsum_model <- vacc_ts %>% 
  fabletools::model(basic_ETS = ETS(stage_one_doses ~ error("A") + trend("A") + season("N")),
                    `ETS_box_cox_lambda .1` = ETS(box_cox(stage_one_doses, lambda = .1) ~ error("A") + trend("A") + season("N")),
                    `ETS_box_cox_lambda .5` = ETS(box_cox(stage_one_doses, lambda = .5) ~ error("A") + trend("A") + season("N")),
                    `ETS_box_cox_lambda .9` = ETS(box_cox(stage_one_doses, lambda = .9) ~ error("A") + trend("A") + season("N")))

vacc_cumsum_model %>% 
  fabletools::forecast(h = 30*12) %>% 
  autoplot(vacc_ts) +
  geom_hline(yintercept = 332410303 * .9, lty = 2) +
  scale_y_comma() +
  coord_cartesian(ylim = c(0, 500000000),
                  xlim = c(ymd("2020-12-01", ymd("2021-12-31")))) +
  facet_wrap(~.model) +
  labs(x = NULL,
       y = "People with at least 1 dose",
       fill = "Confidence level")

vacc_cumulative_forecast <- vacc_ts %>% 
  as_tibble() %>% 
  bind_rows(vacc_cumsum_model %>% 
              select(`ETS_box_cox_lambda .5`) %>%
              forecast(h = 30*12) %>%
              #as_tibble() %>% 
              select(date, stage_one_doses, .mean) %>% 
              hilo(level = 80) %>% 
              unpack_hilo(`80%`) %>% 
              rename(.pred = stage_one_doses,
                     stage_one_doses = .mean,
                     pi_80_upper = `80%_upper`,
                     pi_80_lower = `80%_lower`) %>% 
              select(date, stage_one_doses, .pred, pi_80_upper, pi_80_lower)) %>%
  mutate(future_flag = date >= ymd(Sys.Date())) %>%
  mutate(cum_vacc_forecast = stage_one_doses) %>% 
  mutate(total_pop = 332410303) %>% 
  mutate(vacc_pct = cum_vacc_forecast / total_pop,
         vacc_pct_upper = pi_80_upper / total_pop,
         vacc_pct_lower = pi_80_lower / total_pop)

vacc_cumulative_forecast %>% 
  filter(vacc_pct > .9) %>%
  slice(1)

vacc_cumulative_forecast %>% 
  ggplot(aes(date, vacc_pct)) +
  geom_ribbon(aes(x = date, ymin = vacc_pct_lower, ymax = vacc_pct_upper),
              fill = "blue", alpha = .3) +
  geom_line(aes(color = future_flag, lty = future_flag)) +
  geom_hline(yintercept = .9, lty = 2) +
  scale_y_percent(breaks = c(0, .25, .5, .75, .9, 1)) +
  scale_color_manual(values = c("black", "blue")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(y = "Pct with 1 vaccination")


month_filters <- c(ymd("2021-02-01"), ymd("2021-04-01"))

forecast_vacc <- function(model){
  
  model %>%
    forecast(h = 30*12) %>%
    #as_tibble() %>% 
    select(date, stage_one_doses, .mean) %>% 
    hilo(level = 80) %>% 
    unpack_hilo(`80%`) %>% 
    rename(.pred = stage_one_doses,
           stage_one_doses = .mean,
           pi_80_upper = `80%_upper`,
           pi_80_lower = `80%_lower`) %>% 
    select(date, stage_one_doses, .pred, pi_80_upper, pi_80_lower) %>% 
    as_tibble()
  
  
}

month_filters %>% 
  set_names() %>% 
  map(~filter(vacc_ts, date <= .x)) %>% 
  enframe(name = "last_date", value = "vacc_tibble") %>% 
  mutate(model = map(vacc_tibble, ~fabletools::model(.data = .x, 
                                                     model_name = ETS(box_cox(stage_one_doses, lambda = .8) ~ 
                                                                        error("A") + 
                                                                        trend("A") + 
                                                                        season("N"))))) %>% 
  unnest(model) %>% 
  mutate(predictions = map(model_name, forecast_vacc)) %>% 
  unnest(predictions) %>% 
  mutate(future_flag = date >= ymd(Sys.Date())) %>%
  mutate(cum_vacc_forecast = stage_one_doses) %>% 
  mutate(total_pop = 332410303) %>% 
  mutate(vacc_pct = cum_vacc_forecast / total_pop,
         vacc_pct_upper = pi_80_upper / total_pop,
         vacc_pct_lower = pi_80_lower / total_pop) %>% 
  ggplot(aes(date, vacc_pct)) +
  geom_ribbon(aes(x = date, ymin = vacc_pct_lower, ymax = vacc_pct_upper),
              fill = "blue", alpha = .3) +
  geom_line(aes(color = future_flag, lty = future_flag)) +
  geom_hline(yintercept = .9, lty = 2) +
  scale_y_percent(breaks = c(0, .25, .5, .75, .9, 1)) +
  scale_color_manual(values = c("black", "blue")) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~last_date) +
  labs(y = "Pct with 1 vaccination")
