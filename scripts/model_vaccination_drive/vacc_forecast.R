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
date_seq <- seq.Date(from = ymd("2020-12-01"), to = ymd("2022-12-01"), by = "day")

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
  
#this is an example from alaba,a
vacc_data_raw %>% 
  mutate(less_than_prev = stage_one_doses < lag(stage_one_doses, 1)) %>% 
  filter(date >= "2021-02-10", date < "2021-02-13",
         province_state == "Alabama") %>% 
  arrange(province_state, date) %>% 
  View()

#replace NA values of stage_one_doses with NA if it is before the current date
vacc_data <- vacc_data_raw %>% 
  mutate(stage_one_doses = case_when(date < ymd(Sys.Date()) & is.na(stage_one_doses) ~ 0,
                                     !is.na(stage_one_doses) ~ stage_one_doses,
                                     TRUE ~ NA_real_)) %>% 
  arrange(province_state, date)

#this shows the cumulative sum of first doses by province_state
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





vacc_forecast <- vacc_data_rolling %>% 
  fill(stage_one_doses_new_rolling, .direction = "down") %>%
  mutate(future_flag = date >= ymd(Sys.Date())) %>%
  mutate(stage_one_doses_new_rolling_forecast = cumsum(coalesce(stage_one_doses_new_rolling, 0))) %>% 
  # mutate(prediction = case_when(future_flag == T ~ cumsum(coalesce(stage_one_doses_new_rolling, 0)),
  #                               future_flag == F ~ NA_real_)) %>% 
  mutate(total_pop = 332410303) %>% 
  mutate(vacc_pct = stage_one_doses_new_rolling_forecast / total_pop)



vacc_forecast %>% 
  filter(vacc_pct > .9) %>% 
  slice(1)

vacc_forecast %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = vacc_pct, color = future_flag)) +
  geom_hline(yintercept = .9, lty = 2) +
  scale_y_percent(limits = c(0, 1), breaks = c(0, .25, .5, .75, .9, 1)) +
  labs(y = "Pct with 1 vaccination")

month_filters <- seq(from = ymd("2021-02-01"), to = ymd("2021-06-01"), by = "week")


animated_vaccine_forecast <- month_filters %>% 
  set_names() %>% 
  map(~filter(vacc_data_rolling, date <= .x)) %>% 
  enframe(name = "last_date", value = "historical_data") %>% 
  mutate(last_date = ymd(last_date),
         current_week = last_date == max(last_date)) %>% 
  unnest(historical_data) %>% 
  group_by(last_date) %>% 
  complete(date = date_seq) %>% 
  fill(stage_one_doses_new_rolling, current_week, .direction = "down") %>% 
  mutate(future_flag = date >= last_date) %>%
  mutate(stage_one_doses_new_rolling_forecast = cumsum(coalesce(stage_one_doses_new_rolling, 0))) %>% 
  mutate(total_pop = 332410303) %>% 
  mutate(vacc_pct = stage_one_doses_new_rolling_forecast / total_pop) %>% 
  filter(vacc_pct < 1)

animated_vaccine_forecast <- animated_vaccine_forecast %>% 
  #create labels for current vaccination rate line, NA for last row
  mutate(current_rate_label = case_when(current_week == T & date != max(date) ~ str_c("Current rate:", scales::percent(vacc_pct, 1), sep = " "),
                                        TRUE ~ "")) %>% 
  #create labels for lines where current_week == F
  #group_by(last_date) %>% 
  mutate(total_vacc_label = case_when(date == max(date) & last_date == min(last_date) ~ as.character(date),
                                      TRUE ~ "")) #%>% 
  #ungroup()

animated_vaccine_forecast_anim <- animated_vaccine_forecast %>% 
  ggplot(aes(x = date, y = vacc_pct,
             group = last_date)) +
  geom_hline(yintercept = .9, lty = 2) +
  geom_line(aes(color = future_flag,  
                alpha = current_week)) +
  geom_point(data = filter(animated_vaccine_forecast, current_week == T),
             aes(color = future_flag),
             size = 3) +
  geom_text(aes(label = total_vacc_label),
             position = position_nudge(y = .03)
             ) +
  geom_text(data = filter(animated_vaccine_forecast, current_week == T),
             aes(label = current_rate_label),
             position = position_nudge(x = -30)) +
  scale_y_percent(limits = c(0, 1.1), breaks = c(0, .25, .5, .75, .9, 1)) +
  scale_alpha_manual(values = c(.3, 1)) +
  scale_color_manual(values = c("black", "blue")) +
  labs(title = "Historic and Current Vaccination Forecasts",
       subtitle = "% of US with at least one dose",
       x = NULL,
       y = "Vaccination %",
       alpha = "Last Date",
       color = NULL) +
  theme_ipsum(base_size = 20) +
  transition_reveal(date)

anim_save(filename = "scripts/model_vaccination_drive/output/vaccination_drive_forecast.gif",
          animated_vaccine_forecast_anim,
          duration = 10,
          nframes = 50,
          height = 1000,
          width = 1500,
          end_pause = 5
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
