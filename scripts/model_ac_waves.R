library(tidyverse)
library(lubridate)
library(tidyquant)
library(hrbrthemes)
library(modeest)
library(tidymodels)

options(scipen = 999, digits = 4)

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

last_updated <- last(df$date)

df %>% 
  ggplot(aes(date, cases)) +
  geom_point()

df %>% 
  ggplot(aes(date, cases_new)) +
  geom_point()

df %>% 
  ggplot(aes(date, cases_new_rolling_14)) +
  geom_point() +
  geom_vline(xintercept = ymd("2020-06-18"))

waves <- df %>% 
  select(date, cases_new_rolling_14) %>%
  filter(!is.na(cases_new_rolling_14)) %>% 
  select(date, cases_new_rolling_14) %>% 
  mutate(peaks = ggpmisc:::find_peaks(cases_new_rolling_14, span = 15, strict = TRUE),
         valleys = ggpmisc:::find_peaks(-cases_new_rolling_14, span = 15, strict = TRUE)) %>%
  rowwise() %>% 
  mutate(wave = sum(peaks, valleys)) %>% 
  ungroup() %>% 
  mutate(wave = (cumsum(wave) %/% 2),
         wave = wave + 1)


waves <- waves %>% 
  group_by(wave) %>%
  mutate(wave_start_date = first(date),
         days_since_wave_started = date - wave_start_date,
         days_since_wave_started = as.numeric(days_since_wave_started)) %>% 
  ungroup()

waves <- waves %>%
  group_by(wave) %>% 
  mutate(pct_change = (cases_new_rolling_14 - lag(cases_new_rolling_14)) / cases_new_rolling_14) %>% 
  ungroup() %>% 
  replace_na(list(pct_change = 0))

waves <- waves %>% 
  mutate(wave = as.factor(wave))

waves %>% 
  ggplot(aes(date, cases_new_rolling_14)) +
  geom_point() +
  stat_valleys(span = 15, color = "blue", size = 3) +
  stat_peaks(span = 15, color = "red", size = 3)

wave_graph_1 <- waves %>% 
  ggplot(aes(date, cases_new_rolling_14, color = wave)) +
  geom_line(size = 2) +
  scale_color_viridis_d() +
  labs(title = str_c("Allegheny County COVID-19 cases (last updated ", last_updated, ")"),
       subtitle = "14-day rolling average",
       x = NULL,
       y = "New cases",
       caption = "@conor_tompkins, data from NYTimes",
       color = "Wave")

wave_graph_1 %>% 
  ggsave(filename = str_c("output/ac_timeline/wave_graph_1_", last_updated, ".png"), width = 12, height = 6)

wave_graph_2 <- waves %>% 
  ggplot(aes(days_since_wave_started, cases_new_rolling_14, color = as.factor(wave))) +
  geom_line(size = 2) +
  scale_color_viridis_d() +
  labs(title = str_c("Allegheny County COVID-19 cases (last updated ", last_updated, ")"),
       subtitle = "14-day rolling average",
       x = "Days since wave started",
       y = "New cases",
       caption = "@conor_tompkins, data from NYTimes",
       color = "Wave")

wave_graph_2 %>% 
  ggsave(filename = str_c("output/ac_timeline/wave_graph_2_", last_updated, ".png"), width = 12, height = 6)
  
wave_pct_change_plot <- waves %>% 
  ggplot(aes(days_since_wave_started, pct_change, color = wave, fill = wave)) +
  geom_point(alpha = .8, size = 1) +
  geom_smooth() +
  scale_y_percent() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(title = str_c("Allegheny County COVID-19 cases (last updated ", last_updated, ")"),
       subtitle = "Percent change in new cases from previous day",
       x = "Days since wave started",
       y = "Percent change",
       caption = "@conor_tompkins, data from NYTimes",
       color = "Wave",
       fill = "Wave")

wave_pct_change_plot %>% 
  ggsave(filename = str_c("output/ac_timeline/wave_pct_change_plot_", last_updated, ".png"), width = 12, height = 6)

### find days until peak for wave 1
waves %>% 
  group_by(wave) %>% 
  mutate(days_until_peak = cumsum(peaks == TRUE),
         days_until_peak_2 = sum(days_until_peak == 0) + 1) %>% 
  View()

%>% 
  summarize(days_until_peak = max(days_until_peak)) %>% 
  ggplot(aes(wave, days_until_peak)) +
  geom_col()





waves %>% 
  select(wave, days_since_wave_started, cases_new_rolling_14, pct_change) %>% 
  filter(wave == 1) %>% 
  mutate(cases_max = max(cases_new_rolling_14),
         max_flag = cases_new_rolling_14 == cases_max,
         flag_after_max = cumsum(max_flag) %>% as.logical) %>% 
  View()

waves %>% 
  select(wave, days_since_wave_started, cases_new_rolling_14, pct_change) %>% 
  filter(wave == 1) %>% 
  summarize(cases_diff = diff(diff(cases_new_rolling_14)))

which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(FALSE,diff(x)>0,TRUE))>0)
    }else {
      which(diff(diff(x)>0)>0)+1
    }
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)+1
    }
  }
}

wave_1_modes <- waves %>% 
  filter(wave == 1) %>% 
  select(date, cases_new_rolling_14) %>% 
  summarize(modes = which.peaks(cases_new_rolling_14))
  which.peaks(.) %>% 
  as_tibble() %>% 
  set_names("modes") %>% 
  mutate(type = "mode")

waves %>% 
  filter(wave == 1) %>% 
  left_join(wave_1_modes, by = c("cases_new_rolling_14" = "modes")) %>% 
  View()

# find.peaks
library(ggpmisc)

waves %>% 
  select(date, cases_new_rolling_14) %>% 
  mutate(peaks = ggpmisc:::find_peaks(cases_new_rolling_14, span = 15, strict = TRUE),
         valleys = ggpmisc:::find_peaks(-cases_new_rolling_14, span = 15, strict = TRUE)) %>%
  rowwise() %>% 
  mutate(wave = sum(peaks, valleys)) %>% 
  ungroup() %>% 
  mutate(wave = cumsum(wave) %/% 2) %>% 
  ggplot(aes(date, cases_new_rolling_14, color = as.factor(wave))) +
  geom_point()
  
  
  ggplot(aes(date, cases_new_rolling_14, color = valleys)) +
  geom_point()


ggpmisc:::find_peaks(.)

waves %>% 
  ggplot(aes(date, cases_new_rolling_14)) +
  geom_point() +
  stat_peaks(geom = "vline", span = 14, color = "red", size = 1) +
  stat_valleys(geom = "vline", span = 14, color = "blue", size = 1)
  


#findPeaks
wave_1_peaks <- waves %>% 
  filter(wave == 1) %>% 
  select(wave, date, cases_new_rolling_14) %>% 
  #group_by(wave) %>% 
  summarize(peaks = findPeaks(cases_new_rolling_14, thresh = 4)) %>% 
  pull(peaks)
  
wave_1_peaks

  mutate(type = "peak")
wave_1_peaks

waves %>% 
  filter(wave == 1) %>% 
  #left_join(wave_1_peaks, by = c("wave" = "wave", "cases_new_rolling_14" = "peaks")) %>% 
  #mutate(peaks = cases_new_rolling_14 >= wave_1_peaks) %>% 
  View()


  ggplot(aes(date, cases_new_rolling_14, color = cases_new_rolling_14 == 37)) + 
  geom_point()

#extrapolate when wave 2 will peak

#model how long wave 2 will last


####


waves %>% 
  ggplot(aes(days_since_wave_started, cases_new_rolling_14, color = as.factor(wave))) +
  geom_point() +
  facet_wrap(~wave, ncol = 1, scales = "free_y")

waves %>% 
  ggplot(aes(days_since_wave_started, pct_change, color = as.factor(wave))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~wave, ncol = 1, scales = "free_y")

waves %>% 
  filter(wave == 1)

waves <- waves %>% 
  select(wave, days_since_wave_started, pct_change) %>% 
  arrange(wave, days_since_wave_started) %>% 
  complete(days_since_wave_started, wave) %>% 
  arrange(wave, days_since_wave_started) 

waves %>% 
  filter(wave == 2)

waves %>% 
  filter(wave == 1) %>% 
  select(days_since_wave_started, pct_change) %>% 
  lm(pct_change ~ ., data = .) %>% 
  tidy()

waves_1 <- waves %>% 
  filter(wave == 1)

waves_2 <- waves %>% 
  filter(wave == 2)

waves_fitted <- waves %>% 
  filter(wave == 1) %>% 
  select(days_since_wave_started, pct_change) %>% 
  #lm(pct_change ~ ., data = .) %>% 
  loess(pct_change ~ days_since_wave_started, data = .) %>% 
  augment(newdata = waves_2) %>% 
  bind_rows(waves_1) %>% 
  mutate(flag_fitted = is.na(pct_change),
         .fitted = case_when(!is.na(pct_change) ~ as.double(NA),
                             is.na(pct_change) ~ .fitted))

waves_fitted %>% 
  ggplot(aes(days_since_wave_started, pct_change, color = as.factor(wave), fill = as.factor(wave))) +
  geom_point() +
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit),
              alpha = .5) +
  #geom_point(aes(y = .fitted)) +
  facet_wrap(~wave, scales = "free_y", ncol = 1)

waves %>% 
  select(wave, days_since_wave_started, pct_change) %>% 
  arrange(wave, days_since_wave_started) %>% 
  complete(days_since_wave_started, wave) %>% 
  arrange(wave, days_since_wave_started) %>% 
  View()



waves %>% 
  ggplot(aes(days_since_wave_started, cases_new_rolling_14, color = wave)) +
  geom_point() +
  facet_wrap(~wave, ncol = 1, scales = "free_y")

#compare peaks
cases <- df %>% 
  select(date, cases_new, cases_new_rolling_14) %>% 
  filter(!is.na(cases_new_rolling_14))


cases %>%
  select(cases_new_rolling_14) %>% 
  kmeans(centers = 2) %>% 
  augment(cases) %>% 
  ggplot(aes(date, cases_new_rolling_14, color = .cluster)) +
  geom_point()

cases %>% 
  count(cases_new, sort = TRUE)

mlv(cases$cases_new, method = "mfv")

d <- density(cases$cases_new_rolling_14)

d$x
d$y