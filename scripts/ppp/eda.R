library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(lubridate)
library(janitor)
library(patchwork)
library(hrbrthemes)

theme_set(theme_ipsum(base_size = 18, axis_title_size = 16, caption_size = 14))

options(scipen = 999, digits = 4, tigris_use_cache = TRUE)

source("scripts/ppp/clean_data.R")

ppp %>% 
  distinct(file_name)

ppp %>% 
  filter(str_detect(file_name, "150k plus"))

pa <- ppp %>% 
  filter(state == "PA") %>% 
  arrange(state, date_approved)

pa %>% 
  distinct(file_name)

pa %>% 
  distinct(loan_range)

glimpse(pa)

# pa %>% 
#   filter(district == "22") %>% 
#   View()


pa_date_agg <- pa %>% 
  group_by(state, date_approved) %>% 
  summarize(loan_amount = sum(loan_amount, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cumulative_loans = cumsum(loan_amount))

pa_date_agg %>% 
  ggplot(aes(date_approved, loan_amount)) +
  geom_point()

pa_date_agg %>% 
  ggplot(aes(date_approved, cumulative_loans), group = 1) +
  geom_line() +
  scale_y_comma()


#map
pa_map <- get_acs(geography = "county", state = "PA", variables = "B19013_001", geometry = TRUE) %>% 
  separate(NAME, into = c("county", "state"), sep = ", ") %>% 
  mutate(fips = str_sub(GEOID, 3, 6))

congress_districts <- congressional_districts(cb = TRUE, resolution = "500k", year = 2018) %>% 
  st_as_sf() %>% 
  clean_names()

congress_districts %>% 
  filter(statefp == 42) %>% 
  ggplot() +
  geom_sf()
  
pa %>% 
  count(district, sort = TRUE) %>% 
  View()

pa %>% 
  filter(district)

ppp_district <- pa %>% 
  group_by(district) %>% 
  summarize(loan_amount = sum(loan_amount, na.rm = TRUE),
            loan_count = n()) %>% 
  arrange(-loan_amount) %>% 
  filter(district != "21",
         district != "22",
         !is.na(district))


graph_loan_amount_map <- congress_districts %>%
  filter(statefp == 42) %>% 
  left_join(ppp_district, by = c("cd116fp" = "district")) %>% 
  ggplot() +
  geom_sf(aes(fill = loan_amount), size = .5) +
  #geom_sf_label(aes(label = cd116fp), size = 1.5) +
  scale_fill_viridis_c(label = scales::dollar) +
  labs(fill = "Loan total") +
  theme_void()

graph_loan_count <- congress_districts %>%
  filter(statefp == 42) %>% 
  left_join(ppp_district, by = c("cd116fp" = "district")) %>% 
  ggplot() +
  geom_sf(aes(fill = loan_count), size = .5) +
  scale_fill_viridis_c(label = scales::comma) +
  labs(fill = "Loan count") +
  theme_void()

graph_bar <- ppp_district %>% 
  mutate(district = fct_reorder(district, loan_amount)) %>% 
  ggplot(aes(loan_amount, district)) +
  geom_col() +
  scale_x_continuous(label = scales::dollar) +
  labs(x = "Total",
       y = "Congressional District")

plot <- graph_bar + (graph_loan_amount_map / graph_loan_count) +
  #plot_layout(ncol = 2) +
  plot_annotation(title = "PPP Loan Distributions in Pennsylvania",
                  subtitle = "By Congressional District",
                  caption = "@conor_tompkins, data from SBA")
plot %>% 
  ggsave(filename = "output/ppp/loan_distribution.png", width = 18, height = 12)
  

