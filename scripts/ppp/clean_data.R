library(tidyverse)
library(lubridate)
library(janitor)

#https://home.treasury.gov/policy-issues/cares-act/assistance-for-small-businesses/sba-paycheck-protection-program-loan-level-data

ppp <- list.files("data/ppp", full.names = TRUE, recursive = TRUE) %>% 
  keep(str_detect(., ".csv$")) %>% 
  set_names() %>% 
  map_dfr(read_csv, col_types = cols(.default = "c"), .id = "file_name") %>% 
  clean_names() %>% 
  mutate(jobs_retained = as.numeric(jobs_retained),
         date_approved = mdy(date_approved),
         loan_amount = as.numeric(loan_amount)) %>% 
  separate(cd, into = c("state_2", "district")) %>% 
  mutate(district = case_when(district == "" ~ NA_character_,
                          district != "" ~ district)) %>% 
  select(-state_2)

# 
# ppp %>% 
#   distinct(file_name)
#   
# pa <- ppp %>% 
#   filter(str_detect(file_name, "_PA"))
# 
# pa %>% 
#   glimpse()
# 
# pa %>% 
#   distinct(business_type)
# 
# pa %>% 
#   distinct(race_ethnicity)
# 
# pa %>% 
#   distinct(veteran)
# 
# pa %>% 
#   distinct(non_profit)



