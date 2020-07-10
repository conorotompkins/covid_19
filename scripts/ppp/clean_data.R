library(tidyverse)
library(lubridate)
library(janitor)

options(scipen = 999, digits = 4)

#https://home.treasury.gov/policy-issues/cares-act/assistance-for-small-businesses/sba-paycheck-protection-program-loan-level-data

path <- "data/ppp"

#find all files in the folder that end in .csv
ppp <- list.files(path, full.names = TRUE, recursive = TRUE) %>% 
  keep(str_detect(., ".csv$")) %>% 
  #read each file with read_csv and combine them
  set_names() %>% 
  map_dfr(read_csv, col_types = cols(.default = "c"), .id = "file_name") %>% 
  clean_names() %>% 
  mutate(jobs_retained = as.numeric(jobs_retained),
         date_approved = mdy(date_approved),
         loan_amount = as.numeric(loan_amount),
         file_name = str_remove(file_name, path)) %>% 
  #separate cd column into state_2 and district
  separate(cd, into = c("state_2", "district"), remove = FALSE) %>% 
  #when district is blank, replace with NA
  mutate(district = case_when(district == "" ~ NA_character_,
                          district != "" ~ district))

