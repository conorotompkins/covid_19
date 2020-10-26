#sources
# https://rpubs.com/esobolewska/dtw-time-series
# http://www.rdatamining.com/examples/time-series-clustering-classification
# http://rstudio-pubs-static.s3.amazonaws.com/398402_abe1a0343a4e4e03977de8f3791e96bb.html

#package
library(tidyverse)
library(tsibble)
library(dtwclust)
library(tidymodels)
library(hrbrthemes)
library(tidycensus)
library(sf)
library(furrr)

options(scipen = 999, digits = 4)

theme_set(theme_ipsum())

set.seed(1234)

census_data <- get_acs(geography = "county", variables = "B01003_001", geometry = FALSE) %>% 
  select(NAME, population = estimate) %>% 
  separate(NAME, into = c("county", "state"), sep = ", ") %>% 
  mutate(county = str_remove(county, " County$"),
         county = str_remove(county, " Parish$"),
         county = str_remove(county, " Municipality$"))


census_data <- census_data %>% 
  mutate(county = case_when(state == "New York" & county %in% c("New York", "Kings", "Queens", "Bronx", "Richmond") ~ "New York City",
                            TRUE ~ county))

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  #read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  select(state, county, date, cases) %>% 
  arrange(state, county, date) %>% 
  filter(county != "Unknown",
         state != "Puerto Rico",
         state != "Virgin Islands",
         state != "Northern Mariana Islands") %>% 
  mutate(county = str_remove(county, " Municipality$"))

first_date <- first(covid$date)
last_date <- last(covid$date)

date_range <- tibble(date = seq(from = first_date, to = last_date, by = "day"))

covid <- covid %>%
  as_tsibble(index = date, key = c(county, state)) %>% 
  fill_gaps() %>% 
  as_tibble() %>% 
  group_by(state, county) %>% 
  fill(cases, .direction = "down") %>% 
  ungroup()

covid %>% 
  as_tsibble(index = date, key = c(county, state)) %>% 
  count_gaps()

# covid %>% 
#   anti_join(census_data) %>% 
#   distinct(state, county) %>% 
#   View()
# 
# census_data %>% 
#   anti_join(covid) %>% 
#   distinct(state, county) %>% 
#   View()




#calculate days since 10th cases
covid_10th_case <- covid %>% 
  filter(cases >= 10) %>% 
  group_by(state, county) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(state, county, date_of_10th_case = date)

covid <- covid %>% 
  left_join(covid_10th_case, by = c("state", "county")) %>% 
  group_by(state, county) %>% 
  mutate(days_since_10th_case = date - date_of_10th_case,
         week_since_10th_case = as.numeric(days_since_10th_case) %/% 7) %>% 
  ungroup() %>% 
  filter(week_since_10th_case > 1)


covid <- covid %>% 
  select(state, county, week_since_10th_case, cases) %>% 
  group_by(state, county, week_since_10th_case) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup()

covid <- covid %>% 
  group_by(state, county) %>% 
  filter(week_since_10th_case != max(week_since_10th_case)) %>% 
  ungroup()

covid <- covid %>% 
  group_by(state, county) %>% 
  mutate(new_cases = cases - lag(cases)) %>% 
  ungroup() %>% 
  mutate(new_cases = case_when(new_cases < 0 ~ 0,
                               new_cases >= 0 ~ new_cases)) %>% 
  drop_na(new_cases)

covid %>% 
  mutate(id = str_c(state, county, sep = " ")) %>% 
  ggplot(aes(week_since_10th_case, new_cases, group = id)) +
  geom_line(alpha = .1)

covid %>% 
  mutate(id = str_c(state, county, sep = " ")) %>% 
  ggplot(aes(week_since_10th_case, id, fill = new_cases)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.y = element_blank())

#calculate cases per capita
covid <- covid %>% 
  left_join(census_data) %>% 
  mutate(new_cases_per_capita = (new_cases / population) * 100000) %>% 
  select(-population)

covid %>% 
  mutate(id = str_c(state, county, sep = " ")) %>% 
  ggplot(aes(week_since_10th_case, new_cases_per_capita, group = id)) +
  geom_line(alpha = .1)

covid %>% 
  mutate(id = str_c(state, county, sep = " ")) %>% 
  add_count(id) %>% 
  filter(n > 25) %>% 
  ggplot(aes(week_since_10th_case, id, fill = new_cases_per_capita)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.y = element_blank())

#unstack into seris of lists
covid %>% 
  mutate(id = str_c(state, county, sep = " ")) %>% 
  add_count(id) %>% 
  filter(n > 25) %>% 
  distinct(state, county)

covid %>% 
  filter(is.na(new_cases_per_capita)) %>% 
  distinct(state, county)

covid_list <- covid %>% 
  select(state, county, new_cases_per_capita) %>% 
  #filter out areas that did not join with census_data
  anti_join(covid %>% 
              filter(is.na(new_cases_per_capita)) %>% 
              distinct(state, county)) %>% 
  mutate(id = str_c(state, county, sep = " ")) %>% 
  add_count(id) %>% 
  filter(n > 25) %>% 
  unstack(new_cases_per_capita ~ id)


cluster_function <- function(number_of_clusters){
  
  message(str_c("Clustering", number_of_clusters, sep = ": "))
  
  cluster_object <- tsclust(covid_list, 
                            type = "h", 
                            k = number_of_clusters,
                            #centroid = median,
                            distance = "dtw", 
                            control = hierarchical_control(method = "complete"), 
                            seed = 390, 
                            preproc = NULL, 
                            args = tsclust_args(dist = list(window.size = 21L)))
  
  return(cluster_object)
}

number_of_clusters <- 5

plan(multisession, workers = 7)

cluster_dtw_h <- 2:number_of_clusters %>% 
  set_names() %>% 
  future_map(~cluster_function(number_of_clusters = .x), .progress = TRUE)

cluster_dtw_h %>% 
  keep(names(.) == 5) %>% 
  map_df(~slot(.x, "cluster")) %>% 
  pivot_longer(cols = everything(), names_to = "state", values_to = "cluster_assignment")

#inspect object
str(cluster_dtw_h[[4]])

slotNames(cluster_dtw_h[[2]])
slot(cluster_dtw_h[[2]], "cldist")


#function to pull cluster assignments
#change "value" to "cluster_assignment"
get_cluster_assignments <- function(object, cluster_number){
  
  df <- cluster_dtw_h %>% 
    keep(names(.) == cluster_number) %>% 
    map_df(~slot(.x, "cluster"))
  
  #df <- slot(object[[cluster_number]], "cluster")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

#pull cluster assignments
cluster_assignments <- 2:number_of_clusters %>%
  set_names() %>% 
  map_df(~get_cluster_assignments(cluster_dtw_h, cluster_number = .x), .id = "kclust") %>% 
  pivot_longer(cols = -kclust, names_to = "id", values_to = "cluster_assignment") %>% 
  separate(id, into = c("state", "county"), sep = " ", remove = FALSE) %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  arrange(state, county, kclust)

write_csv(cluster_assignments, "/Users/conortompkins/github_repos/covid_19/output/time_series_clustering/cluster_assignments_county.csv")

##review cluster assigments
county_variance <- cluster_assignments %>% 
  distinct(id, state, county, cluster_assignment) %>% 
  count(id, state, county, sort = TRUE)

cluster_assignments %>%
  left_join(state_variance) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(kclust, state, fill = as.factor(cluster_assignment))) +
  geom_tile() +
  scale_fill_viridis_d()

cluster_assignments %>% 
  distinct(state, cluster_assignment) %>% 
  count(state) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(n, state)) +
  geom_col() +
  labs(title = "How much does each state react to a change in kclust?")

#cluster 8 is the first with a singelton cluster
cluster_assignments %>% 
  count(kclust, cluster_assignment) %>% 
  group_by(kclust) %>% 
  mutate(min_cluster_population = min(n)) %>% 
  ungroup() %>% 
  filter(cluster_assignment == min_cluster_population) %>% 
  ungroup() %>% 
  select(kclust, min_cluster_population, n) %>% 
  mutate(first_singleton = cumsum(min_cluster_population == 1) == 1) %>%
  filter(first_singleton == TRUE)

cluster_assignments %>% 
  count(kclust, cluster_assignment) %>% 
  ggplot(aes(kclust, n, color = as.factor(cluster_assignment))) +
  geom_jitter(show.legend = FALSE)

#function to get cluster metrics
get_cluster_metrics <- function(object, cluster_number){
  
  df <- object %>% 
    keep(names(.) == cluster_number) %>% 
    map_df(~slot(.x, "clusinfo"))
  
  #df <- slot(object[[cluster_number]], "clusinfo")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

cluster_metrics <- 2:number_of_clusters %>%
  set_names() %>% 
  map_df(~get_cluster_metrics(cluster_dtw_h, cluster_number = .x), .id = "kclust")

write_csv(cluster_metrics, "output/time_series_clustering/cluster_metrics.csv")

#get cluster metrics and review
cluster_metrics %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  ggplot(aes(kclust, av_dist)) +
  geom_jitter(aes(color = as.factor(kclust), size = size), show.legend = FALSE) +
  geom_smooth(group = 1) +
  #geom_vline(xintercept = 6, linetype = 2) +
  scale_size_continuous(range = c(.5, 4))

#figure out way to find kclust with biggest decrease in av_dist with fewest singleton clusters

#write function to get cluster datalists
get_cluster_datalist <- function(object, cluster_number){
  
  df <- object %>% 
    keep(names(.) == cluster_number) %>% 
    map_df(~slot(.x, "datalist"))
  
  #df <- slot(object[[cluster_number]], "datalist")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

#get cluster centroids
2:number_of_clusters %>%
  #set_names() %>% 
  map(~get_cluster_datalist(cluster_dtw_h, cluster_number = .x), .id = "kclust")


#write function to get cluster centroids
get_cluster_distances <- function(object, cluster_number){
  
  df <- object %>% 
    keep(names(.) == cluster_number) %>% 
    map_df(~slot(.x, "cldist"))
  
  #df <- slot(object[[cluster_number]], "cldist")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

2:number_of_clusters %>%
  set_names() %>% 
  map(~get_cluster_distances(cluster_dtw_h, cluster_number = .x), .id = "kclust") %>% 
  map(~rename(., 1 = V1))

map(~as_tibble(.)) %>% 
  
  bind_rows(.id = "kclust") %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  ggplot(aes(kclust, V1)) +
  geom_jitter(alpha = .1) +
  geom_smooth()


get_cluster_centroids <- function(object, cluster_number){
  
  df <- object %>% 
    keep(names(.) == cluster_number) %>% 
    map(~slot(.x, "centroids"))
  
  #df <- slot(object[[cluster_number]], "centroids")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

centroids <- 2:number_of_clusters %>%
  #set_names() %>% 
  map(~get_cluster_centroids(cluster_dtw_h, cluster_number = .x), .id = "kclust")

str(centroids)

str(centroids[[2]])

plot(cluster_dtw_h[[7]], type = "sc")


#compare silhouettes of clusters
big_comparison_plot <- covid %>% 
  as_tibble() %>% 
  left_join(cluster_assignments) %>% 
  ggplot(aes(week_since_10th_case, new_cases_per_capita, 
             color = as.factor(cluster_assignment), group = state)) +
  #geom_line(alpha = .3) +
  geom_point(alpha = .1, size = .3) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(kclust~as.factor(cluster_assignment)) +
  theme(panel.grid.minor = element_blank())

big_comparison_plot %>% 
  ggsave(filename = "output/time_series_clustering/comparison_plot.png", width = 24, height = 24)




best_kclust <- 9

cluster_assignments %>% 
  filter(kclust == best_kclust)

covid %>% 
  as_tibble() %>% 
  left_join(filter(cluster_assignments, kclust == best_kclust)) %>% 
  add_count(cluster_assignment) %>% 
  mutate(cluster_assignment = str_c("Cluster", cluster_assignment, sep = " "),
         cluster_assignment = fct_reorder(as.character(cluster_assignment), n),
         cluster_assignment = fct_rev(cluster_assignment)) %>% 
  ggplot(aes(week_since_10th_case, new_cases_per_capita, 
             color = cluster_assignment, group = state)) +
  geom_line(alpha = .5) +
  #geom_point(alpha = .4, size = .3) +
  #geom_line(data = filter(covid, state == "Alaska") %>% 
  #            left_join(filter(cluster_assignments, kclust == best_kclust)), 
  #          size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = 2) +
  #geom_vline(xintercept = 60, linetype = 2) +
  facet_wrap(~cluster_assignment, ncol = 3) +
  guides(color = FALSE) +
  theme(panel.grid.minor = element_blank())

covid %>% 
  as_tibble() %>% 
  left_join(filter(cluster_assignments, kclust == best_kclust)) %>% 
  add_count(cluster_assignment) %>% 
  mutate(cluster_assignment = str_c("Cluster", cluster_assignment, sep = " "),
         cluster_assignment = fct_reorder(as.character(cluster_assignment), n),
         cluster_assignment = fct_rev(cluster_assignment)) %>% 
  ggplot(aes(week_since_10th_case, new_cases_per_capita, 
             color = cluster_assignment)) +
  geom_line(aes(group = state), alpha = .1) +
  geom_smooth(aes(group = cluster_assignment), se = FALSE, span = .3)

selected_kclust <- 9

covid %>% 
  left_join(cluster_assignments %>% 
              filter(kclust == selected_kclust)) %>% 
  mutate(state = tidytext::reorder_within(state, new_cases_per_capita, cluster_assignment, .fun = min)) %>% 
  ggplot(aes(week_since_10th_case, state, fill = new_cases_per_capita)) +
  geom_tile() +
  facet_wrap(~cluster_assignment, scales = "free_y") +
  scale_fill_viridis_c() +
  scale_x_continuous(expand = c(0,0)) +
  tidytext::scale_y_reordered(expand = c(0,0)) +
  #scale_y_discrete(expand = c(0,0)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#map
# census_vars <- load_variables(dataset = "acs1", year = 2018)
# 
# census_vars %>% 
#   mutate(across(c(label, concept), str_to_lower)) %>% 
#   filter(concept == "total population")

map <- get_acs(geography = "state", variables = "B01003_001", geometry = TRUE, shift_geo = TRUE)

cluster_assignments %>% 
  filter(kclust == best_kclust)

map_cluster <- map %>% 
  left_join(cluster_assignments %>% 
              filter(kclust == best_kclust), by = c("NAME" = "state")) %>% 
  add_count(cluster_assignment) %>% 
  mutate(cluster_assignment = as.character(cluster_assignment),
         cluster_assignment = fct_reorder(cluster_assignment, desc(n))) %>% 
  group_by(cluster_assignment) %>% 
  summarize()

state_clustered <- map %>% 
  left_join(cluster_assignments %>% 
              filter(kclust == best_kclust), by = c("NAME" = "state")) %>% 
  add_count(cluster_assignment) %>% 
  mutate(cluster_assignment = as.character(cluster_assignment),
         cluster_assignment = fct_reorder(cluster_assignment, desc(n)))

map_cluster %>% 
  ggplot() +
  geom_sf(aes(fill = cluster_assignment),
          size = 1) +
  geom_sf(data = map, color = "grey", size = .1, alpha = 0) +
  theme_void()

cluster_assignments %>% 
  filter(kclust == best_kclust) %>% 
  count(cluster_assignment, sort = TRUE) %>% 
  mutate(cluster_assignment = as.character(cluster_assignment),
         cluster_assignment = fct_reorder(cluster_assignment, n)) %>% 
  ggplot(aes(n, cluster_assignment, fill = cluster_assignment)) +
  geom_col(color = "black", show.legend = FALSE)

cluster_assignments %>% 
  filter(kclust == best_kclust) %>% 
  View()

covid %>% 
  left_join(cluster_assignments) %>% 
  mutate(days_since_10th_case = as.numeric(days_since_10th_case)) %>%
  pivot_longer(cols = contains("cases"), names_to = "metric", values_to = "value") %>% 
  mutate(metric = case_when(metric == "cases" ~ "Cases",
                            metric == "cases_per_capita" ~ "Cases per 100,000",
                            metric == "cases_per_capita_scaled" ~ "Cases per 100,000 scaled")) %>% 
  write_csv("output/time_series_clustering/time_series_data_clustered.csv")
