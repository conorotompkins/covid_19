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

options(scipen = 999, digits = 4)

theme_set(theme_ipsum())

set.seed(1234)

census_data <- get_acs(geography = "state", variables = "B01003_001", geometry = FALSE) %>% 
  select(state = NAME, population = estimate)


covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  arrange(state, date) %>% 
  semi_join(census_data)

#calculate days since 10th cases
covid_10th_case <- covid %>% 
  filter(cases >= 10) %>% 
  group_by(state) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(state, date_of_10th_case = date)

covid <- covid %>% 
  left_join(covid_10th_case, by = c("state" = "state")) %>% 
  group_by(state) %>% 
  mutate(days_since_10th_case = date - date_of_10th_case) %>% 
  ungroup() %>% 
  #complete(state, days_since_10th_case) %>% 
  #fill(contains("cases"), .direction = "down") %>% 
  filter(days_since_10th_case >= 0)


covid <- covid %>% 
  select(state, days_since_10th_case, cases)

#calculate cases per capita

covid <- covid %>% 
  left_join(census_data) %>% 
  mutate(cases_per_capita = (cases / population) * 100000) %>% 
  select(-population)

#calculate scaled caes
covid <- covid %>% 
  group_by(state) %>% 
  mutate(cases_per_capita_scaled = scale(cases_per_capita, center = TRUE, scale = TRUE)) %>% 
  ungroup()

covid %>% 
  ggplot(aes(days_since_10th_case, cases_per_capita_scaled, group = state)) +
  geom_line(alpha = .4)

#unstack into seris of lists
covid_list <- covid %>% 
  select(state, cases_per_capita_scaled) %>% 
  unstack(cases_per_capita_scaled ~ state)

#loop through clustering algorithm 20 times
cluster_dtw_h <- list()

number_of_clusters <- 20

for (i in 2:number_of_clusters){
  cluster_dtw_h[[i]] <- tsclust(covid_list, 
                                type = "h", 
                                k = i,
                                #centroid = median,
                                distance = "dtw", 
                                control = hierarchical_control(method = "complete"), 
                                seed = 390, 
                                preproc = NULL, 
                                args = tsclust_args(dist = list(window.size = 21L)))
  
  print(i)
}


#inspect object
str(cluster_dtw_h[[2]])

slotNames(cluster_dtw_h[[2]])
slot(cluster_dtw_h[[5]], "cldist")


#function to pull cluster assignments
#change "value" to "cluster_assigment"
get_cluster_assigments <- function(object, cluster_number){
  
  df <- slot(object[[cluster_number]], "cluster")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

#pull cluster assignments
cluster_assigments <- 2:number_of_clusters %>%
  set_names() %>% 
  map_df(~get_cluster_assigments(cluster_dtw_h, cluster_number = .x), .id = "kclust") %>% 
  pivot_longer(cols = -kclust, names_to = "state", values_to = "cluster_assignment") %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  arrange(state, kclust)

##review cluster assigments
state_variance <- cluster_assigments %>% 
  distinct(state, cluster_assignment) %>% 
  count(state, sort = TRUE)

cluster_assigments %>%
  left_join(state_variance) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(kclust, state, fill = as.factor(cluster_assignment))) +
  geom_tile() +
  scale_fill_viridis_d()

cluster_assigments %>% 
  distinct(state, cluster_assignment) %>% 
  count(state) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(n, state)) +
  geom_col() +
  labs(title = "How much does each state react to a change in kclust?")

#cluster 6 is the first with a singelton cluster
cluster_assigments %>% 
  count(kclust, cluster_assignment) %>% 
  group_by(kclust) %>% 
  mutate(min_cluster_population = min(n)) %>% 
  ungroup() %>% 
  #filter(min_clusters > 2) %>% 
  count(kclust, min_cluster_population) %>% 
  mutate(first_singleton = cumsum(min_cluster_population == 1) == 1) %>% 
  filter(first_singleton == TRUE)

cluster_assigments %>% 
  count(kclust, cluster_assignment) %>% 
  ggplot(aes(kclust, n, color = as.factor(cluster_assignment))) +
  geom_jitter(show.legend = FALSE)

#function to get cluster metrics
get_cluster_metrics <- function(object, cluster_number){
  
  df <- slot(object[[cluster_number]], "clusinfo")
  # for (i in 2:number_of_clusters)
  # {
  #   df <- slot(object[[i]], "cluster")
  # 
  #   return(df)
  # }
  
  return(df)
}

#get cluster metrics and review
2:number_of_clusters %>%
  set_names() %>% 
  map_df(~get_cluster_metrics(cluster_dtw_h, cluster_number = .x), .id = "kclust") %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  ggplot(aes(kclust, av_dist)) +
  geom_jitter(aes(color = as.factor(kclust), size = size), show.legend = FALSE) +
  geom_smooth(group = 1) +
  geom_vline(xintercept = 8, linetype = 2) +
  scale_size_continuous(range = c(.5, 4))

#figure out way to find kclust with biggest decrease in av_dist with fewest singleton clusters

#write function to get cluster datalists
get_cluster_datalist <- function(object, cluster_number){
  
  df <- slot(object[[cluster_number]], "datalist")
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


#compare silhouettes of clusters
big_comparison_plot <- covid %>% 
  as_tibble() %>% 
  left_join(cluster_assigments) %>% 
  ggplot(aes(days_since_10th_case, cases_per_capita_scaled, 
             color = as.factor(cluster_assignment), group = state)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(kclust~as.factor(cluster_assignment))

big_comparison_plot %>% 
  ggsave(filename = "output/time_series_clustering/comparison_plot.png", width = 24, height = 24)




best_kclust <- 8

cluster_assigments %>% 
  filter(kclust == best_kclust)

covid %>% 
  as_tibble() %>% 
  left_join(filter(cluster_assigments, kclust == best_kclust)) %>% 
  ggplot(aes(days_since_10th_case, cases_per_capita_scaled, 
             color = as.factor(cluster_assignment), group = state)) +
  geom_line() +
  #geom_line(data = filter(covid, state == "Alaska") %>% 
  #            left_join(filter(cluster_assigments, kclust == best_kclust)), 
  #          size = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 60, linetype = 2) +
  facet_wrap(kclust~as.factor(cluster_assignment), ncol = 4)

#map
# census_vars <- load_variables(dataset = "acs1", year = 2018)
# 
# census_vars %>% 
#   mutate(across(c(label, concept), str_to_lower)) %>% 
#   filter(concept == "total population")

map <- get_acs(geography = "state", variables = "B01003_001", geometry = TRUE, shift_geo = TRUE)

cluster_assigments %>% 
  filter(kclust == best_kclust)

map_cluster <- map %>% 
  left_join(cluster_assigments %>% 
             filter(kclust == best_kclust), by = c("NAME" = "state")) %>% 
  add_count(cluster_assignment) %>% 
  mutate(cluster_assignment = as.character(cluster_assignment),
         cluster_assignment = fct_reorder(cluster_assignment, desc(n))) %>% 
  group_by(cluster_assignment) %>% 
  summarize()

state_clustered <- map %>% 
  left_join(cluster_assigments %>% 
              filter(kclust == best_kclust), by = c("NAME" = "state")) %>% 
  add_count(cluster_assignment) %>% 
  mutate(cluster_assignment = as.character(cluster_assignment),
         cluster_assignment = fct_reorder(cluster_assignment, desc(n)))

map_cluster %>% 
  ggplot() +
  geom_sf(aes(fill = cluster_assignment),
          size = 1) +
  geom_sf(data = map, color = "grey", size = .1, alpha = 0) +
  geom_sf_label(data = state_clustered, 
                aes(label = cluster_assignment, color = cluster_assignment), 
                alpha = .8,
                size = 3,
                show.legend = FALSE)

cluster_assigments %>% 
  filter(kclust == best_kclust) %>% 
  count(cluster_assignment, sort = TRUE) %>% 
  mutate(cluster_assignment = as.character(cluster_assignment),
         cluster_assignment = fct_reorder(cluster_assignment, desc(n))) %>% 
  ggplot(aes(n, cluster_assignment, fill = cluster_assignment)) +
  geom_col(color = "black", show.legend = FALSE)

cluster_assigments %>% 
  filter(kclust == best_kclust) %>% 
  View()

covid %>% 
  left_join(cluster_assigments) %>% 
  mutate(days_since_10th_case = as.numeric(days_since_10th_case)) %>% 
  pivot_longer(cols = contains("cases"), names_to = "metric", values_to = "value") %>% 
  write_csv("output/time_series_clustering/time_series_data_clustered.csv")
