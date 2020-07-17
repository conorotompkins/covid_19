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


theme_set(theme_ipsum())

set.seed(1234)

covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  arrange(state, date)

#calculate days since 10th cases
covid_10th_case <- covid %>% 
  filter(cases >= 10) %>% 
  group_by(state) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(state, date_of_10th_case = date)

#figure out if i should scale & center by state or overall

covid <- covid %>% 
  left_join(covid_10th_case, by = c("state" = "state")) %>% 
  group_by(state) %>% 
  mutate(days_since_10th_case = date - date_of_10th_case) %>% 
  mutate(cases_scaled = scale(cases, center = TRUE, scale = TRUE)) %>% 
  ungroup() %>% 
  #complete(state, days_since_10th_case) %>% 
  #fill(contains("cases"), .direction = "down") %>% 
  filter(days_since_10th_case >= 0)
  

covid <- covid %>% 
  select(state, days_since_10th_case, cases, cases_scaled)

covid %>% 
  ggplot(aes(days_since_10th_case, cases_scaled, group = state)) +
  geom_line(alpha = .4)

covid_list <- covid %>% 
  select(state, cases_scaled) %>% 
  unstack(cases_scaled ~ state)

covid_list[1]

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
                                preproc = NULL#, 
                                #args = tsclust_args(dist = list(window.size = 5L)
                                                    )
}

cluster_dtw_h

slotNames(cluster_dtw_h[[2]])
slot(cluster_dtw_h[[5]], "cldist")


#function to pull cluster assigments
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
  pivot_longer(cols = -kclust, names_to = "state") %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  arrange(state, kclust)

cluster_assigments %>%
  group_by(state) %>% 
  mutate(max_cluster = max(value)) %>% 
  ungroup() %>% 
  mutate(state = fct_reorder(state, max_cluster)) %>% 
  ggplot(aes(kclust, state, fill = as.factor(value))) +
  geom_tile() +
  scale_fill_viridis_d()

cluster_assigments %>% 
  distinct(state, value) %>% 
  count(state) %>% 
  mutate(state = fct_reorder(state, n)) %>% 
  ggplot(aes(n, state)) +
  geom_col() +
  labs(title = "How much does each state react to a change in kclust?")


#function to get cluster info
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

#get cluster metrics
2:number_of_clusters %>%
  set_names() %>% 
  map_df(~get_cluster_metrics(cluster_dtw_h, cluster_number = .x), .id = "kclust") %>% 
  mutate(kclust = as.numeric(kclust)) %>% 
  ggplot(aes(kclust, av_dist)) +
  geom_jitter(aes(color = as.factor(kclust), size = size), show.legend = FALSE) +
  geom_smooth(group = 1) +
  scale_size_continuous(range = c(.5, 4))


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
  #left_join(filter(cluster_assigments, kclust == 2)) %>% 
  left_join(cluster_assigments) %>% 
  #filter(between(kclust, 10, 15)) %>% 
  ggplot(aes(days_since_10th_case, cases_scaled, 
             color = as.factor(value), group = state)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(kclust~as.factor(value))

big_comparison_plot %>% 
  ggsave(filename = "output/time_series_clustering/comparison_plot.png", width = 24, height = 24)


cluster_assigments %>% 
  count(kclust, value) %>% 
  group_by(kclust) %>% 
  mutate(min_clusters = min(value)) %>% 
  ungroup() %>% 
  #filter(min_clusters > 2) %>% 
  count(min_clusters)

cluster_assigments %>% 
  count(kclust, value) %>% 
  ggplot(aes(kclust, n, color = as.factor(value))) +
  geom_jitter(show.legend = FALSE)


#map
census_vars <- load_variables(dataset = "acs1", year = 2018)


census_vars %>% 
  mutate(across(c(label, concept), str_to_lower)) %>% 
  filter(concept == "total population")
  filter(str_detect(concept, "total population"))

map <- get_acs(geography = "state", variables = "B01003_001", geometry = TRUE, shift_geo = TRUE)

map %>% 
  ggplot() +
  geom_sf(aes(fill = estimate)) +
  scale_fill_viridis_c()

cluster_assigments %>% 
  filter(kclust == 13)

map %>% 
  left_join(cluster_assigments %>% 
              filter(kclust == 13), by = c("NAME" = "state")) %>% 
  add_count(value) %>% 
  mutate(value = as.character(value),
         value = fct_reorder(value, desc(n))) %>% 
  group_by(value) %>% 
  summarize() %>% 
  ggplot() +
  geom_sf(aes(fill = value))

cluster_assigments %>% 
  filter(kclust == 13) %>% 
  count(value, sort = TRUE) %>% 
  mutate(value = as.character(value),
         value = fct_reorder(value, desc(n))) %>% 
  ggplot(aes(n, value, fill = value)) +
  geom_col(color = "black", show.legend = FALSE)

cluster_assigments %>% 
  filter(kclust == 13) %>% 
  View()
  
