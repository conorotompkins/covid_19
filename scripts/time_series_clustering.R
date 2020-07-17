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

covid <- covid %>% 
  as_tsibble(key = state, index = date) %>% 
  fill_gaps(.full = FALSE) %>% 
  fill(cases, .direction = "down") 

#figure out if i should scale & center by state or overall

covid <- covid %>% 
  left_join(covid_10th_case, by = c("state" = "state")) %>% 
  group_by(state) %>% 
  mutate(days_since_10th_case = date - date_of_10th_case,
         cases_scaled = scale(cases, center = TRUE, scale = TRUE)) %>% 
  ungroup() %>% 
  filter(days_since_10th_case >= 0)
  

covid <- covid %>% 
  select(state, days_since_10th_case, cases_scaled)

covid %>% 
  ggplot(aes(days_since_10th_case, cases_scaled, group = state)) +
  geom_line(alpha = .2)

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
                                preproc = NULL, 
                                args = tsclust_args(dist = list(window.size = 5L)))
}

cluster_dtw_h

slotNames(cluster_dtw_h[[2]])
slot(cluster_dtw_h[[5]], "cldist")

cluster_dtw_h

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

get_cluster_assigments(object = cluster_dtw_h, cluster_number = 2)

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

covid %>% 
  as_tibble() %>% 
  #left_join(filter(cluster_assigments, kclust == 2)) %>% 
  left_join(cluster_assigments) %>% 
  filter(kclust < 8) %>% 
  ggplot(aes(days_since_10th_case, cases_scaled, color = as.factor(value), group = state)) +
  geom_line() +
  facet_grid(kclust~as.factor(value))


#map


