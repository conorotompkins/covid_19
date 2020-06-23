I am trying to turn US county geometries into hexagon tilemaps. The goal is to have a simple features object with a hexagon for each county.

I tried using `geogrid`, but it hangs and never finishes calculating. This test code shows that it works in smaller scales.

```
library(tidyverse)
library(tidycensus)
library(janitor)
library(geogrid)

options(scipen = 999, digits = 2)

df_ny <- get_decennial(geography = "county", state = "New York", variables = "P001001", geometry = TRUE) %>% 
  mutate(polygon_type = "normal") %>% 
  separate(NAME, into = c("county", "state"), sep = ",") %>% 
  mutate(across(c(county, state), str_trim))

df_ny %>% 
  ggplot() +
  geom_sf()

ny_new_cells_hex <- calculate_grid(shape = df_ny, grid_type = "hexagonal", seed = 3)
df_ny_hex <- assign_polygons(df_ny, ny_new_cells_hex) 

df_ny_hex %>% 
  ggplot() +
  geom_sf()
```

`tilemaps` seems much faster, but it cannot handle non-contiguous polygons. I can deal with handling Hawaii and Alaska separately, but some counties are islands themselves, and there are hundreds of counties on the coast that have non-contiguous islands.

Sample `tilemaps` code:
  ```

```

I am looking for a way to "merge" disparate polygons while retaining their separate nature.

