# GSL dust costs: Organize dust scenarios
# albert.garcia@utah.edu
# created: 06/20/2025
# updated: 

# Set up environment ########################################

# load or install necessary libraries. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(mapview,  # view spatial data in viewer
               measurements,  # convert units easily
               progress,  # progress bar
               scales, # add commas long numbers
               sf,  # shapefile read
               tidyverse, # tidyverse
               stringr,
               fuzzyjoin,
               ggplot2
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set basic parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

baseline_scenario = 1282

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### load csvs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files  <- list.files("data/dust", pattern = '\\.csv', full.names = T)
tables <- lapply(seq_along(files), function(x) cbind(read.csv(files[x], header = TRUE), id=files[x]))
scenarios <- do.call(rbind , tables)%>%
  mutate(scenario = str_sub(id,-32,-29))%>%
  select(-c(id, X))%>%
  select(scenario, everything())%>%
  pivot_longer(2:ncol(.), names_to = "centroid_name", values_to = "pm25")%>%
  group_by(scenario, centroid_name)%>%
  summarise(pm25 = mean(pm25))%>%
  ungroup%>%
  mutate(pm10 = pm25*9)# Assumes pm10 makes up 90% of the total mass and pm2.5 10%

scenarios_daily <- do.call(rbind , tables)%>%
  mutate(scenario = str_sub(id,-32,-29),
         d = str_sub(X,7,8),
         md = str_sub(X,5,8))%>%
  select(-c(id, X))%>%
  select(scenario, d, md, everything())%>%
  pivot_longer(4:ncol(.), names_to = "centroid_name", values_to = "pm25")%>%
  group_by(scenario, d, md, centroid_name)%>%
  summarise(pm25 = mean(pm25))%>%
  ungroup%>%
  mutate(pm10 = pm25*9)

table(scenarios_daily$md)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Match centroids back to "real" census tracts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tracts.shp <- read_sf("data/gis/tracts/CensusTracts2020.shp")%>%
  rename(FIPS = GEOID20)%>%select(FIPS)

# centroids from Derek's dust models
#pop_centroids <- read_sf("data/dust/centroid_locations/pop_centroids.shp")

centroid_location <- read.csv("data/dust/centroid_locations/centroid_location.csv")

ct_centroids <- st_as_sf(centroid_location, coords = c("lon", "lat"), 
         crs = 4326)%>%
  st_join(tracts.shp %>% st_transform(crs = 4326))%>%
  st_drop_geometry()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Get deltas relative to 1282 mASL
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scenario_pm_deltas_daily <- scenarios_daily %>%
  left_join(scenarios_daily %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_pm10 = pm10,
                     baseline_pm25 = pm25)%>%
              dplyr::select(-scenario)
            , by = c("d", "md", "centroid_name")
  )%>%
  mutate(pm10_delta = pm10 - baseline_pm10,
         pm25_delta = pm25 - baseline_pm25)%>%
  select(-c(pm10, pm25, baseline_pm10, baseline_pm25))%>%
  filter(scenario != baseline_scenario)%>%
  right_join(ct_centroids, by = "centroid_name")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Write data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write.csv(scenario_pm_deltas_daily, file = "processed/scenario_pm_deltas_daily.csv", row.names = FALSE)
