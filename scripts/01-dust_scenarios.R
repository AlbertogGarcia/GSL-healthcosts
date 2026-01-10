# GSL dust costs: Organize dust scenarios
# albert.garcia@utah.edu
# created: 06/20/2025
# updated: 01/05/2026

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

baseline_scenario = 4203
current_scenario = 4192

# bad: 4183
# optimistic 4200
# target: 4198
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### load csvs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files  <- list.files("data/dust/updated", pattern = '\\.csv', full.names = T)
tables <- lapply(seq_along(files), function(x) cbind(read.csv(files[x], header = TRUE), id=files[x]))

scenarios_avgStorm <- do.call(rbind , tables)%>%
  mutate(scenario = str_sub(id,-10,-7))%>%
  select(-c(id, X))%>%
  select(scenario, everything())%>%
  pivot_longer(2:ncol(.), names_to = "centroid_name", values_to = "pm25")%>%
  group_by(scenario, centroid_name)%>%
  summarise(pm25 = mean(pm25, na.rm = T))%>%
  ungroup%>%
  mutate(pm10 = pm25*9)# Assumes pm10 makes up 90% of the total mass and pm2.5 10%

scenarios <- do.call(rbind , tables)%>%
  # 1. Convert timestamp to datetime
  mutate(scenario = str_sub(id,-10,-7),
         timestamp = ymd_hm(X)) %>%  # lubridate parses YYYYMMDDHHMM
  # 2. Arrange by datetime
  arrange(timestamp) %>%
  # 3. Detect consecutive-day event groups
  mutate(event = cumsum(c(TRUE, diff(as.Date(timestamp)) > 1))) %>%
  # 4. Calculate event duration in hours
  group_by(event) %>%
  mutate(event_hours = as.numeric(difftime(max(timestamp), min(timestamp), units = "hours"))) %>%
  ungroup() %>%
  select(-c(id, X))%>%
  select(scenario, timestamp, event, event_hours, everything()) %>%
  pivot_longer(5:ncol(.), names_to = "centroid_name", values_to = "pm25")

table(year(scenarios$timestamp))

scenarios_event <- scenarios %>%
  group_by(scenario, event, event_hours, centroid_name)%>%
  summarise(pm25 = mean(pm25, na.rm = T))%>%
  ungroup %>%
  mutate(pm10 = pm25*9)

table(scenarios_event$event_hours)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Match centroids back to "real" census tracts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_centroids <- st_as_sf(read.csv("data/dust/centroid_locations/centroid_location.csv"), 
                         coords = c("lon", "lat"), 
                         crs = 4326) %>%
  st_join(read_sf("data/gis/tracts/CensusTracts2020.shp")%>%
            rename(FIPS = GEOID20)%>%
            select(FIPS) %>% 
            st_transform(crs = 4326)
          )%>%
  st_drop_geometry()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Get relative to "baseline scenario" of 4203ft
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scenario_pm_deltas_event <- scenarios_event %>%
  left_join(scenarios_event %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_pm10 = pm10,
                     baseline_pm25 = pm25)%>%
              dplyr::select(-c(scenario, event_hours))
            , by = c("event", "centroid_name")
  )%>%
  mutate(pm10_delta = pm10 - baseline_pm10,
         pm25_delta = pm25 - baseline_pm25,
         # If the event is less than 24 hours, get the 24 hour average
         # assumes non-reported hours had no pollution
         pm10_delta = ifelse(event_hours < 24, pm10_delta*event_hours/24, pm10_delta),
         pm25_delta = ifelse(event_hours < 24, pm25_delta*event_hours/24, pm25_delta),
         event_days = event_hours/24
         )%>%
  select(-c(pm10, pm25, baseline_pm10, baseline_pm25))%>%
  filter(scenario != baseline_scenario)%>%
  full_join(ct_centroids, by = "centroid_name")%>%
  drop_na(scenario)

# make sure there are no duplicate census tract, event, scenario triples--should be 1
nrow(scenario_pm_deltas_event %>% select(centroid_name, scenario, event) %>% distinct())/nrow(scenario_pm_deltas_event)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Get "current" and "relative" deltas
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
current_pm_deltas_event <- scenario_pm_deltas_event %>%
  filter(scenario == current_scenario) %>%
  rename(baseline_pm10_delta = pm10_delta,
         baseline_pm25_delta = pm25_delta)%>%
  select(event, FIPS, baseline_pm10_delta, baseline_pm25_delta)

scenario_pm_deltas_event <- scenario_pm_deltas_event %>%
  left_join(current_pm_deltas_event, by = c("event", "FIPS"))%>%
  mutate(relative_pm10_delta = pm10_delta - baseline_pm10_delta,
         relative_pm25_delta = pm25_delta - baseline_pm25_delta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Write csv
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write.csv(scenario_pm_deltas_event, file = "processed/scenario_pm_deltas_event.csv", row.names = FALSE)
