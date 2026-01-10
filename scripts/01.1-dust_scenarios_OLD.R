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
current_scenario = 1278
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

scenarios_hourly <- do.call(rbind , tables)%>%
  mutate(scenario = str_sub(id,-32,-29),
         h = str_sub(X,-4,-1),
         md = str_sub(X,5,8))%>%
  select(-c(id, X))%>%
  select(scenario, h, md, everything())%>%
  pivot_longer(6:ncol(.), names_to = "centroid_name", values_to = "pm25")%>%
  group_by(scenario, h, md, centroid_name)%>%
  summarise(pm25 = mean(pm25))%>%
  ungroup%>%
  mutate(pm10 = pm25*9)

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

combos <- expand.grid("h" = unique(scenarios_hourly$h),
                    "md" = unique(scenarios_hourly$md),
                    "scenario" = unique(scenarios_hourly$scenario),
                    "centroid_name" = unique(scenarios_hourly$centroid_name)
)

scenario_pm_deltas_hourly <- scenarios_hourly %>%
  mutate(event = str_sub(md, 1, 2)) %>%
  left_join(scenarios_hourly %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_pm10 = pm10,
                     baseline_pm25 = pm25)%>%
              dplyr::select(-scenario)
            , by = c("h", "md", "centroid_name")
  )%>%
  mutate(pm10_delta = pm10 - baseline_pm10,
         pm25_delta = pm25 - baseline_pm25)%>%
  select(-c(pm10, pm25, baseline_pm10, baseline_pm25))%>%
  #right_join(combos, by = c("h", "md", "scenario", "centroid_name"))%>%
  filter(scenario != baseline_scenario)%>%
  right_join(ct_centroids, by = "centroid_name")%>%
  drop_na(scenario)


# plot one of the events by hour
event_1 <- scenario_pm_deltas_hourly %>%
  filter(event %in% "04") %>%
  group_by(h, md, scenario) %>%
  summarise(pm10_delta = mean(pm10_delta, na.rm = T))%>%
  ungroup %>%
  group_by(scenario) %>%
  arrange(md, h) %>%
  mutate(event_hour = 1:n())
  
# get pollution from event one by event time
event_1 %>%
  ggplot(aes(x = event_hour, y = pm10_delta, color = scenario))+
  ylab("Particulate Matter Exposure") + xlab("Storm hour")+
  geom_line()+
  theme_minimal()

# plot the other by hour
event_2 <- scenario_pm_deltas_hourly %>%
  filter(event %in% "05") %>%
  group_by(h, md, scenario) %>%
  summarise(pm10_delta = mean(pm10_delta, na.rm = T))%>%
  ungroup %>%
  group_by(scenario) %>%
  arrange(md, h) %>%
  mutate(event_hour = 1:n())
  
event_2 %>%  
  ggplot(aes(x = event_hour, y = pm10_delta, color = scenario))+
  ylab("Particulate Matter Exposure") + xlab("Storm hour")+
  geom_line()+
  theme_minimal()

event_1_length <- max(event_1$event_hour)
event_2_length <- max(event_2$event_hour)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Try interpolation
# library(zoo)
# event_2[, c := exp(na.spline(log(pm10_delta), x = , na.rm = FALSE)), by = group]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scenario_pm_deltas_event <- scenario_pm_deltas_hourly %>%
  group_by(scenario, event, FIPS) %>%
  summarise(pm10_delta = mean(pm10_delta),
            pm25_delta = mean(pm25_delta))%>%
  ungroup %>%
  mutate(event_length = case_when(event == "04" ~ event_1_length,
                                  event == "05" ~ event_2_length),
         pm10_delta = ifelse(event_length < 24, pm10_delta*event_length/24, pm10_delta),
         pm25_delta = ifelse(event_length < 24, pm25_delta*event_length/24, pm25_delta),
         event_days = case_when(event_length < 24 ~ 1,
                                event_length >= 24 ~ event_length/24),
  )

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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Write data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#write.csv(scenario_pm_deltas_daily, file = "processed/scenario_pm_deltas_daily.csv", row.names = FALSE)
write.csv(scenario_pm_deltas_event, file = "processed/scenario_pm_deltas_event.csv", row.names = FALSE)

