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
#### load csvs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files  <- list.files("data/dust", pattern = '\\.csv', full.names = T)
tables <- lapply(seq_along(files), function(x) cbind(read.csv(files[x], header = TRUE), id=files[x]))
scenarios <- do.call(rbind , tables)%>%
  mutate(scenario = str_sub(id,-32,-29))%>%
  select(-c(id, X))%>%
  select(scenario, everything())%>%
  pivot_longer(2:ncol(.), names_to = "centroid_name", values_to = "pm10")%>%
  group_by(scenario, centroid_name)%>%
  summarise(pm10 = mean(pm10))%>%
  ungroup%>%
  mutate(pm25 = pm10*0.176)

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

scenarios <- scenarios %>%
  right_join(ct_centroids, by = "centroid_name")


write.csv(scenarios, file = "processed/scenario_pm_deltas.csv", row.names = FALSE)


# Make sure the same as Austin's calculations

my_test <- scenarios %>%
  group_by(scenario)%>%
  summarise(my_pm10_conc = mean(pm10))
  
test <- read.csv("data/dust/centroid_locations/2020CensusTract_TableToExcel.csv")%>%
  summarise_at(vars(gsl_1275_0_mASL_centroid:gsl_1282_0_mASL_centroid), ~mean(.))%>%
  pivot_longer(cols = gsl_1275_0_mASL_centroid:gsl_1282_0_mASL_centroid, names_to = "scenario", values_to = "pm10_conc")%>%
  mutate(scenario = str_sub(scenario, 5, 8))%>%
  full_join(my_test, by = c("scenario"))
