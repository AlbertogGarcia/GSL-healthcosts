# GSL dust costs: asthma impacts
# albert.garcia@utah.edu
# created: 05/28/2025
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
               ggplot2,
               readxl,
               cowplot
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

# Color palette
palette <- list("white" = "#FAFAFA",
                "dark" = "#0c2230",
                "red" = "#d7191c",
                "blue" = "#2c7bb6",
                "orange" = "#fc8d62",
                "green" = "#66c2a5",
                "purple" = "#8da0cb",
                "sc1275" = "#d7191c",
                "sc1278" = "#fdae61",
                #"sc1278" = 
                #  "grey50", 
                #"#ffd93f", 
                "sc1280" = "#abd9e9",
                "sc1281" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_scenarios <- seq(1275, 1281, by = 1) #just excludes baseline of 1282
current_scenario = 1278
relevant_scenarios <- c(1275, 1278, 1280, 1281) 

scenario_pal <- c(palette$sc1275, palette$sc1278, palette$sc1280, palette$sc1281)

n_storms_data = 2
n_storms_annual = 3


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load and merge processed data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas_event.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_morbidity <- read.csv("processed/ct_incidence_morbidity.csv", stringsAsFactors =  FALSE)


#Merge w/ pollution deltas 
ct_morbidity_pollution <- ct_incidence_morbidity %>%
  left_join(scenario_pm_deltas, by = "FIPS")%>%
  mutate(incidence_rate_event = value*event_days) # incidence rates are already daily for morbidity


morbidity_valuations_2024 <- read.csv("data/health/morbidity_valuations_2024.csv", stringsAsFactors = F)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_morbidity_age_temp <- ct_morbidity_pollution %>%
  mutate(pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         pm25_delta = ifelse(scenario == current_scenario, pm25_delta, relative_pm25_delta),
         morbidity_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)*(n_storms_annual/n_storms_data),
         morbidity_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_event*pop)*(n_storms_annual/n_storms_data),
         morbidity = morbidity_pm10 + morbidity_pm25,
         pm_delta = pm10_delta + pm25_delta
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_morbidity_age_current <- ct_morbidity_age_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_morbidity = morbidity,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, age_group, endpoint, current_morbidity, current_pm_delta)

ct_morbidity_age <- ct_morbidity_age_temp %>%
  left_join(ct_morbidity_age_current, by = c("FIPS", "County", "event", "age_group", "endpoint"))%>%
  mutate(relative_morbidity = ifelse(scenario == current_scenario, 0, morbidity),
         morbidity = relative_morbidity + current_morbidity,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta
         ) %>%
  select(FIPS, County, scenario, event, age_group, pop, pm_delta, endpoint, morbidity)

# ct_morbidity <- ct_morbidity_age %>%
#   group_by(scenario, FIPS, County, endpoint) %>%
#   summarise(morbidity = sum(morbidity, na.rm = T))%>%
#   ungroup

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Totals morbidity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_morbidity <- ct_morbidity_age %>%
  group_by(scenario, endpoint) %>%
  summarise(morbidity = sum(morbidity, na.rm = T))%>%
  ungroup %>%
  left_join(morbidity_valuations_2024, by = c("endpoint" = "Endpoint"))


