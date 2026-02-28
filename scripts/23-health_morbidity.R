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
                "bad" = "#d7191c",
                "current" = "#fdae61",
                "target" = "#abd9e9",
                "avg" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_scenarios <- seq(4182, 4203, by = 1) 
current_scenario = 4192
relevant_scenarios <- c(4183, current_scenario, 4198, 4200) 

scenario_pal <- c(palette$bad, palette$current, palette$target, palette$avg)

n_years_storms = 6

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


morbidity_valuations_2024 <- read.csv("data/health/morbidity_valuations_2024.csv", stringsAsFactors = F)%>%
  select(-c(COI_2015, Qualifier))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_morbidity_age_temp <- ct_morbidity_pollution %>%
  mutate(pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         morbidity_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         morbidity_pm10_lower = ((1-(1/exp(beta_pm10_lower*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         morbidity_pm10_upper = ((1-(1/exp(beta_pm10_upper*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         morbidity = morbidity_pm10,
         morbidity_lower = morbidity_pm10_lower,
         morbidity_upper = morbidity_pm10_upper,
         pm_delta = pm10_delta
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_morbidity_age_current <- ct_morbidity_age_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_morbidity = morbidity,
         current_morbidity_lower = morbidity_lower,
         current_morbidity_upper = morbidity_upper,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, age_group, endpoint, current_morbidity, current_morbidity_lower, current_morbidity_upper, current_pm_delta)

ct_morbidity_age <- ct_morbidity_age_temp %>%
  left_join(ct_morbidity_age_current, by = c("FIPS", "County", "event", "age_group", "endpoint"))%>%
  mutate(relative_morbidity = ifelse(scenario == current_scenario, 0, morbidity),
         relative_morbidity_lower = ifelse(scenario == current_scenario, 0, morbidity_lower),
         relative_morbidity_upper = ifelse(scenario == current_scenario, 0, morbidity_upper),
         morbidity = ifelse(relative_morbidity + current_morbidity >= 0, relative_morbidity + current_morbidity, 0),
         morbidity_lower = ifelse(relative_morbidity_lower + current_morbidity_lower >= 0, relative_morbidity_lower + current_morbidity_lower, 0),
         morbidity_upper = ifelse(relative_morbidity_upper + current_morbidity_upper >= 0, relative_morbidity_upper + current_morbidity_upper, 0),
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta
         ) %>%
  select(FIPS, County, scenario, event, age_group, pop, pm_delta, endpoint, morbidity, morbidity_lower, morbidity_upper)

ct_morbidity <- ct_morbidity_age %>%
  group_by(FIPS, County, scenario, endpoint) %>%
  summarise(morbidity = sum(morbidity, na.rm = T))%>%
  ungroup %>%
  left_join(morbidity_valuations_2024, by = c("endpoint" = "Endpoint")) %>%
  mutate(costs = morbidity*COI_24)

write.csv(ct_morbidity, file = "processed/ct_morbidity.csv", row.names = FALSE)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Totals morbidity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_morbidity <- ct_morbidity_age %>%
  group_by(scenario, endpoint) %>%
  summarise(morbidity = sum(morbidity, na.rm = T),
            morbidity_lower = sum(morbidity_lower, na.rm = T),
            morbidity_upper = sum(morbidity_upper, na.rm = T))%>%
  ungroup %>%
  left_join(morbidity_valuations_2024, by = c("endpoint" = "Endpoint")) %>%
  mutate(costs = morbidity*COI_24,
         costs_lower = morbidity_lower*COI_24,
         costs_upper = morbidity_upper*COI_24)

write.csv(total_morbidity, file = "processed/total_morbidity.csv", row.names = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# By race
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#2 Population and incidence
ct_incidence_morbidity_race <- read.csv("processed/ct_incidence_morbidity_race.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_morbidity_pollution_race <- ct_incidence_morbidity_race %>%
  left_join(scenario_pm_deltas, by = "FIPS", 
            relationship = "many-to-many")%>%
  mutate(incidence_rate_event = value*event_days) # incidence rates are already daily for morbidity

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts

ct_morbidity_agebyrace_temp <- ct_morbidity_pollution_race %>%
  mutate(pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         morbidity_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         morbidity_pm10_lower = ((1-(1/exp(beta_pm10_lower*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         morbidity_pm10_upper = ((1-(1/exp(beta_pm10_upper*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         morbidity = morbidity_pm10,
         morbidity_lower = morbidity_pm10_lower,
         morbidity_upper = morbidity_pm10_upper,
         pm_delta = pm10_delta
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_morbidity_agebyrace_current <- ct_morbidity_agebyrace_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_morbidity = morbidity,
         current_morbidity_lower = morbidity_lower,
         current_morbidity_upper = morbidity_upper,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, race, age_group, endpoint, current_morbidity, current_morbidity_lower, current_morbidity_upper, current_pm_delta)

ct_morbidity_agebyrace <- ct_morbidity_agebyrace_temp %>%
  left_join(ct_morbidity_agebyrace_current, by = c("FIPS", "County", "event", "race", "age_group", "endpoint"))%>%
  mutate(relative_morbidity = ifelse(scenario == current_scenario, 0, morbidity),
         relative_morbidity_lower = ifelse(scenario == current_scenario, 0, morbidity_lower),
         relative_morbidity_upper = ifelse(scenario == current_scenario, 0, morbidity_upper),
         morbidity = ifelse(relative_morbidity + current_morbidity >= 0, relative_morbidity + current_morbidity, 0),
         morbidity_lower = ifelse(relative_morbidity_lower + current_morbidity_lower >= 0, relative_morbidity_lower + current_morbidity_lower, 0),
         morbidity_upper = ifelse(relative_morbidity_upper + current_morbidity_upper >= 0, relative_morbidity_upper + current_morbidity_upper, 0),
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta
  ) %>%
  select(FIPS, County, scenario, event, race, age_group, pop, pm_delta, endpoint, morbidity, morbidity_lower, morbidity_upper)

ct_morbidity_race <- ct_morbidity_agebyrace %>%
  group_by(FIPS, County, scenario, race, endpoint) %>%
  summarise(morbidity = sum(morbidity, na.rm = T))%>%
  ungroup %>%
  left_join(morbidity_valuations_2024, by = c("endpoint" = "Endpoint")) %>%
  mutate(costs = morbidity*COI_24)

write.csv(ct_morbidity_race, file = "processed/ct_morbidity_race.csv", row.names = FALSE)


total_morbidity_race <- ct_morbidity_agebyrace %>%
  drop_na(race) %>%
  filter(race %ni% c("Other Race", "Multiple Race", "American Indian"))%>%
  group_by(scenario, endpoint, race) %>%
  summarise(morbidity = sum(morbidity, na.rm = T),
            morbidity_lower = sum(morbidity_lower, na.rm = T),
            morbidity_upper = sum(morbidity_upper, na.rm = T))%>%
  ungroup %>%
  left_join(morbidity_valuations_2024, by = c("endpoint" = "Endpoint")) %>%
  mutate(costs = morbidity*COI_24,
         costs_lower = morbidity_lower*COI_24,
         costs_upper = morbidity_upper*COI_24)
write.csv(total_morbidity_race, file = "processed/total_morbidity_race.csv", row.names = FALSE)

ct_morbidity_map <- ct_morbidity_agebyrace %>%
  group_by(FIPS, County, scenario, endpoint) %>%
  summarise(morbidity = sum(morbidity, na.rm = T))%>%
  ungroup %>%
  left_join(morbidity_valuations_2024, by = c("endpoint" = "Endpoint")) %>%
  mutate(costs = morbidity*COI_24)

write.csv(ct_morbidity_map, file = "processed/ct_morbidity_map.csv", row.names = FALSE)
