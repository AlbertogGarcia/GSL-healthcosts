# GSL dust costs: Morbidity impacts - projected
# albert.garcia@utah.edu
# created: 01/5/2026
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

all_scenarios <- seq(4182, 4202, by = 1) 
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

#2 Projected valuations
morbidity_valuations_projected <- read.csv("processed/morbidity_valuations_projected.csv", stringsAsFactors =  FALSE)
total_morbidity_projections <- data.frame()
for(e in unique(morbidity_valuations_projected$Endpoint)){
  
  # Population and incidence projections
  ct_morbidity_pollution <- read.csv("processed/ct_incidence_morbidity_projections.csv", stringsAsFactors =  FALSE) %>%
    filter(endpoint == e) %>%
    #Merge w/ pollution deltas
    inner_join(scenario_pm_deltas, by = "FIPS") %>%
    inner_join(morbidity_valuations_projected %>% filter(Endpoint == e)
              , by = c("Year", "endpoint" = "Endpoint")) %>%
    filter(Year >= 2025) 
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #### estimate morbidity impacts
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # test <- ct_morbidity_pollution %>% head(500)
  
  ct_morbidity_projections_temp <- ct_morbidity_pollution %>%
    mutate(incidence_rate_event = value*event_days, # incidence rates are already daily for morbidity
           pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
           pm25_delta = ifelse(scenario == current_scenario, pm25_delta, relative_pm25_delta),
           morbidity_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
           morbidity_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_event*pop)/n_years_storms,
           morbidity = morbidity_pm10 + morbidity_pm25,
           pm_delta = pm10_delta + pm25_delta
    )%>%
    drop_na(scenario)
  
  #### Get overall impacts, not just relative
  ct_morbidity_projections_current <- ct_morbidity_projections_temp %>%
    filter(scenario == current_scenario) %>%
    rename(current_morbidity = morbidity,
           current_pm_delta = pm_delta) %>%
    select(FIPS, County, event, Year, age_group, lower_age, upper_age, endpoint, current_morbidity, current_pm_delta)
  
  ct_morbidity_projections <- ct_morbidity_projections_temp %>%
    inner_join(ct_morbidity_projections_current, by = c("FIPS", "County", "event", "Year", "age_group", "lower_age", "upper_age", "endpoint"))%>%
    mutate(relative_morbidity = ifelse(scenario == current_scenario, 0, morbidity),
           morbidity = relative_morbidity + current_morbidity,
           relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
           pm_delta = relative_pm_delta + current_pm_delta,
           PV_costs_COI = (morbidity*COI_proj)/(1+0.03)^(Year - 2024)
    ) %>%
    select(FIPS, County, scenario, event, Year, age_group, lower_age, upper_age, pop, pm_delta, endpoint, morbidity, PV_costs_COI)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ## Aggregation
  
  total_morbidity_projections <- ct_morbidity_projections %>%
    group_by(scenario, Year, endpoint) %>%
    summarise(morbidity = sum(morbidity, na.rm = T),
              PV_costs_COI = sum(PV_costs_COI, na.rm = T)/1000000
    )%>%
    ungroup %>%
    group_by(scenario) %>%
    mutate(PV_cum_costs_COI = cumsum(PV_costs_COI),
           cum_morbidity = cumsum(morbidity))%>%
    ungroup %>%
    rbind(total_morbidity_projections)
  
  
  rm(ct_morbidity_projections, ct_morbidity_projections_current, ct_morbidity_projections_temp, ct_morbidity_pollution)
  
  }

write.csv(total_morbidity_projections, file = "processed/total_morbidity_projections.csv", row.names = FALSE)

