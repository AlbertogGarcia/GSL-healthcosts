# GSL dust costs: School absence impacts
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

#2 Population and incidence projections
ct_school_projections <- read.csv("processed/ct_school_projections.csv", stringsAsFactors =  FALSE)

#3 Projected cost of school loss day
morbidity_valuations_projected <- read.csv("processed/morbidity_valuations_projected.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_school_pollution <- ct_school_projections %>%
  right_join(scenario_pm_deltas, by = "FIPS", relationship = "many-to-many") %>%
  left_join(morbidity_valuations_projected %>%
              filter(Endpoint == "School Loss Days")
            , by = "Year") %>%
  filter(Year >= 2025)



schoolloss_parameters <- read_excel("data/health/morbidity_parameters.xlsx") %>%
  select(-reference) %>%
  filter(endpoint == "School Loss Days",
         pollutant == "pm10") %>%
  mutate(CI_lower = ifelse(is.na(CI_lower), parameter_value - 1.96*se, CI_lower),
         CI_upper = ifelse(is.na(CI_upper), parameter_value + 1.96*se, CI_upper))%>%
  mutate(beta = case_when(
    parameter == "beta" ~ parameter_value,
    parameter == "RR" ~ log(parameter_value)/dose),
    beta_lower = case_when(
      parameter == "beta" ~ CI_lower,
      parameter == "RR" ~ log(CI_lower)/dose),
    beta_upper = case_when(
      parameter == "beta" ~ CI_upper,
      parameter == "RR" ~ log(CI_upper)/dose))

beta_pm10 <- schoolloss_parameters %>% select(beta) %>% pull()
beta_pm10_lower <- schoolloss_parameters %>% select(beta_lower) %>% pull()
beta_pm10_upper <- schoolloss_parameters %>% select(beta_upper) %>% pull()

ct_schoolloss_projections_temp <- ct_school_pollution %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         SLD_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD_pm10_lower = ((1-(1/exp(beta_pm10_lower*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD_pm10_upper = ((1-(1/exp(beta_pm10_upper*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD = SLD_pm10,
         SLD_lower = SLD_pm10_lower,
         SLD_upper = SLD_pm10_upper,
         pm_delta = pm10_delta
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_schoolloss_projections_current <- ct_schoolloss_projections_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_SLD = SLD,
         current_SLD_lower = SLD_lower,
         current_SLD_upper = SLD_upper,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, Year, age_group, lower_age, upper_age, Endpoint, current_SLD, current_SLD_lower, current_SLD_upper, current_pm_delta)

ct_schoolloss_projections <- ct_schoolloss_projections_temp %>%
  left_join(ct_schoolloss_projections_current, by = c("FIPS", "County", "event", "Year", "age_group", "lower_age", "upper_age", "Endpoint"), relationship = "many-to-many")%>%
  mutate(relative_SLD = ifelse(scenario == current_scenario, 0, SLD),
         relative_SLD_lower = ifelse(scenario == current_scenario, 0, SLD_lower),
         relative_SLD_upper = ifelse(scenario == current_scenario, 0, SLD_upper),
         SLD = pmax(relative_SLD + current_SLD, 0),
         SLD_lower = pmax(relative_SLD_lower + current_SLD_lower, 0),
         SLD_upper = pmax(relative_SLD_upper + current_SLD_upper, 0),
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         PV_costs_SLD = COI_proj*SLD/(1+0.03)^(Year - 2024),
         PV_costs_lower = COI_proj*SLD_lower/(1+0.03)^(Year - 2024),
         PV_costs_upper = COI_proj*SLD_upper/(1+0.03)^(Year - 2024)
         ) %>%
  select(FIPS, County, scenario, event, Year, age_group, lower_age, upper_age, pop, pm_delta, Endpoint, 
         SLD, SLD_lower, SLD_upper, PV_costs_SLD, PV_costs_lower, PV_costs_upper)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Relevant aggregations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_schoolloss_projections <- ct_schoolloss_projections %>%
  group_by(scenario, Year) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            SLD_lower = sum(SLD_lower, na.rm = T),
            SLD_upper = sum(SLD_upper, na.rm = T),
            PV_costs_SLD = sum(PV_costs_SLD, na.rm = T)/1000000,
            PV_costs_lower = sum(PV_costs_lower, na.rm = T)/1000000,
            PV_costs_upper = sum(PV_costs_upper, na.rm = T)/1000000
  )%>%
  ungroup %>%
  group_by(scenario) %>%
  mutate(cum_SLD = cumsum(SLD),
         cum_SLD_lower = cumsum(SLD_lower),
         cum_SLD_upper = cumsum(SLD_upper),
         PV_cum_costs_SLD = cumsum(PV_costs_SLD),
         PV_cum_costs_lower = cumsum(PV_costs_lower),
         PV_cum_costs_upper = cumsum(PV_costs_upper)) %>%
  ungroup

write.csv(total_schoolloss_projections, file = "processed/total_schoolloss_projections.csv", row.names = FALSE)

