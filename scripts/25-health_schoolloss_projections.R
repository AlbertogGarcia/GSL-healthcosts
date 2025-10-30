# GSL dust costs: Mortality impacts
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
                # "sc1278" = "grey50", 
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

# cost of a school loss day
SLD_24 = 1673.504


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load and merge processed data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas_event.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence projections
ct_school_projections <- read.csv("processed/ct_school_projections.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_school_pollution <- ct_school_projections %>%
  right_join(scenario_pm_deltas, by = "FIPS")



#School Loss Days coefficients
RR_pm25 = 1.02
beta_pm25 <- log(RR_pm25)/10

beta_pm10 <- 2.5/100/10



ct_schoolloss_projections_temp <- ct_school_pollution %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         pm25_delta = ifelse(scenario == current_scenario, pm25_delta, relative_pm25_delta),
         SLD_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)*(n_storms_annual/n_storms_data),
         SLD_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_event*pop)*(n_storms_annual/n_storms_data),
         SLD = SLD_pm10 + SLD_pm25,
         pm_delta = pm10_delta + pm25_delta,
         endpoint = "School Loss Days"
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_schoolloss_projections_current <- ct_schoolloss_projections_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_SLD = SLD,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, Year, age_group, lower_age, upper_age, endpoint, current_SLD, current_pm_delta)

ct_schoolloss_projections <- ct_schoolloss_projections_temp %>%
  left_join(ct_schoolloss_projections_current, by = c("FIPS", "County", "event", "Year", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_SLD = ifelse(scenario == current_scenario, 0, SLD),
         SLD = relative_SLD + current_SLD,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         costs_SLD = SLD*SLD_24,
         PV_costs_SLD = costs_SLD/(1+0.03)^(Year - 2024)
         ) %>%
  select(FIPS, County, scenario, event, Year, age_group, lower_age, upper_age, pop, pm_delta, endpoint, SLD, costs_SLD, PV_costs_SLD)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Relevant aggregations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_schoolloss_projections <- ct_schoolloss_projections %>%
  group_by(scenario, Year) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            costs_SLD = sum(costs_SLD, na.rm = T)/1000000,
            PV_costs_SLD = sum(PV_costs_SLD, na.rm = T)/1000000
  )%>%
  ungroup %>%
  group_by(scenario) %>%
  mutate(PV_cum_costs_SLD = cumsum(PV_costs_SLD),
         cum_costs_SLD = cumsum(costs_SLD),
         cum_SLD = cumsum(SLD))%>%
  ungroup

write.csv(total_schoolloss_projections, file = "processed/total_schoolloss_projections.csv", row.names = FALSE)

