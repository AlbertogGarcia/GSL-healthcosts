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
               cowplot,
               ggpubr,
               zoo
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

#2 Population and incidence
ct_incidence_projections <- read.csv("processed/ct_incidence_projections_adj.csv", stringsAsFactors =  FALSE) %>%
  select(-Factor)

#3 Projected VSL
VSL_projected <- read.csv("processed/VSL_projected.csv", stringsAsFactors =  FALSE)
age_VSL_projected <- read.csv("processed/age_VSL_projected.csv", stringsAsFactors =  FALSE)

age_VSL_projected_grouped <- ct_incidence_projections %>%
  select(lower_age, upper_age, age_group) %>%
  distinct()%>%
  tidyr::crossing(age_VSL_projected)%>%
  filter(age >= lower_age & age <= upper_age)%>%
  group_by(lower_age, upper_age, age_group, Year) %>%
  summarise(age_vsl_proj = mean(age_vsl_proj))



#Merge w/ pollution deltas 
ct_projections <- ct_incidence_projections %>%
  left_join(scenario_pm_deltas, by = "FIPS", relationship = "many-to-many")%>%
  left_join(VSL_projected, by = "Year") %>%
  left_join(age_VSL_projected_grouped, by = c("Year", "lower_age", "upper_age", "age_group")) %>%
  filter(Year >= 2025)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Mortality impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 mcrogram increase - hence the divide by 10
# 1.0065 is PM2.5 RR from a short-term exposure
beta_pm25 <- log(1.0065)/10

beta_pm10 <- log(1.0041)/10
beta_pm10_lower <- log(1.0034)/10
beta_pm10_upper <- log(1.0049)/10

#Mortality impact
ct_mortality_projections_temp <- ct_projections %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         mortality_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         mortality = mortality_pm10,
         mortality_lower = ((1-(1/exp(beta_pm10_lower*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         mortality_upper = ((1-(1/exp(beta_pm10_upper*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         pm_delta = pm10_delta)%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_mortality_projections_current <- ct_mortality_projections_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_mortality = mortality,
         current_mortality_lower = mortality_lower,
         current_mortality_upper = mortality_upper,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, Year, age_group, lower_age, upper_age, endpoint, current_mortality, current_mortality_lower, current_mortality_upper, current_pm_delta)

ct_mortality_projections <- ct_mortality_projections_temp %>%
  left_join(ct_mortality_projections_current, by = c("FIPS", "County", "event", "Year", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_mortality = ifelse(scenario == current_scenario, 0, mortality),
         relative_mortality_lower = ifelse(scenario == current_scenario, 0, mortality_lower),
         relative_mortality_upper = ifelse(scenario == current_scenario, 0, mortality_upper),
         mortality = relative_mortality + current_mortality,
         mortality_lower = relative_mortality_lower + current_mortality_lower,
         mortality_upper = relative_mortality_upper + current_mortality_upper,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         PV_costs_VSL = (mortality*VSL_proj)/(1+0.03)^(Year - 2024),
         PV_costs_lower = (mortality_lower*VSL_proj)/(1+0.03)^(Year - 2024),
         PV_costs_upper = (mortality_upper*VSL_proj)/(1+0.03)^(Year - 2024),
         PV_costs_age_VSL = (mortality*age_vsl_proj)/(1+0.03)^(Year - 2024),
         PV_costs_age_lower = (mortality_lower*age_vsl_proj)/(1+0.03)^(Year - 2024),
         PV_costs_age_upper = (mortality_upper*age_vsl_proj)/(1+0.03)^(Year - 2024)
         ) %>%
  select(FIPS, County, scenario, event, Year, age_group, lower_age, upper_age, incidence_rate, pop, pm_delta, endpoint, 
         mortality, mortality_lower, mortality_upper, PV_costs_VSL, PV_costs_lower, PV_costs_upper, PV_costs_age_VSL, PV_costs_age_lower, PV_costs_age_upper)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_mortality_projections <- ct_mortality_projections %>%
  group_by(scenario, Year) %>%
  summarise(mortality = sum(mortality, na.rm = T),
            mortality_lower = sum(mortality_lower, na.rm = T),
            mortality_upper = sum(mortality_upper, na.rm = T),
            PV_costs_VSL = sum(PV_costs_VSL, na.rm = T),
            PV_costs_lower = sum(PV_costs_lower, na.rm = T),
            PV_costs_upper = sum(PV_costs_upper, na.rm = T),
            PV_costs_age_VSL = sum(PV_costs_age_VSL, na.rm = T),
            PV_costs_age_lower = sum(PV_costs_age_lower, na.rm = T),
            PV_costs_age_upper = sum(PV_costs_age_upper, na.rm = T))%>%
  ungroup %>%
  group_by(scenario) %>%
  mutate(cum_mortality = cumsum(mortality),
         cum_mortality_lower = cumsum(mortality_lower),
         cum_mortality_upper = cumsum(mortality_upper),
         PV_cum_costs_VSL = cumsum(PV_costs_VSL),
         PV_cum_costs_lower = cumsum(PV_costs_lower),
         PV_cum_costs_upper = cumsum(PV_costs_upper),
         PV_cum_costs_age_VSL = cumsum(PV_costs_age_VSL),
         PV_cum_costs_age_lower = cumsum(PV_costs_age_lower),
         PV_cum_costs_age_upper = cumsum(PV_costs_age_upper)
         )%>%
  ungroup
  
write.csv(total_mortality_projections, file = "processed/total_mortality_projections.csv", row.names = FALSE)



mortality_proj_annual <- total_mortality_projections %>%
  ggplot(aes(x = Year, y = mortality, color = as.character(scenario)))+
  geom_line(linewidth = 1)+
  geom_point(size = 2)+
  scale_y_continuous(name = "Annual mortality") +
  ggtitle("Annual dust-induced mortality (2025-2060)")+
  scale_color_manual(name = "GSL water level (ftASL)", values = scenario_pal)+
  scale_fill_manual(values = scenario_pal, guide="none")+
  theme_cowplot(14)+
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray80", size = 0.25),
        plot.title = element_text(hjust = 0.5, size=16)
  )
mortality_proj_annual
