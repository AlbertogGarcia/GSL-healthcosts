# GSL dust costs: Mortality impacts
# albert.garcia@utah.edu
# created: 05/28/2025
# updated: 

# Set up environment ########################################

# packages=c("xlsx", "gdata", "dplyr","tidyr", "stringr", "fuzzyjoin", "stringr", 
#            "ggplot2", "stargazer", "plm", "cowplot", "sf", "lwgeom","data.table")

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### VSL and VSLY
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
VSL_24 = 12.57222

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### SET BASELINE LAKE SCENARIO
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
baseline_scenario = 1280

relevant_scenarios <- c(1275, 1277, 1278, 1280, 1281)

# Load and merge processed data #####################################

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_projections <- read.csv("processed/ct_incidence_projections.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_projections <- ct_incidence_projections %>%
  left_join(scenario_pm_deltas, by = "FIPS", relationship = "many-to-many")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Mortality impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 mcrogram increase
RR_pm25 = 1.0065
beta_pm25 <- log(RR_pm25)

RR_pm10 = 1.0041
beta_pm10 <- log(RR_pm10)

#Mortality impact
ct_mortality_projections <- ct_projections %>%
  mutate(exposure_pm10 = pm10/10,
         exposure_pm25 = pm25/10,
         mortality_pm10 = (1-(1/exp(beta_pm10*exposure_pm10)))*incidence_rate*pop,
         mortality_pm25 = (1-(1/exp(beta_pm25*exposure_pm25)))*incidence_rate*pop,
         mortality_pm = mortality_pm10 + mortality_pm25,
         life_yrs_remaining = 77.2 - (lower_age + upper_age)/2,
         FV_costs_VSL = mortality_pm*VSL_24,
         PV_costs_VSL = FV_costs_VSL/(1+0.03)^(Year - 2024)
         )%>%
  drop_na(scenario)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_mortality_projections <- ct_mortality_projections %>%
  group_by(scenario, Year) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            costs_VSL = sum(PV_costs_VSL, na.rm = T))%>%
  ungroup

total_mortality_projections_relative <- total_mortality_projections %>%
  left_join(total_mortality_projections %>%
          filter(scenario == baseline_scenario)%>%
          rename(baseline_mortality_pm10 = mortality_pm10,
                 baseline_mortality_pm25 = mortality_pm25,
                 baseline_mortality_pm = mortality_pm,
                 baseline_costs_VSL = costs_VSL)%>%
          dplyr::select(-scenario)
          , by = "Year"
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm,
         relative_costs_VSL = costs_VSL - baseline_costs_VSL)%>%
  group_by(scenario)%>%
  mutate(PV_cum_relative_costs = cumsum(relative_costs_VSL),
         cum_relative_mortality = cumsum(relative_mortality))%>%
  filter(Year <= 2050)%>%
  select(scenario, Year, relative_mortality, cum_relative_mortality, relative_costs_VSL, PV_cum_relative_costs)

ggplot(total_mortality_projections_relative, aes(x = Year, y = relative_mortality, color = as.character(scenario)))+
  geom_line()+
  geom_point(size = 0.75)

ggplot(total_mortality_projections_relative, aes(x = Year, y = cum_relative_mortality, color = as.character(scenario)))+
  geom_line()+
  geom_point(size = 0.75)+
  #geom_vline(xintercept = 2033, linetype = "dashed", linewidth = 0.2)+
  ylab("Cumulative premature mortalities") +
  ggtitle("Mortality through 2050 (relative to 1280 mASL)")+
  scale_color_discrete(name = "GSL water level (mASL)")+
  theme_classic()+
  theme(legend.position = "bottom")

ggplot(total_mortality_projections_relative, aes(x = Year, y = PV_cum_relative_costs, color = as.character(scenario)))+
  geom_line()+
  geom_point(size = 0.75)+
  #geom_vline(xintercept = 2033, linetype = "dashed", linewidth = 0.2)+
  ylab("Present value of cumulative costs (millions USD)") +
  ggtitle("Costs through 2050 (relative to 1280 mASL)")+
  scale_color_discrete(name = "GSL water level (mASL)")+
  theme_classic()+
  theme(legend.position = "bottom")
