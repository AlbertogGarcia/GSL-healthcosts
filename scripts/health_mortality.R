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
               readxl
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

# SET BASELINE LAKE SCENARIO #############
baseline_scenario = 1280

relevant_scenarios <- c(1275, 1277, 1279, 1280, 1281)

# Load and merge processed data #####################################

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_mortality <- read.csv("processed/ct_incidence_mortality.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_mortality_pollution <- ct_incidence_mortality %>%
  left_join(scenario_pm_deltas, by = "FIPS")

# Mortality impact ######################################

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 mcrogram increase
RR_pm25 = 1.0065
beta_pm25 <- log(RR_pm25)

RR_pm10 = 1.0041
beta_pm10 <- log(RR_pm10)

#Mortality impact
ct_mortality_age <- ct_mortality_pollution %>%
  #filter(lower_age>29)%>%
  mutate(exposure_pm10 = pm10/10,
         exposure_pm25 = pm25/10,
         mortality_pm10 = (1-(1/exp(beta_pm10*exposure_pm10)))*incidence_rate*pop,
         mortality_pm25 = (1-(1/exp(beta_pm25*exposure_pm25)))*incidence_rate*pop,
         mortality_pm = mortality_pm10 + mortality_pm25)%>%
  drop_na(scenario)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Create data for analyses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


VSL_24 = 13.24189

# total mortality by census tract
ct_mortality <- ct_mortality_age %>%
  group_by(scenario, FIPS) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T))%>%
  ungroup

ct_mortality_relative <- ct_mortality %>%
  left_join(ct_mortality %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_pm10 = pm10,
                     baseline_pm25 = pm25,
                     baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm)%>%
              dplyr::select(-scenario)
            , by = c("FIPS")
            )%>%
  mutate(relative_pm10 = pm10 - baseline_pm10,
         relative_pm25 = pm25 - baseline_pm25,
         relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm)%>%
  select(scenario, FIPS, relative_pm10, relative_pm25, relative_mortality_pm10, relative_mortality_pm25, relative_mortality)

# total overall mortality
total_mortality <- ct_mortality_age %>%
  group_by(scenario) %>%
  summarise(pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T))%>%
  ungroup

total_mortality_relative <- total_mortality %>%
  cbind(total_mortality %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_pm10 = pm10,
                     baseline_pm25 = pm25,
                     baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm)%>%
              dplyr::select(-scenario)
  )%>%
  mutate(relative_pm10 = pm10 - baseline_pm10,
         relative_pm25 = pm25 - baseline_pm25,
         relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm)%>%
  select(scenario, pm10, pm25, relative_pm10, relative_pm25, relative_mortality_pm10, relative_mortality_pm25, relative_mortality)%>%
  mutate(relative_mortality_millionUSD = relative_mortality*VSL_24)

ggplot(total_mortality_relative
       , aes(x=scenario, y=relative_pm10))+
  geom_bar(stat='identity')+
  theme_cowplot()

ggplot(total_mortality_relative
       , aes(x=scenario, y=relative_mortality))+
  geom_bar(stat='identity')+
  theme_cowplot()

ggplot(total_mortality_relative
       , aes(x=scenario, y=relative_mortality_millionUSD))+
  geom_bar(stat='identity')+
  theme_cowplot()

# mortality by age group
mortality_byage <- ct_mortality_age %>%
  group_by(scenario, age_group, lower_age, upper_age) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T))%>%
  ungroup

mortality_byage_relative <- mortality_byage %>%
  left_join(mortality_byage %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm)%>%
              dplyr::select(-scenario)
            , by = c("age_group", "lower_age", "upper_age")
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm)%>%
  select(scenario, age_group, lower_age, upper_age, relative_mortality_pm10, relative_mortality_pm25, relative_mortality)

ggplot(mortality_byage_relative %>% filter(scenario == 1277)
       , aes(x=reorder(age_group, lower_age), y=relative_mortality))+
  geom_bar(stat='identity')+
  theme_cowplot()

