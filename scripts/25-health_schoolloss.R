# GSL dust costs: School loss days impacts
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

# cost of a school loss day
SLD_24 = 1673.504


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load and merge processed data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas_event.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_school <- read.csv("processed/ct_school.csv", stringsAsFactors =  FALSE)
ct_school_race <- read.csv("processed/ct_school_race.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_school_pollution <- ct_school %>%
  right_join(scenario_pm_deltas, by = "FIPS")

ct_school_pollution_race <- ct_school_race %>%
  right_join(scenario_pm_deltas, by = "FIPS")



#School Loss Days coefficients
RR_pm25 = 1.02
beta_pm25 <- log(RR_pm25)

RR_pm10 = 1.0228
beta_pm10 <- log(RR_pm10)/10

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
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_schoolloss_temp <- ct_school_pollution %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         SLD_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD_pm10_lower = ((1-(1/exp(beta_pm10_lower*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD_pm10_upper = ((1-(1/exp(beta_pm10_upper*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD = SLD_pm10,
         SLD_lower = SLD_pm10_lower,
         SLD_upper = SLD_pm10_upper,
         pm_delta = pm10_delta,
         endpoint = "School Loss Days"
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_schoolloss_current <- ct_schoolloss_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_SLD = SLD,
         current_SLD_lower = SLD_lower,
         current_SLD_upper = SLD_upper,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, age_group, lower_age, upper_age, endpoint, current_SLD, current_SLD_lower, current_SLD_upper, current_pm_delta)

ct_schoolloss <- ct_schoolloss_temp %>%
  left_join(ct_schoolloss_current, by = c("FIPS", "County", "event", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_SLD = ifelse(scenario == current_scenario, 0, SLD),
         relative_SLD_lower = ifelse(scenario == current_scenario, 0, SLD_lower),
         relative_SLD_upper = ifelse(scenario == current_scenario, 0, SLD_upper),
         SLD = pmax(relative_SLD + current_SLD, 0),
         SLD_lower = pmax(relative_SLD_lower + current_SLD_lower, 0),
         SLD_upper = pmax(relative_SLD_upper + current_SLD_upper, 0),
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta) %>%
  select(FIPS, County, scenario, event, age_group, lower_age, upper_age, pop, pm_delta, endpoint, SLD, SLD_lower, SLD_upper)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts disaggregated by race
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ct_schoolloss_race_temp <- ct_school_pollution_race %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         SLD_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD_pm10_lower = ((1-(1/exp(beta_pm10_lower*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD_pm10_upper = ((1-(1/exp(beta_pm10_upper*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         SLD = SLD_pm10,
         SLD_lower = SLD_pm10_lower,
         SLD_upper = SLD_pm10_upper,
         pm_delta = pm10_delta,
         endpoint = "School Loss Days"
  )%>%
  drop_na(scenario)

#### Get overall impacts, not just relative
ct_schoolloss_race_current <- ct_schoolloss_race_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_SLD = SLD,
         current_SLD_lower = SLD_lower,
         current_SLD_upper = SLD_upper,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, Race, age_group, lower_age, upper_age, endpoint, current_SLD, current_SLD_lower, current_SLD_upper, current_pm_delta)

ct_schoolloss_race <- ct_schoolloss_race_temp %>%
  left_join(ct_schoolloss_race_current, by = c("FIPS", "County", "event", "Race", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_SLD = ifelse(scenario == current_scenario, 0, SLD),
         relative_SLD_lower = ifelse(scenario == current_scenario, 0, SLD_lower),
         relative_SLD_upper = ifelse(scenario == current_scenario, 0, SLD_upper),
         SLD = pmax(relative_SLD + current_SLD, 0),
         SLD_lower = pmax(relative_SLD_lower + current_SLD_lower, 0),
         SLD_upper = pmax(relative_SLD_upper + current_SLD_upper, 0),
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta) %>%
  select(FIPS, County, scenario, event, Race, age_group, lower_age, upper_age, pop, pm_delta, endpoint, SLD, SLD_lower, SLD_upper)

ct_schoolloss_map <- ct_schoolloss_race %>%
  group_by(scenario, FIPS, County, endpoint) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            costs_SLD = sum(SLD*SLD_24, na.rm = T)
            )%>%
  ungroup
write.csv(ct_schoolloss_map, file = "processed/ct_schoolloss_map.csv", row.names = FALSE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Aggregating to relevant scales
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_schoolloss <- ct_schoolloss %>%
  group_by(scenario, endpoint) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            SLD_lower = sum(SLD_lower, na.rm = T),
            SLD_upper = sum(SLD_upper, na.rm = T),
            costs_SLD = sum(SLD*SLD_24, na.rm = T),
            costs_SLD_lower = sum(SLD_lower*SLD_24, na.rm = T),
            costs_SLD_upper = sum(SLD_upper*SLD_24, na.rm = T),
            pop = sum(pop, na.rm = T)
  )%>%
  ungroup %>%
  mutate(delta_rate = SLD/pop)

write.csv(total_schoolloss, file = "processed/total_schoolloss.csv", row.names = FALSE)




total_schoolloss_race <- ct_schoolloss_race %>%
  drop_na(Race) %>%
  filter(Race %ni% c("Other Race", "Multiple Race", "American Indian"))%>%
  group_by(scenario, endpoint, Race) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            costs_SLD = sum(SLD*SLD_24, na.rm = T),
            student_pop = sum(pop, na.rm = T)/length(unique(event))
  )%>%
  ungroup %>%
  mutate(SLD_per_100k = SLD/student_pop*100000)

write.csv(total_schoolloss_race, file = "processed/total_schoolloss_race.csv", row.names = FALSE)


schoolloss_race <- total_schoolloss_race %>%
  mutate(SLD_per_100k = SLD/student_pop*100000) %>%
  ggplot(aes(x=scenario, color = reorder(Race, -SLD_per_100k), y=SLD_per_100k))+
  geom_line(linewidth = 1)+
  geom_point(data = total_schoolloss_race %>% filter(scenario %in% relevant_scenarios)
             , size = 2.5)+
  # ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(name = "School Loss Days (per 100k students)")+
  #scale_x_reverse(
  scale_x_continuous(
    breaks = relevant_scenarios,
    name = "GSL water level (mASL)")+
  scale_color_manual(values = c(palette$red, palette$green, palette$orange, palette$blue, palette$dark),
                     #name = "Race",
                     labels = c("Hawaiian/Pacific Islander", "Black/African American", "Hispanic/Latino", "White Non-hispanic", "Asian")
  )+
  ggtitle("School Loss Days by race")+
  theme_cowplot(16)+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.justification = "center"
  )
schoolloss_race
ggsave("figs/schoolloss_by_race.png", 
       width = 8, height = 5)
