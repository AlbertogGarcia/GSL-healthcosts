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
                "light_grey" = "#d9d9d9",
                "grey" = "grey50",
                "dark" = "#0c2230",
                "red" = "#d7191c",
                "blue" = "#2c7bb6",
                "purple" = "#880ED4",
                "sc1275" = "#d7191c",
                "sc1277" = "#fdae61",
                "sc1278" = "#ffd92f", # "#fee090"
                "sc1280" = "#abd9e9",
                "sc1281" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### SET BASELINE LAKE SCENARIO
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
baseline_scenario = 1282

relevant_scenarios <- c(1275, 1277, 1278, 1280, 1281, baseline_scenario)

scenario_pal <- c(palette$sc1275, palette$sc1277, palette$sc1278, palette$sc1280, palette$sc1281)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### VSL and age-based VSL
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
VSL_24 = 12.57222

age_based_VSL_2024 <- read.csv("processed/age_based_VSL_2024.csv")%>%
  select(age, age_vsl_2024)

# Load and merge processed data #####################################

#1 Emissions scenarios by water-level
scenario_pm_deltas_daily <- read.csv("processed/scenario_pm_deltas_daily.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_mortality <- read.csv("processed/ct_incidence_mortality.csv", stringsAsFactors =  FALSE)%>%
  mutate(incidence_rate = incidence_rate/365) # get daily incidence rate

age_VSL_2024_grouped <- ct_incidence_mortality %>%
  select(lower_age, upper_age, age_group) %>%
  distinct()%>%
  tidyr::crossing(age_based_VSL_2024)%>%
  filter(age >= lower_age & age <= upper_age)%>%
  group_by(lower_age, upper_age, age_group) %>%
  summarise(age_vsl_2024 = mean(age_vsl_2024))


# Same but disaggregated by race
ct_incidence_mortality_race <- read.csv("processed/ct_incidence_mortality_race.csv", stringsAsFactors =  FALSE)


#Merge w/ pollution deltas 
ct_mortality_pollution <- ct_incidence_mortality %>%
  left_join(scenario_pm_deltas_daily, by = "FIPS")%>%
  left_join(age_VSL_2024_grouped, by = c("lower_age", "upper_age", "age_group"))

#Merge w/ pollution deltas 
ct_mortality_pollution_race <- ct_incidence_mortality_race %>%
  left_join(scenario_pm_deltas_daily, by = "FIPS")%>%
  left_join(age_VSL_2024_grouped, by = c("lower_age", "upper_age", "age_group"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Mortality impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 mcrogram increase
RR_pm25 = 1.0065
beta_pm25 <- log(RR_pm25)/10

RR_pm10 = 1.0041
beta_pm10 <- log(RR_pm10)/10

#Mortality impact
ct_mortality_age <- ct_mortality_pollution %>%
  mutate(mortality_pm10 = (1-(1/exp(beta_pm10*pm10)))*incidence_rate*pop,
         mortality_pm25 = (1-(1/exp(beta_pm25*pm25)))*incidence_rate*pop,
         mortality_pm = mortality_pm10 + mortality_pm25,
         life_yrs_remaining = 77.2 - (lower_age + upper_age)/2,
         costs_VSL = mortality_pm*VSL_24,
         costs_age_VSL = mortality_pm*age_vsl_2024)%>%
  drop_na(scenario)

#Mortality impact
ct_mortality_agebyrace <- ct_mortality_pollution_race %>%
  mutate(mortality_pm10 = (1-(1/exp(beta_pm10*pm10)))*incidence_rate*pop,
         mortality_pm25 = (1-(1/exp(beta_pm25*pm25)))*incidence_rate*pop,
         mortality_pm = mortality_pm10 + mortality_pm25,
         life_yrs_remaining = 77.2 - (lower_age + upper_age)/2,
         costs_VSL = mortality_pm*VSL_24,
         costs_age_VSL = mortality_pm*age_vsl_2024
  )%>%
  drop_na(scenario)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Create data for analyses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# total mortality by census tract
ct_mortality <- ct_mortality_age %>%
  group_by(scenario, FIPS, County) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T))%>%
  ungroup

ct_mortality_relative <- ct_mortality %>%
  left_join(ct_mortality %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm,
                     baseline_costs_VSL = costs_VSL,
                     baseline_costs_age_VSL = costs_age_VSL)%>%
              dplyr::select(-scenario)
            , by = c("FIPS", "County")
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm,
         relative_costs_VSL = costs_VSL - baseline_costs_VSL,
         relative_costs_age_VSL = costs_age_VSL - baseline_costs_age_VSL)%>%
  select(scenario, FIPS, County, relative_mortality_pm10, relative_mortality_pm25, relative_mortality, relative_costs_VSL, relative_costs_age_VSL)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_mortality <- ct_mortality_age %>%
  group_by(scenario) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T)
  )%>%
  ungroup

total_mortality_relative <- total_mortality %>%
  cbind(total_mortality %>%
          filter(scenario == baseline_scenario)%>%
          rename(baseline_mortality_pm10 = mortality_pm10,
                 baseline_mortality_pm25 = mortality_pm25,
                 baseline_mortality_pm = mortality_pm,
                 baseline_costs_VSL = costs_VSL,
                 baseline_costs_age_VSL = costs_age_VSL
          )%>%
          dplyr::select(-scenario)
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm,
         relative_costs_VSL = costs_VSL - baseline_costs_VSL,
         relative_costs_age_VSL = costs_age_VSL - baseline_costs_age_VSL
  )%>%
  filter(scenario != baseline_scenario)%>%
  select(scenario, relative_mortality_pm10, relative_mortality_pm25, relative_mortality, relative_costs_VSL, relative_costs_age_VSL)

ggplot(total_mortality_relative
       , aes(x=reorder(scenario, scenario, order = T), y=relative_costs_VSL, fill = as.character(scenario))
)+
  #geom_point(data = data.frame(scenario = baseline_scenario, relative_mortality = 0), color = palette$dark)+
  geom_bar(stat='identity')+
  geom_hline(yintercept = 0, linewidth = 0.25)+
  scale_y_continuous(
    "Expected costs (millions USD)\nrelative to 1278 mASL", 
    sec.axis = sec_axis(~ . / VSL_24, 
                        name = "Expected mortalities"
                        , breaks = seq(-0, 10, by = 1)
    )
  )+
  geom_text(aes(label=round(relative_costs_VSL, 2), y=relative_costs_VSL+VSL_24/abs(max(relative_mortality)))
            #, fontface='bold'
  ) +
  #xlab("GSL water level (mASL)")+
  scale_fill_manual(values = scenario_pal, name = "GSL water level (mASL)")+
  theme_cowplot(16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by County
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
county_mortality <- ct_mortality_age %>%
  group_by(scenario, County) %>%
  summarise(pop = sum(pop, na.rm = T),
            pm10 = mean(pm10, na.rm = T),
            pm25 = mean(pm25, na.rm = T),
            mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T))%>%
  ungroup

county_mortality_relative <- county_mortality %>%
  left_join(county_mortality %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_pm10 = pm10,
                     baseline_pm25 = pm25,
                     baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm,
                     baseline_costs_VSL = costs_VSL)%>%
              dplyr::select(-c(scenario, pop))
            , by = "County"
  )%>%
  mutate(relative_pm10 = pm10 - baseline_pm10,
         relative_pm25 = pm25 - baseline_pm25,
         relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm,
         relative_costs_VSL = costs_VSL - baseline_costs_VSL)%>%
  select(scenario, County, pop, pm10, pm25, relative_pm10, relative_pm25, relative_mortality_pm10, relative_mortality_pm25, relative_mortality, relative_costs_VSL)

county_mortality_relative %>% 
  filter(scenario != baseline_scenario, relative_costs_VSL > 0)%>%
  mutate(proportion_of_burden = relative_costs_VSL/sum(relative_costs_VSL))%>%
  group_by(County) %>% 
  summarise(proportion_of_burden = mean(proportion_of_burden))%>%
  ggplot(aes(x=reorder(County, - proportion_of_burden), y=proportion_of_burden))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of total costs across counties")+
  scale_y_continuous(label = scales::percent, 
                     #  breaks = seq(from = 0.05, to = .3, by = 0.05),
                     name = "Percent of total expected costs")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())

county_mortality_relative %>% 
  filter(scenario != baseline_scenario, relative_mortality > 0)%>%
  mutate(costs_per_100k = relative_costs_VSL/pop*100000)%>%
  group_by(County) %>% 
  summarise(costs_per_100k = mean(costs_per_100k))%>%
  ggplot(aes(x=reorder(County, - costs_per_100k), y=costs_per_100k))+
  geom_bar(stat='identity')+
  ggtitle("Costs per capita across counties")+
  scale_y_continuous(#label = scales::percent, 
                     #  breaks = seq(from = 0.05, to = .3, by = 0.05),
                     name = "Expected costs per capita (USD millions)")+
  theme_cowplot(16)+
  theme(axis.title.x=element_blank())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by age group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mortality_age <- ct_mortality_age %>%
  group_by(scenario, age_group, lower_age, upper_age) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            pop = sum(pop, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T))%>%
  ungroup

mortality_age_relative <- mortality_age %>%
  left_join(mortality_age %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm,
                     baseline_costs_VSL = costs_VSL)%>%
              dplyr::select(-c(scenario, pop))
            , by = c("age_group", "lower_age", "upper_age")
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm,
         relative_costs_VSL = costs_VSL - baseline_costs_VSL)%>%
  select(scenario, age_group, lower_age, upper_age, pop, relative_mortality_pm10, relative_mortality_pm25, relative_mortality, relative_costs_VSL)%>%
  group_by(scenario)%>%
  mutate(costs_per_100k = relative_costs_VSL/pop*100000,
         proportion_of_burden = relative_mortality/sum(relative_mortality))%>%
  ungroup

mortality_age_relative %>% 
  filter(scenario < baseline_scenario)%>%
  group_by(age_group, lower_age, upper_age) %>% 
  summarise(proportion_of_burden = mean(proportion_of_burden))%>%
  ggplot(aes(x=reorder(age_group, lower_age), y=proportion_of_burden))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of mortality across age")+
  scale_y_continuous(label = scales::percent, 
                     breaks = seq(from = 0.05, to = .3, by = 0.05),
                     name = "Percent of total expected mortality")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())

mortality_age_relative %>% 
  filter(scenario != baseline_scenario)%>%
  group_by(age_group, lower_age, upper_age) %>% 
  summarise(costs_per_100k = mean(costs_per_100k))%>%
  ggplot(aes(x=reorder(age_group, lower_age), y=costs_per_100k))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of costs by age")+
  scale_y_continuous(name = "Expected costs per 100k residents (USD millions)")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by race
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mortality_race <- ct_mortality_agebyrace %>%
  group_by(scenario, race) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            pop = sum(pop, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T))%>%
  ungroup


mortality_race_relative <- mortality_race %>%
  left_join(mortality_race %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm,
                     baseline_costs_VSL = costs_VSL)%>%
              dplyr::select(-c(scenario, pop))
            , by = c("race")
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm,
         relative_costs_VSL = costs_VSL - baseline_costs_VSL)%>%
  select(scenario, race, pop, relative_mortality_pm10, relative_mortality_pm25, relative_mortality, relative_costs_VSL)%>%
  group_by(scenario)%>%
  mutate(mortality_per_hundredk = relative_mortality/pop*100000,
         proportion_of_burden = relative_mortality/sum(relative_mortality))%>%
  ungroup%>%
  filter(race %in% c(
    "Asian alone", "White NH", "Hispanic or Latino", "Black or African American Alone", "Hawaiian and PI Alone"
  )
  )

ggplot(mortality_race_relative %>% filter(scenario == 1277)
       , aes(x=reorder(race, relative_costs_VSL), y=relative_costs_VSL))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of total mortality costs across race")+
  scale_y_continuous(#breaks = seq(from = 0.05, to = .3, by = 0.05),
    name = "Total mortality costs (millions USD)")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())

ggplot(mortality_race_relative %>% filter(scenario == 1277)
       , aes(x=reorder(race, mortality_per_hundredk), y=mortality_per_hundredk))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(#breaks = seq(from = 0.05, to = .3, by = 0.05),
    name = "Mortality per 100 thousand people")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())

ggplot(mortality_race_relative %>% filter(scenario < baseline_scenario)
       , aes(x=scenario, color = race, y=mortality_per_hundredk))+
  geom_line()+
  # ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(#breaks = seq(from = 0.05, to = .3, by = 0.05),
    name = paste("Additional mortality per 100 thousand people\n(relative to", baseline_scenario, "mASL)"))+
  scale_x_reverse(breaks = relevant_scenarios,
                  name = "Great Salt Lake water level (mASL)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by income
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

income_sheet = 4
ct_ACS_income <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = income_sheet)

ct_mortality_income <- ct_mortality_age %>%
  left_join(ct_ACS_income, by = "FIPS")%>%
  mutate(median_ct_HHI = median(as.numeric(`Median household income (dollars)`), na.rm = T),
         above_median_ct_HHI = ifelse(`Median household income (dollars)` >= median_ct_HHI, 1, 0))

