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
#### SET BASELINE LAKE SCENARIO
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
baseline_scenario = 1280

relevant_scenarios <- seq(1275, 1281, by = 1)

# Load and merge processed data #####################################

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_mortality <- read.csv("processed/ct_incidence_mortality.csv", stringsAsFactors =  FALSE)

# Same but disaggregated by race
ct_incidence_mortality_race <- read.csv("processed/ct_incidence_mortality_race.csv", stringsAsFactors =  FALSE)


#Merge w/ pollution deltas 
ct_mortality_pollution <- ct_incidence_mortality %>%
  left_join(scenario_pm_deltas, by = "FIPS")

#Merge w/ pollution deltas 
ct_mortality_pollution_race <- ct_incidence_mortality_race %>%
  left_join(scenario_pm_deltas, by = "FIPS")

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
ct_mortality_age <- ct_mortality_pollution %>%
  mutate(exposure_pm10 = pm10/10,
         exposure_pm25 = pm25/10,
         mortality_pm10 = (1-(1/exp(beta_pm10*exposure_pm10)))*incidence_rate*pop,
         mortality_pm25 = (1-(1/exp(beta_pm25*exposure_pm25)))*incidence_rate*pop,
         mortality_pm = mortality_pm10 + mortality_pm25)%>%
  drop_na(scenario)

#Mortality impact
ct_mortality_agebyrace <- ct_mortality_pollution_race %>%
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

income_sheet = 4

ct_mortality_relative_income <- ct_mortality_relative %>%
  left_join(
    readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = income_sheet)
    , by = "FIPS"
    )%>%
  mutate(median_HHI = as.numeric(`Median household income (dollars)`),
         median_MHHI = median(median_HHI, na.rm = T),
         above_median_HHI = as.character(ifelse(median_HHI >= median_MHHI, 1, 0)))

# ct_mortality_relative %>%
#   filter(scenario == 1277)%>%
#   mutate(qrelative_pm10 = percent_rank(relative_pm10),
#          qrelative_mortality = percent_rank(relative_mortality))%>%
#   ggplot(aes(x = qrelative_pm10, y = qrelative_mortality#, color = as.character(scenario)
#   )
#          )+
#   geom_point(shape = 21)+
#   theme_bw()

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
  select(scenario, pm10, pm25, relative_pm10, relative_pm25, relative_mortality_pm10, relative_mortality_pm25, relative_mortality)

ggplot(total_mortality_relative
       , aes(x=scenario, y=relative_mortality))+
  geom_point(data = data.frame(scenario = baseline_scenario, relative_mortality = 0), color = "grey30")+
  geom_bar(stat='identity')+
  scale_y_continuous(
    "Expected additional mortality\n(relative to 1280 mASL)", 
    sec.axis = sec_axis(~ . * VSL_24, 
                        name = "Expected costs (millions USD)",
                        breaks = seq(-300, 200, by = 100))
  )+
  scale_x_reverse(name = "Great Salt Lake water level (mASL)",
                  breaks = seq(1275, 1282, by = 1)
                  )+
  theme_cowplot()


# mortality by age group
mortality_age <- ct_mortality_age %>%
  group_by(scenario, age_group, lower_age, upper_age) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            pop = sum(pop, na.rm = T))%>%
  ungroup

mortality_age_relative <- mortality_age %>%
  left_join(mortality_age %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm)%>%
              dplyr::select(-c(scenario, pop))
            , by = c("age_group", "lower_age", "upper_age")
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm)%>%
  select(scenario, age_group, lower_age, upper_age, pop, relative_mortality_pm10, relative_mortality_pm25, relative_mortality)%>%
  group_by(scenario)%>%
  mutate(mortality_per_hundredk = relative_mortality/pop*100000,
         proportion_of_burden = relative_mortality/sum(relative_mortality))%>%
  ungroup

ggplot(mortality_age_relative %>% filter(scenario == 1277)
       , aes(x=reorder(age_group, lower_age), y=proportion_of_burden))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of mortality across age")+
  scale_y_continuous(label = scales::percent, 
                     breaks = seq(from = 0.05, to = .3, by = 0.05),
                     name = "Percent of total mortality")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())


# Mortality by race
mortality_race <- ct_mortality_agebyrace %>%
  group_by(scenario, race) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality_pm = sum(mortality_pm, na.rm = T),
            pop = sum(pop, na.rm = T))%>%
  ungroup


mortality_race_relative <- mortality_race %>%
  left_join(mortality_race %>%
              filter(scenario == baseline_scenario)%>%
              rename(baseline_mortality_pm10 = mortality_pm10,
                     baseline_mortality_pm25 = mortality_pm25,
                     baseline_mortality_pm = mortality_pm)%>%
              dplyr::select(-c(scenario, pop))
            , by = c("race")
  )%>%
  mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
         relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
         relative_mortality = mortality_pm - baseline_mortality_pm)%>%
  select(scenario, race, pop, relative_mortality_pm10, relative_mortality_pm25, relative_mortality)%>%
  group_by(scenario)%>%
  mutate(mortality_per_hundredk = relative_mortality/pop*100000,
         proportion_of_burden = relative_mortality/sum(relative_mortality))%>%
  ungroup%>%
  filter(race %in% c(
    "Asian alone", "White NH", "Hispanic or Latino", "Black or African American Alone", "Hawaiian and PI Alone"
  )
         )

ggplot(mortality_race_relative %>% filter(scenario == 1277)
       , aes(x=reorder(race, mortality_per_hundredk), y=mortality_per_hundredk))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(#breaks = seq(from = 0.05, to = .3, by = 0.05),
                     name = "Mortality per 100 thousand people")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())

ggplot(mortality_race_relative %>% filter(scenario <= baseline_scenario)
       , aes(x=scenario, color = race, y=mortality_per_hundredk))+
  geom_line()+
  # ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(#breaks = seq(from = 0.05, to = .3, by = 0.05),
    name = paste("Additional mortality per 100 thousand people\n(relative to", baseline_scenario, "mASL)"))+
  scale_x_reverse(breaks = seq(from = 1275, to = 1282, by = 1),
    name = "Great Salt Lake water level (mASL)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank())

