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
                "sc1277" = "#fdae61",
                "sc1278" = 
                  "grey50", 
                #"#ffd93f", 
                "sc1280" = "#abd9e9",
                "sc1281" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_scenarios <- seq(1275, 1281, by = 1) #just excludes baseline of 1282
relevant_scenarios <- c(1275, 1277, 1278, 1280, 1281) 

scenario_pal <- c(palette$sc1275, palette$sc1277, palette$sc1278, palette$sc1280, palette$sc1281)

n_storms_data = 2
n_storms_annual = 3

# main VSL and age-based VSL
VSL_24 = 12.57222

age_based_VSL_2024 <- read.csv("processed/age_based_VSL_2024.csv")%>%
  select(age, age_vsl_2024)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load and merge processed data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas_event.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% all_scenarios)

#2 Population and incidence
ct_incidence_mortality <- read.csv("processed/ct_incidence_mortality.csv", stringsAsFactors =  FALSE)

age_VSL_2024_grouped <- ct_incidence_mortality %>%
  select(lower_age, upper_age, age_group) %>%
  distinct()%>%
  tidyr::crossing(age_based_VSL_2024)%>%
  filter(age >= lower_age & age <= upper_age)%>%
  group_by(lower_age, upper_age, age_group) %>%
  summarise(age_vsl_2024 = mean(age_vsl_2024))


# Same but disaggregated by race
ct_incidence_mortality_race <- read.csv("processed/ct_incidence_mortality_race.csv", stringsAsFactors =  FALSE) 

age_VSL_2024_grouped_race <- ct_incidence_mortality_race %>%
  select(lower_age, upper_age, age_group) %>%
  distinct()%>%
  tidyr::crossing(age_based_VSL_2024)%>%
  filter(age >= lower_age & age <= upper_age)%>%
  group_by(lower_age, upper_age, age_group) %>%
  summarise(age_vsl_2024 = mean(age_vsl_2024))

#Merge w/ pollution deltas 
ct_mortality_pollution <- ct_incidence_mortality %>%
  left_join(scenario_pm_deltas, by = "FIPS")%>%
  left_join(age_VSL_2024_grouped, by = c("lower_age", "upper_age", "age_group"))

#Merge w/ pollution deltas 
ct_mortality_pollution_race <- ct_incidence_mortality_race %>%
  left_join(scenario_pm_deltas, by = "FIPS")%>%
  left_join(age_VSL_2024_grouped_race, by = c("lower_age", "upper_age", "age_group"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Mortality impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 microgram increase

#Mortality impact
ct_mortality_age <- ct_mortality_pollution %>%
  mutate(
    beta_pm10 = case_when(
      endpoint == "Mortality, All-cause" ~ log(1.0041)/10,
      endpoint == "Mortality, Respiratory" ~ log(1.0091)/10,
      endpoint == "Mortality, Cardiovascular" ~ log(1.0060)/10
      ),
    beta_pm25 = case_when(
      endpoint == "Mortality, All-cause" ~ log(1.0065)/10,
      endpoint == "Mortality, Respiratory" ~ log(1.0073)/10,
      endpoint == "Mortality, Cardiovascular" ~ log(1.0092)/10
      ),
    mortality_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_daily*pop)*(n_storms_annual/n_storms_data),
    mortality_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_daily*pop)*(n_storms_annual/n_storms_data),
    mortality = mortality_pm10 + mortality_pm25,
    life_yrs_remaining = 77.2 - (lower_age + upper_age)/2,
    costs_VSL = mortality*VSL_24,
    costs_age_VSL = mortality*age_vsl_2024)%>%
  drop_na(scenario)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Create data for analyses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# total mortality by census tract
ct_mortality <- ct_mortality_age %>%
  group_by(scenario, FIPS, County) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            pop = sum(pop, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T))%>%
  ungroup


write.csv(ct_mortality, file = "processed/ct_mortality.csv", row.names = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_mortality <- ct_mortality_age %>%
  group_by(scenario, endpoint) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T)
  )%>%
  ungroup

total_mortality %>%
  filter(scenario %in% relevant_scenarios,
         endpoint == "Mortality, All-cause"
         )%>%
ggplot(aes(x=reorder(scenario, scenario, order = T), y=costs_VSL, fill = as.character(scenario))
)+
  #geom_point(data = data.frame(scenario = baseline_scenario, relative_mortality = 0), color = palette$dark)+
  geom_bar(stat='identity')+
  geom_hline(yintercept = 0, linewidth = 0.25)+
  scale_y_continuous(
    "Expected costs (millions USD)", 
    sec.axis = sec_axis(~ . / VSL_24, 
                        name = "Expected mortalities"
                        , breaks = seq(-0, 10, by = 1)
    )
  )+
  geom_text(aes(label=round(costs_VSL, 2), y=costs_VSL+(VSL_24/abs(max(mortality)+5)))
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
  group_by(scenario, County, endpoint) %>%
  summarise(pop = sum(pop, na.rm = T),
            mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T))%>%
  ungroup


county_mortality %>% 
  filter(endpoint == "Mortality, All-cause") %>%
  mutate(mortality_per_100k = mortality/pop*100000)%>%
  group_by(County) %>% 
  summarise(mortality_per_100k = mean(mortality_per_100k))%>%
  ggplot(aes(x=reorder(County, - mortality_per_100k), y=mortality_per_100k))+
  geom_bar(stat='identity')+
  ggtitle("Mortality burden across counties")+
  scale_y_continuous(#label = scales::percent, 
                     #  breaks = seq(from = 0.05, to = .3, by = 0.05),
                     name = "Expected mortalities per 100k residents (USD)")+
  theme_cowplot(16)+
  theme(axis.title.x=element_blank())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by age group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mortality_age <- ct_mortality_age %>%
  group_by(scenario, age_group, lower_age, upper_age, endpoint) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            pop = sum(pop, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T))%>%
  ungroup


mortality_age %>% 
  filter(scenario == 1278, 
         endpoint == "Mortality, All-cause"
         )%>%
  mutate(proportion_of_mortality = mortality/sum(mortality))%>%
  group_by(age_group, lower_age, upper_age) %>% 
  summarise(proportion_of_mortality = mean(proportion_of_mortality))%>%
  ggplot(aes(x=reorder(age_group, lower_age), y=proportion_of_mortality))+
  geom_bar(stat='identity')+
  ggtitle("Distribution of mortality across age")+
  scale_y_continuous(label = scales::percent, 
                    # breaks = seq(from = 0.01, to = .07, by = 0.01),
                     name = "Percent of total expected mortality")+
  theme_cowplot()+
  theme(axis.title.x=element_blank())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by race
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#All-cause coefficients
RR_pm25 = 1.0065
beta_pm25 <- log(RR_pm25)/10
RR_pm10 = 1.0041
beta_pm10 <- log(RR_pm10)/10

#Mortality impact
ct_mortality_agebyrace <- ct_mortality_pollution_race %>%
  mutate(mortality_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_daily*pop)*(n_storms_annual/n_storms_data),
         mortality_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_daily*pop)*(n_storms_annual/n_storms_data),
         mortality = mortality_pm10 + mortality_pm25,
         life_yrs_remaining = 77.2 - (lower_age + upper_age)/2,
         costs_VSL = mortality*VSL_24,
         costs_age_VSL = mortality*age_vsl_2024
  )%>%
  drop_na(scenario)

mortality_race <- ct_mortality_agebyrace %>%
  group_by(scenario, race) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            pop = sum(pop, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T)
            )%>%
  mutate(mortality_per_100k = mortality/pop*100000,
         cost_per_capita = costs_VSL/pop,
         age_cost_per_capita = costs_age_VSL/pop)%>%
  ungroup %>%
  filter(race %in% c(
    "Asian alone", "White NH", "Hispanic or Latino", "Black or African American Alone", "Hawaiian and PI Alone"
  )
  )

mortality_race %>%
ggplot(aes(x=scenario, color = reorder(race, -mortality_per_100k), y=mortality_per_100k))+
  geom_line(linewidth = 1.5)+
  # ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(name = paste("Dust-induced mortalities per 100 thousand people"),
                     limits = c(0, max(mortality_race$mortality_per_100k))
                     )+
  scale_x_reverse(breaks = relevant_scenarios,
                  name = "GSL water elevation (mASL)")+
  scale_color_manual(values = c(palette$red, palette$blue, palette$dark, palette$orange, palette$green),
                     labels = c("Hawaiian/Pacific Islander", "White Non-hispanic", "Asian", "Hispanic/Latino", "Black/African American")
                     )+
  theme_cowplot(14)+
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank())
