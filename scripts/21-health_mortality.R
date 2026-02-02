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
                "bad" = "#d7191c",
                "current" = "#fdae61",
                "target" = "#abd9e9",
                "avg" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_scenarios <- seq(4182, 4203, by = 1) 
current_scenario = 4192
relevant_scenarios <- c(4183, current_scenario, 4198, 4200) 

scenario_pal <- c(palette$bad, palette$current, palette$target, palette$avg)

scenario_descrip <- c("No conservation",
                      "Current lake    ",
                      "Minimum healthy\nlake          ",
                      "Recent historical\naverage      "
                      )

n_years_storms = 6

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
#### Mortality impacts - relative to current levels
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 microgram increase

#Mortality impact
ct_mortality_age_temp <- ct_mortality_pollution %>%
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
    incidence_rate_event = incidence_rate_daily*event_days,
    pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
    pm25_delta = ifelse(scenario == current_scenario, pm25_delta, relative_pm25_delta),
    mortality_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
    mortality_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_event*pop)/n_years_storms,
    mortality = mortality_pm10 + mortality_pm25,
    pm_delta = pm10_delta + pm25_delta
    #life_yrs_remaining = 77.2 - (lower_age + upper_age)/2,
    )%>%
  drop_na(scenario)

ct_mortality_age_current <- ct_mortality_age_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_mortality = mortality,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, age_group, lower_age, upper_age, endpoint, current_mortality, current_pm_delta)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Get overall mortality impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ct_mortality_age <- ct_mortality_age_temp %>%
  left_join(ct_mortality_age_current, by = c("FIPS", "County", "event", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_mortality = ifelse(scenario == current_scenario, 0, mortality),
         mortality = relative_mortality + current_mortality,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         costs_VSL = mortality*VSL_24,
         costs_age_VSL = mortality*age_vsl_2024) %>%
  select(FIPS, County, scenario, event, age_group, lower_age, upper_age, incidence_rate, pop, pm_delta, endpoint, mortality, costs_VSL, costs_age_VSL)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Create data for analyses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# total mortality by census tract
ct_mortality <- ct_mortality_age %>%
  group_by(scenario, FIPS, County, endpoint) %>%
  summarise(pm_delta = weighted.mean(pm_delta, pop, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            population = sum(pop, na.rm = T)/length(unique(event)),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T),
            ct_incidence_rate_annual = weighted.mean(incidence_rate, pop, na.rm = T)
            )%>%
  ungroup

write.csv(ct_mortality, file = "processed/ct_mortality.csv", row.names = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_mortality <- ct_mortality_age %>%
  group_by(scenario, endpoint) %>%
  summarise(mortality = sum(mortality, na.rm = T),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T)
  )%>%
  ungroup

write.csv(total_mortality, file = "processed/total_mortality.csv", row.names = FALSE)


mortality_plot_prep <- total_mortality %>%
  select(scenario, mortality, endpoint) %>%
  filter(scenario %in% relevant_scenarios)%>%
  group_by(scenario)%>%
  mutate(mortality = ifelse(endpoint == "Mortality, All-cause", 2*mortality - sum(mortality), mortality),
         endpoint = case_when(endpoint == "Mortality, All-cause" ~ "All other causes",
                              endpoint == "Mortality, Respiratory" ~ "Respiratory",
                              endpoint == "Mortality, Cardiovascular" ~ "Cardiovascular"),
         total_mortality = sum(mortality),
         costs_VSL = mortality*VSL_24,
         total_costs_VSL = total_mortality*VSL_24
         ) %>%
  ungroup 

mortality_costs_plot <- mortality_plot_prep %>%
  ggplot(aes(x=reorder(scenario, scenario, order = T), y=mortality, fill = reorder(endpoint, - mortality))
  )+
  geom_bar(stat='identity')+
  #geom_hline(yintercept = 0, linewidth = 0.25)+
  scale_x_discrete(labels = scenario_descrip)+
  scale_y_continuous(
    "Premature mortalities",
    sec.axis = sec_axis(~ . * VSL_24, 
                        name = "Costs (millions USD)",
                        breaks = seq(0, 80, 20)
    )
  )+
  # geom_text(data = mortality_costs_plot_prep
  #           , aes(label=round(total_mortality, 2), y=total_mortality+1)
  #           #, fontface='bold'
  # ) +
  ggtitle("Current annual dust-induced mortality costs") + 
  #xlab("Lake scenario")+
  scale_fill_manual(values = c(palette$blue, palette$red, palette$green))+
  theme_cowplot(16)+
  theme(legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
       )+
  coord_flip()+
  guides(fill = guide_legend(title = NULL, reverse=T, hjust = 0.5))
mortality_costs_plot
ggsave("figs/mortality_costs.png", width = 9, height = 7)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Alternate VSL comparison
total_mortality %>%
  filter(scenario %in% relevant_scenarios,
         endpoint == "Mortality, All-cause"
  )%>%
  pivot_longer(cols = c("costs_VSL", "costs_age_VSL"), names_to = "VSL_type", values_to = "costs") %>%
  mutate(VSL_type = case_when(VSL_type == "costs_VSL" ~ "EPA recommended",
                              VSL_type == "costs_age_VSL" ~ "Cohort-adjusted\n(Aldy & Viscusi 2008)"
  )) %>%
  ggplot(aes(x=reorder(scenario, scenario, order = T), y=costs, fill = reorder(VSL_type, costs))
  )+
  geom_bar(stat='identity', width=.8, position = "dodge")+
  scale_x_discrete(labels = scenario_descrip)+
  scale_y_continuous(
    "Expected costs (millions USD)"
  )+
  scale_fill_manual(values = c(palette$blue, palette$red), name = "VSL")+
  theme_cowplot(16)+
  #xlab("GSL water level (ftASL)") + 
  theme(legend.position = "top",
        legend.justification = "center",
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()
      )+
  coord_flip()+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5, reverse = T))
ggsave("figs/mortality_by_VSL.png", width = 8, height = 6)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by County
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

county_mortality <- ct_mortality_age %>%
  group_by(scenario, County, endpoint) %>%
  summarise(pop = sum(pop, na.rm = T)/length(unique(event)),
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
  summarise(mortality = sum(mortality, na.rm = T),
            pop = sum(pop, na.rm = T)/length(unique(event)),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T))%>%
  ungroup


mortality_age %>% 
  filter(scenario == current_scenario, 
         endpoint == "Mortality, All-cause"
         )%>%
  mutate(proportion_of_mortality = mortality/sum(mortality),
         age_group = ifelse(age_group == "85 and over", "Over 85", age_group))%>%
  group_by(age_group, lower_age, upper_age) %>% 
  summarise(proportion_of_mortality = mean(proportion_of_mortality))%>%
  ggplot(aes(x=reorder(age_group, lower_age), y=proportion_of_mortality))+
  geom_bar(stat='identity')+
  ggtitle("All-cause mortality by age")+
  scale_y_continuous(label = scales::percent, 
                    # breaks = seq(from = 0.01, to = .07, by = 0.01),
                     name = "Percent of total expected mortality")+
  theme_cowplot(16)+
  theme(axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11)
  )
ggsave("figs/mortality_by_age.png",
       width = 9, height = 6)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Mortality by race
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#All-cause coefficients
RR_pm25 = 1.0065
beta_pm25 <- log(RR_pm25)/10
RR_pm10 = 1.0041
beta_pm10 <- log(RR_pm10)/10

#Mortality impact
ct_mortality_agebyrace_temp <- ct_mortality_pollution_race %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         pm10_delta = ifelse(scenario == current_scenario, pm10_delta, relative_pm10_delta),
         pm25_delta = ifelse(scenario == current_scenario, pm25_delta, relative_pm25_delta),
         mortality_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)/n_years_storms,
         mortality_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_event*pop)/n_years_storms,
         mortality = mortality_pm10 + mortality_pm25,
         pm_delta = pm10_delta + pm25_delta)%>%
  drop_na(scenario)


#### Get overall impacts, not just relative
ct_mortality_agebyrace_current <- ct_mortality_agebyrace_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_mortality = mortality,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, race, age_group, lower_age, upper_age, endpoint, current_mortality, current_pm_delta)

ct_mortality_agebyrace <- ct_mortality_agebyrace_temp %>%
  left_join(ct_mortality_agebyrace_current, by = c("FIPS", "County", "event", "race", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_mortality = ifelse(scenario == current_scenario, 0, mortality),
         mortality = relative_mortality + current_mortality,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         costs_VSL = mortality*VSL_24,
         costs_age_VSL = mortality*age_vsl_2024) %>%
  select(FIPS, County, scenario, event, race, age_group, lower_age, upper_age, incidence_rate, pop, pm_delta, endpoint, mortality, costs_VSL, costs_age_VSL)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Aggregate to relevant scales
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ct_mortality_race <- ct_mortality_agebyrace %>%
  group_by(scenario, FIPS, County, race, endpoint) %>%
  summarise(mortality = sum(mortality, na.rm = T),
            population = sum(pop, na.rm = T)/length(unique(event)),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T))%>%
  ungroup

write.csv(ct_mortality_race, file = "processed/ct_mortality_race.csv", row.names = FALSE)

total_mortality_race <- ct_mortality_agebyrace %>%
  group_by(scenario, race) %>%
  summarise(mortality = sum(mortality, na.rm = T),
            population = sum(pop, na.rm = T)/length(unique(event)),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T)
            )%>%
  mutate(mortality_per_100k = mortality/population*100000,
         cost_per_capita = costs_VSL/population,
         age_cost_per_capita = costs_age_VSL/population
         )%>%
  ungroup %>%
  filter(race %in% c(
    "Asian alone", "White NH", "Hispanic or Latino", "Black or African American Alone", "Hawaiian and PI Alone"
  )
  )
write.csv(total_mortality_race, file = "processed/total_mortality_race.csv", row.names = FALSE)

mortality_race <- total_mortality_race %>%
ggplot(aes(x=scenario, color = reorder(race, -mortality_per_100k), y=mortality_per_100k))+
  geom_line(linewidth = 1)+
  geom_point(data = total_mortality_race %>% filter(scenario %in% relevant_scenarios)
             , size = 2.5)+
  # ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(name = paste("Premature mortality (per 100k)"),
                     limits = c(0, max(total_mortality_race$mortality_per_100k))
                     )+
  #scale_x_reverse(
  scale_x_continuous(
    breaks = relevant_scenarios,
                  name = "GSL water level (ftASL)")+
  scale_color_manual(values = c(palette$red, palette$blue, palette$dark, palette$orange, palette$green),
                     #name = "Race",
                     labels = c("Hawaiian/Pacific Islander", "White Non-hispanic", "Asian", "Hispanic/Latino", "Black/African American")
                     )+
  ggtitle("All-cause mortality by race")+
  theme_cowplot(16)+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.justification = "center"
        )
mortality_race

ggsave("figs/mortality_by_race.png", 
       width = 8, height = 5)


ct_mortality_map <- ct_mortality_agebyrace %>%
  group_by(scenario, FIPS, County, endpoint) %>%
  summarise(pw_pm_delta = weighted.mean(pm_delta, pop, na.rm = T),
            pm_delta = mean(pm_delta, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            population = sum(pop, na.rm = T)/length(unique(event)),
            costs_VSL = sum(costs_VSL, na.rm = T),
            costs_age_VSL = sum(costs_age_VSL, na.rm = T),
            ct_incidence_rate_annual = weighted.mean(incidence_rate,pop)
  )%>%
  ungroup
write.csv(ct_mortality_map, file = "processed/ct_mortality_map.csv", row.names = FALSE)

