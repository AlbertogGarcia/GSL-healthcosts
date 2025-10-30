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
                #"sc1278" = 
                #  "grey50", 
                #"#ffd93f", 
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
  filter(scenario %in% all_scenarios)

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
beta_pm25 <- log(RR_pm25)/10

beta_pm10 <- 2.5/100/10

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_schoolloss_temp <- ct_school_pollution %>%
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
ct_schoolloss_current <- ct_schoolloss_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_SLD = SLD,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, age_group, lower_age, upper_age, endpoint, current_SLD, current_pm_delta)

ct_schoolloss <- ct_schoolloss_temp %>%
  left_join(ct_schoolloss_current, by = c("FIPS", "County", "event", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_SLD = ifelse(scenario == current_scenario, 0, SLD),
         SLD = relative_SLD + current_SLD,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         costs_SLD = SLD*SLD_24) %>%
  select(FIPS, County, scenario, event, age_group, lower_age, upper_age, pop, pm_delta, endpoint, SLD, costs_SLD)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Getting impacts disaggregated by race
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


ct_schoolloss_race_temp <- ct_school_pollution_race %>%
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
ct_schoolloss_race_current <- ct_schoolloss_race_temp %>%
  filter(scenario == current_scenario) %>%
  rename(current_SLD = SLD,
         current_pm_delta = pm_delta) %>%
  select(FIPS, County, event, Race, age_group, lower_age, upper_age, endpoint, current_SLD, current_pm_delta)

ct_schoolloss_race <- ct_schoolloss_race_temp %>%
  left_join(ct_schoolloss_race_current, by = c("FIPS", "County", "event", "Race", "age_group", "lower_age", "upper_age", "endpoint"))%>%
  mutate(relative_SLD = ifelse(scenario == current_scenario, 0, SLD),
         SLD = relative_SLD + current_SLD,
         relative_pm_delta = ifelse(scenario == current_scenario, 0, pm_delta),
         pm_delta = relative_pm_delta + current_pm_delta,
         costs_SLD = SLD*SLD_24) %>%
  select(FIPS, County, scenario, event, Race, age_group, lower_age, upper_age, pop, pm_delta, endpoint, SLD, costs_SLD)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Aggregating to relevant scales
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_schoolloss <- ct_schoolloss %>%
  group_by(scenario, endpoint) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            costs_SLD = sum(costs_SLD, na.rm = T)/1000000,
            pop = sum(pop, na.rm = T)/length(unique(event))
  )%>%
  ungroup %>%
  mutate(delta_rate = SLD/pop)

schoolloss_costs <- total_schoolloss %>%
  filter(scenario %in% relevant_scenarios
  )%>%
  ggplot(aes(x=reorder(scenario, scenario, order = T), y=costs_SLD, fill = as.character(scenario))
  )+
  #geom_point(data = data.frame(scenario = baseline_scenario, relative_mortality = 0), color = palette$dark)+
  geom_bar(stat='identity')+
  #geom_hline(yintercept = 0, linewidth = 0.25)+
  scale_y_continuous(
    "Expected costs (millions USD)", 
    sec.axis = sec_axis(~ . / SLD_24*1000000, 
                        name = "Expected school loss days"
                       # , breaks = seq(0, 8000, by = 1000)
    )
  )+
  geom_text(aes(label=round(costs_SLD, 2), y=costs_SLD+(SLD_24*3/abs(max(SLD))))
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
schoolloss_costs

schoolloss_counts <- total_schoolloss %>%
  filter(scenario %in% relevant_scenarios
  )%>%
  ggplot(aes(x=reorder(scenario, scenario, order = T), y=SLD, fill = as.character(scenario))
  )+
  #geom_point(data = data.frame(scenario = baseline_scenario, relative_mortality = 0), color = palette$dark)+
  geom_bar(stat='identity')+
  #geom_hline(yintercept = 0, linewidth = 0.25)+
  scale_y_continuous(
    "Dust-induced school loss days", 
    # sec.axis = sec_axis(~ . / SLD_24*1000000, 
    #                     name = "Expected school loss days"
    #                     # , breaks = seq(0, 8000, by = 1000)
    # )
  )+
  geom_text(aes(label=round(SLD, 0), y=SLD+(SLD_24*1000/abs(max(SLD))))
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
schoolloss_counts




total_schoolloss_race <- ct_schoolloss_race %>%
  drop_na(Race) %>%
  filter(Race %ni% c("Other Race", "Multiple Race", "American Indian"))%>%
  group_by(scenario, endpoint, Race) %>%
  summarise(SLD = sum(SLD, na.rm = T),
            costs_SLD = sum(costs_SLD, na.rm = T),
            pop = sum(pop, na.rm = T)/length(unique(event))
  )%>%
  ungroup %>%
  mutate(SLD_per_100k = SLD/pop*100000)

schoolloss_race <- total_schoolloss_race %>%
  ggplot(aes(x=scenario, color = reorder(Race, -SLD_per_100k), y=SLD_per_100k))+
  geom_line(linewidth = 1)+
  geom_point(data = total_schoolloss_race %>% filter(scenario %in% relevant_scenarios)
             , size = 2.5)+
  # ggtitle("Distribution of mortality risk across race")+
  scale_y_continuous(name = paste("School Loss Days (per 100k students)"),
                     limits = c(0, max(total_schoolloss_race$SLD_per_100k))
  )+
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
