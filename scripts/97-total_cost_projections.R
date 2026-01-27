# GSL dust costs: Mortality and asthma data processing
# albert.garcia@utah.edu
# created: 06/20/2025
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
               cowplot,
               purrr,
               ggplot2
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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Annual
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Mortality
total_mortality <- read.csv("processed/total_mortality.csv", stringsAsFactors =  FALSE) %>%
  filter(endpoint == "Mortality, All-cause",
         scenario %in% relevant_scenarios)%>%
  mutate(costs = costs_VSL*1000000)%>%
  select(scenario, costs) %>%
  mutate(endpoint_category = "Mortality")

#### School Loss Days
total_schoolloss <- read.csv("processed/total_schoolloss.csv", stringsAsFactors =  FALSE) %>%
  rename(costs = costs_SLD)%>%
  select(scenario, costs) 

#### All morbidity
total_morbidity <- read.csv("processed/total_morbidity.csv", stringsAsFactors =  FALSE) %>%
  group_by(scenario) %>%
  summarise(costs = sum(COI_24, na.rm = T))%>%
  ungroup %>%
  rbind(total_schoolloss) %>%
  group_by(scenario) %>%
  summarise(costs = sum(costs, na.rm = T))%>%
  ungroup %>%
  mutate(endpoint_category = "Morbidity")

total_annual_costs_plot <- total_morbidity %>%
  rbind(total_mortality) %>%
  mutate(costs_millions = costs/1000000)%>%
  ggplot(aes(x=reorder(scenario, scenario, order = T), y=costs_millions, fill = endpoint_category)
  )+
  geom_bar(stat='identity')+
  geom_hline(yintercept = 0, linewidth = 0.25)+
  ggtitle("Annual dust-induced health costs") + 
  xlab("GSL water level (ftASL)")+
  ylab("Costs (millions USD)") +
  scale_fill_manual(values = c(palette$blue, palette$red, palette$green))+
  theme_cowplot(16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )+
  guides(fill = guide_legend(title = NULL, reverse=T, hjust = 0.5))
total_annual_costs_plot
  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Projections through 2060
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Mortality
total_mortality_projections <- read.csv("processed/total_mortality_projections.csv", stringsAsFactors =  FALSE) %>%
  filter(scenario %in% relevant_scenarios)%>%
  mutate(PV_costs = PV_costs_VSL,
         PV_cum_costs = PV_cum_costs_VSL)%>%
  select(scenario, Year, PV_costs, PV_cum_costs) %>%
  mutate(endpoint_category = "Mortality")


#### School Loss Days
total_schoolloss_projections <- read.csv("processed/total_schoolloss_projections.csv", stringsAsFactors =  FALSE) %>%
  rename(PV_costs = PV_costs_SLD,
         PV_cum_costs = PV_cum_costs_SLD)%>%
  select(scenario, Year, PV_costs, PV_cum_costs) 


#### Morbidity
total_morbidity_projections <- read.csv("processed/total_morbidity_projections.csv", stringsAsFactors =  FALSE) %>%
  group_by(scenario, Year) %>%
  summarise(PV_costs = sum(PV_costs_COI, na.rm = T),
            PV_cum_costs = sum(PV_cum_costs_COI, na.rm = T))%>%
  ungroup %>%
  rbind(total_schoolloss_projections) %>%
  group_by(scenario, Year) %>%
  summarise(PV_costs = sum(PV_costs, na.rm = T),
            PV_cum_costs = sum(PV_cum_costs, na.rm = T))%>%
  ungroup %>%
  mutate(endpoint_category = "Morbidity")


cum_costs_proj <- total_morbidity_projections %>%
  rbind(total_mortality_projections) %>%
  group_by(scenario, Year) %>%
  summarise(PV_costs = sum(PV_costs),
            PV_cum_costs = sum(PV_cum_costs),
            PV_costs_billions = sum(PV_costs/1000),
            PV_cum_costs_billions = sum(PV_cum_costs/1000))

ggplot(data = cum_costs_proj,
       aes(x = Year, y = PV_costs, color = as.character(scenario))
)+
  geom_line()+
  geom_point(size = 1.5)+
  scale_y_continuous(name = "Costs (present value millions USD)"
  , breaks = seq(0, 250, 50)) +
  ggtitle("Annual projected health costs (2025-2060)")+
  scale_color_manual(name = "GSL water level (mASL)", values = scenario_pal)+
  theme_cowplot(16)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)#, size=16)
  )

ggplot(data = cum_costs_proj,
       aes(x=reorder(scenario, scenario, order = T), y=PV_costs_billions, fill = as.character(scenario))
)+
  geom_bar(stat='identity')+
  geom_hline(yintercept = 0, linewidth = 0.25)+
  ggtitle("Projected dust-induced health costs through 2060") + 
  xlab("GSL water level (ftASL)")+
  ylab("Costs (billions USD)") +
  scale_fill_manual(name = "GSL water level (mASL)", values = scenario_pal)+
  theme_cowplot(16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )+
  guides(fill = guide_legend(title = NULL, reverse=T, hjust = 0.5))
