# GSL dust costs: asthma impacts
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

relevant_scenarios <- c(1275, 1277, 1278, 1280, 1281) # note, excludes 1282 baseline

scenario_pal <- c(palette$sc1275, palette$sc1277, palette$sc1278, palette$sc1280, palette$sc1281)

n_storms_data = 2
n_storms_annual = 3


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load and merge processed data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas_daily.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_asthma <- read.csv("processed/ct_edvists.csv", stringsAsFactors =  FALSE) %>%
  mutate(incidence_rate_daily = EDvisits_rate/365) # get daily incidence rate


#Merge w/ pollution deltas 
ct_asthma_pollution <- ct_incidence_asthma %>%
  left_join(scenario_pm_deltas, by = "FIPS")
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### asthma impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
#Coefficients from Bi et al.
  
RR_pm25 = 1.016
beta_pm25 <- log(RR_pm25)/6.3###IMPORTANT: for a 6.3 mcrogram increase

RR_pm10 = 1.014
beta_pm10 <- log(RR_pm10)/9.6###IMPORTANT: for a 9.6 mcrogram increase

#asthma impact
ct_asthmaED_age <- ct_asthma_pollution %>%
  mutate(asthmaED_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_daily*pop)*(n_storms_annual/n_storms_data),
         asthmaED_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_daily*pop)*(n_storms_annual/n_storms_data),
         asthmaED = asthmaED_pm10 + asthmaED_pm25)%>%
  drop_na(scenario)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Create data for analyses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# total asthma-induced ED visits by census tract
ct_asthmaED <- ct_asthmaED_age %>%
  group_by(scenario, FIPS, County) %>%
  summarise(asthmaED = sum(asthmaED, na.rm = T),
            pop = sum(pop, na.rm = T))%>%
  ungroup


write.csv(ct_asthmaED, file = "processed/ct_asthmaED.csv", row.names = FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_asthmaED <- ct_asthmaED_age %>%
  group_by(scenario) %>%
  summarise(asthmaED = sum(asthmaED, na.rm = T)
  )%>%
  ungroup


ggplot(total_asthmaED
       , aes(x=reorder(scenario, scenario, order = T), y=asthmaED, fill = as.character(scenario))
)+
  #geom_point(data = data.frame(scenario = baseline_scenario, relative_mortality = 0), color = palette$dark)+
  geom_bar(stat='identity')+
  geom_hline(yintercept = 0, linewidth = 0.25)+
  scale_fill_manual(values = scenario_pal, name = "GSL water level (mASL)")+
  theme_cowplot(16)+
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())+
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5))
