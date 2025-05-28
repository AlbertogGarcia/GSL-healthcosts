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
               ggplot2
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

# Load and merge processed data #####################################

#1 Emissions scenarios by water-level


#2 Merge population and incidence

ct_incidence_utah <- read.csv("data/processed/ct_incidence_utah.csv", stringsAsFactors =  FALSE)

#Add pollution change 
ct_incidence_utah_poll <- ct_incidence_utah %>%
  left_join(tff_pm25, by = c("ct_id"="geoid", "year"="year")); ct_incidence_utah_poll

# Mortality impact ######################################

#Coefficients from Krewski et al (2009)
beta <- 0.00582
se <- 0.0009628

#Mortality impact for adults (>=29 years old)
ct_health <- ct_incidence_utah_poll %>%
  filter(lower_age>29)%>%
  mutate(mortality_change = (1-(1/exp(beta*delta_totalpm25)))*incidence_2015*pop)

## Output for graph analysis

ct_health_mort <- ct_health %>%
  group_by(ct_id, year, sector, scenario)%>%
  summarise(mortality = sum(mortality_change),
            pop = sum(pop),
            disadvantaged = unique(disadvantaged))


## Prelim analsysis of results

##Statewide by emission source

ct_health_mort%>%
  group_by(scenario)%>%
  summarise(mortality = sum(mortality, na.rm = T))%>%
  ggplot(aes(x=scenario, y=mortality))+
  geom_bar(stat='identity')+
  theme_cowplot()

state_wide <- ct_health_mort%>%
  mutate(sce_sector = paste0(scenario,"_",sector))%>%
  group_by(year,sce_sector)%>%
  summarize(mortality = as.integer(sum(mortality_change)))%>%#,
  #mortality_2020comp = as.integer(sum(mortality_change_2020comp)),
  #mortality_2020inc = as.integer(sum(mortality_change_2020inc)));state_wide
  spread(sce_sector,mortality);state_wide

