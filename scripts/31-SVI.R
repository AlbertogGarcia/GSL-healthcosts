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

relevant_scenarios <- c(1275, 1277, 1278, 1280, 1281) # note, excludes 1282 baseline

scenario_pal <- c(palette$sc1275, palette$sc1277, palette$sc1278, palette$sc1280, palette$sc1281)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in data from health analyses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_mortality <- read.csv("processed/ct_mortality.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in and merge SVI data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SVI <- read.csv("data/SVI/Utah.csv") %>%
  rename(RPL_composite = RPL_THEMES, 
         RPL_socio = RPL_THEME1, 
         RPL_household = RPL_THEME2, 
         RPL_race = RPL_THEME3, 
         RPL_housing = RPL_THEME4,
         SPL_composite = SPL_THEMES, 
         SPL_socio = SPL_THEME1, 
         SPL_household = SPL_THEME2, 
         SPL_race = SPL_THEME3, 
         SPL_housing = SPL_THEME4) %>%
  select(FIPS, RPL_composite, RPL_socio, RPL_household, RPL_race, RPL_housing,
         SPL_composite, SPL_socio, SPL_household, SPL_race, SPL_housing
         )

mortality_SVI <- SVI %>%
  right_join(ct_mortality, by = "FIPS")%>%
  mutate(mortality_per100k = mortality/pop*100000)

mortality_SVI_current <- mortality_SVI %>%
  filter(scenario == 1278)

model1 <- lm(mortality ~ SPL_race + as.factor(scenario)
             , weights = pop
  , data = mortality_SVI
)
summary(model1)
