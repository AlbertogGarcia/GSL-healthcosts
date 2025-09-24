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
               purrr,
               ggplot2
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
                "sc1278" = "grey50", 
                "sc1280" = "#abd9e9",
                "sc1281" = "#2c7bb6"
)


all_scenarios <- seq(1275, 1281, by = 1) #just excludes baseline of 1282
relevant_scenarios <- c(1275, 1278, 1280, 1281) 

scenario_pal <- c(palette$sc1275, palette$sc1277, palette$sc1280, palette$sc1281)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_mortality_projections <- read.csv("processed/total_mortality_projections.csv", stringsAsFactors =  FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### School Loss Days
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_schoolloss_projections <- read.csv("processed/total_schoolloss_projections.csv", stringsAsFactors =  FALSE)
 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Morbidity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Combine
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_projections <- total_mortality_projections %>%
  ungroup %>%
  filter(scenario %in% relevant_scenarios)%>%
  full_join(total_schoolloss_projections, by = c("scenario", "Year"))%>%
  mutate(PV_cum_costs = PV_cum_costs_VSL + PV_cum_costs_SLD,
         cum_costs = cum_costs_VSL + cum_costs_SLD
         )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Figures
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

costs_proj <- total_projections %>%
  ggplot(aes(x = Year, y = cum_costs/1000, color = as.character(scenario)))+
  geom_line()+
  geom_point(size = 1.5)+
  scale_y_continuous(name = "Cumulative costs (billions USD)",
                     breaks = seq(0, 3, by = 0.5)
  ) +
  ggtitle("Cumulative projected costs (2025-2060)")+
  scale_color_manual(name = "GSL water level (mASL)", values = scenario_pal)+
  theme_cowplot(16)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)#, size=16)
  )
costs_proj
