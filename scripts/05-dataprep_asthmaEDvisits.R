# GSL dust costs: Mortality and asthma data processing
# albert.garcia@utah.edu
# created: 06/20/2025
# updated: 

###trying to get the existing incident asthma rate by tract and age group

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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Asthma ED visits
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# census-tract population demographics (by age)

ct_ACS_age_wide <- readxl::read_excel("data/population/Demographic_Raw_ACS2014.xlsx")

ct_ACS_age <- ct_ACS_age_wide %>%
  select(-`Total population`)%>%
  mutate("5 to 14" = `5 to 9 years` + `10 to 14 years`, 
         "15 to 34" = `15 to 19 years` + `20 to 24 years` + `25 to 34 years`, 
         "35 to 64" = `35 to 44 years` + `45 to 54 years` + `55 to 59 years` + `60 to 64 years`, 
  )%>%
  pivot_longer(`Under 5 years`:ncol(.), names_to = "age_group", values_to = "pop")%>%
  filter(age_group %in% c("Under 5 years", 
                          "5 to 14", 
                          "15 to 34",
                          "35 to 64",
                          "65 years and over"))%>%
  mutate(lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         age_group = str_remove(age_group," years"),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",100,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age) ## What it actually should be (under 5 year old)
  )

# get county by age population totals
county_age_pop_ACS <- ct_ACS_age %>%
  group_by(County, lower_age, upper_age)%>%
  summarise(pop = sum(pop, na.rm = T))

# Get county level incidence data
edvisits <- read.csv("data/health/asthma/edvisits10_14.csv") %>%
  rename(age_group = Age)%>%
  mutate(County = ifelse(County == "Salt lake", "Salt Lake", County))%>%
  filter(County %in% county_age_pop_ACS$County)%>%
  mutate(lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",100,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age) ## What it actually should be (under 5 year old)
  )%>%
  full_join(county_age_pop_ACS, by = c("lower_age", "upper_age", "County"))%>%
  mutate(ED_visits = as.numeric(gsub(",","", ED_visits)),
         EDvisits_annual = ED_visits/5,
         EDvisits_rate = EDvisits_annual/pop)%>%
  select(-pop, -ED_visits, -EDvisits_annual, - age_group)

# match back to census tracts
ct_edvisits <- ct_ACS_age %>%
  full_join(edvisits, by = c("lower_age", "upper_age", "County")) %>%
  mutate(EDvisits_rate = replace_na(EDvisits_rate, 0),
         endpoint = "Asthma ED visits")



write.csv(ct_edvisits, file = "processed/ct_edvists.csv", row.names = FALSE)

