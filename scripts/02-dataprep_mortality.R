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
#### MORTALITY
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# census-tract population demographics (by age)

age_sheet = 3
ct_ACS_age_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = age_sheet)

ct_ACS_age <- ct_ACS_age_wide %>%
  select(-`Total population`)%>%
  mutate(`55 to 64 years` = `55 to 59 years` + `60 to 64 years`)%>%
  pivot_longer(`Under 5 years`:ncol(.), names_to = "age_group", values_to = "pop")%>%
  filter(age_group %ni% c("16 years and over", 
                          "18 years and over", 
                          "21 years and over",
                          "65 years and over",
                          "62 years and over",
                          "Under 18 years",
                          "55 to 59 years", "60 to 64 years"
                          )
         )%>%
  mutate(lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         age_group = str_remove(age_group," years"),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",100,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age) ## What it actually should be (under 5 year old)
  )

# get county by age population totals
county_age_pop_ACS <- ct_ACS_age %>%
  group_by(County, age_group, lower_age, upper_age)%>%
  summarise(pop = sum(pop, na.rm = T))
 
# Get county level incidence data

csv_files <- fs::dir_ls("data/health/mortality/CDC_wonder/compressed/age", regexp = "\\.csv$")
  
county_incidence <- csv_files %>%
  purrr::map_dfr(read.csv,
          .id = "filename")%>%
  mutate(filename = str_replace(basename(filename), "_1016.csv", ""),
         endpoint_file = str_replace(filename, "mortality_", ""),
         endpoint = case_when(
           endpoint_file == "all-cause" ~ "Mortality, All-cause",
           endpoint_file == "respiratory" ~ "Mortality, Respiratory",
           endpoint_file == "cardio" ~ "Mortality, Cardiovascular"
         )
         )%>%
  mutate(age_group = ifelse(Age.Group %in% c("< 1 year", "1-4 years"), "Under 5 years", Age.Group))%>%
  drop_na(County.Code)%>% filter(age_group != "NS") %>%
  group_by(County, age_group, endpoint)%>%
  summarise(Deaths = sum(Deaths, na.rm = T),
            Population = sum(Population, na.rm = T))%>%
  ungroup %>%
  mutate(County = str_remove(County, " County, UT"))%>%
  filter(County %in% county_age_pop_ACS$County)%>%
  mutate(age_group = str_replace(age_group, "-", " to "),
         age_group = str_remove(age_group," years"),
         lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(lower_age == 85,100,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age), ## What it actually should be (under 5 year old)
         incidence_rate = Deaths/Population
         ) %>%
  select(County, lower_age, upper_age, endpoint, incidence_rate)

table(county_age_pop_ACS$age_group) 
table(county_incidence$age_group) 

# match back to census tracts
ct_incidence_mortality <- ct_ACS_age %>%
  expand_grid(data.frame("endpoint" = c("Mortality, All-cause", "Mortality, Respiratory", "Mortality, Cardiovascular")))%>%
  full_join(county_incidence, by = c("lower_age", "upper_age", "County", "endpoint"), relationship = "many-to-many")%>%
  mutate(incidence_rate = tidyr::replace_na(incidence_rate, 0),
         incidence_rate_daily = incidence_rate/365) # get daily incidence rate


write.csv(ct_incidence_mortality, file = "processed/ct_incidence_mortality.csv", row.names = FALSE)

