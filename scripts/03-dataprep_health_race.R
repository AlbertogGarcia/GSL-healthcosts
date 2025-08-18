# GSL dust costs: Mortality and asthma by race data processing
# albert.garcia@utah.edu
# created: 06/20/2025
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
               sjmisc
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### MORTALITY
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# census-tract population demographics by age AND RACE

agebyrace_sheet = 11
ct_ACS_agebyrace_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = agebyrace_sheet)

ct_ACS_agebyrace <- ct_ACS_agebyrace_wide %>%
  pivot_longer(3:ncol(.), names_to = "agebyrace_group", values_to = "pop")%>%
  filter(!str_detect(agebyrace_group, "Total"))%>%
  mutate(agebyrace_group = str_remove_all(agebyrace_group, "[()]"),
         race = case_when(
           str_detect(agebyrace_group, "White") ~ "White NH",
           str_detect(agebyrace_group, "Hispanic") ~ "Hispanic or Latino",
           str_detect(agebyrace_group, "American Indian and Alaska Native") ~ "American Indian and Alaska Native Alone",
           str_detect(agebyrace_group, "Black") ~ "Black or African American Alone",
           str_detect(agebyrace_group, "Asian") ~ "Asian alone",
           str_detect(agebyrace_group, "Hawaiian and PI") ~ "Hawaiian and PI Alone",
           str_detect(agebyrace_group, "Other Race") ~ "Other Race Alone",
           str_detect(agebyrace_group, "Multi Racial") ~ "Multi Racial"
  ),
  age_group = str_remove(agebyrace_group, paste0(race, " ")),
  age_group = case_when(
    age_group %in% c("15 to 17 years", "18 and 19 years", "20 to 24 years") ~ "15 to 24 years",
    age_group %in% c("25 to 29 years", "30 to 34 years") ~ "25 to 34 years",
    age_group %ni% c("25 to 29 years", "30 to 34 years", "15 to 17 years", "18 and 19 years", "20 to 24 years") ~ age_group
  )
  ) %>%
  group_by(FIPS, County, age_group, race)%>%
  summarise(pop = sum(pop, na.rm = T))%>%
  ungroup %>%
  mutate(lower_age = str_sub(age_group,1,2),
  lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
  age_group = str_remove(age_group," years"),
  upper_age = str_sub(age_group,-2,-1),
  upper_age = as.numeric(ifelse(upper_age %in% "er",100,upper_age)),
  upper_age = ifelse(upper_age==5,4,upper_age) ## What it actually should be (under 5 year old)
  )

# get county by age population totals
county_agebyrace_pop_ACS <- ct_ACS_agebyrace %>%
  group_by(County, lower_age, upper_age, race)%>%
  summarise(pop = sum(pop, na.rm = T))%>%
  ungroup

# Get county level incidence data
county_incidence_agebyrace <- read.delim("data/health/mortality/CDC_wonder/2018to23/county_race_age10yr.txt") %>%
  mutate(age_group = ifelse(Ten.Year.Age.Groups %in% c("< 1 year", "1-4 years"), "Under 5 years", Ten.Year.Age.Groups))%>%
  filter(age_group != "NS", Notes != "Total")%>% drop_na(County.Code) %>%
  group_by(County, age_group, Single.Race.6.Code, Hispanic.Origin)%>%
  summarise(Deaths = sum(Deaths, na.rm = T))%>%
  ungroup %>%
  mutate(race = case_when(
    Single.Race.6.Code == "NHOPI" ~ "Hawaiian and PI Alone",
    Single.Race.6.Code == "A" ~ "Asian alone",
    Single.Race.6.Code == "M" ~ "Multi Racial",
    Single.Race.6.Code == "1002-5" ~ "Black or African American Alone",
    Single.Race.6.Code == "2054-5" ~ "American Indian and Alaska Native",
    Single.Race.6.Code == "2106-3" ~ "White NH",
    Single.Race.6.Code %ni% c("2106-3", "2054-5", "1002-5", "M", "A", "NHOPI") ~ "Other Race Alone"
  ),
         race = ifelse(Hispanic.Origin == "Hispanic or Latino", Hispanic.Origin, race),
         County = str_remove(County, " County, UT"))%>%
  filter(County %in% county_agebyrace_pop_ACS$County)%>%
  mutate(age_group = str_replace(age_group, "-", " to "),
         age_group = str_remove(age_group," years"),
         lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(lower_age == 85,100,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age) ## What it actually should be (under 5 year old)
  ) %>%
  dplyr::full_join(county_agebyrace_pop_ACS, by = c("County", "race", "lower_age", "upper_age"))%>%
  mutate(Deaths_annual = Deaths/6,
         incidence_rate = Deaths_annual/pop)%>% filter(is.finite(incidence_rate)) %>%
  select(County, upper_age, lower_age, race, incidence_rate)

# match back to census tracts
ct_incidence_mortality <- ct_ACS_agebyrace %>%
  full_join(county_incidence_agebyrace, by = c("lower_age", "upper_age", "race", "County"))%>%
  mutate(incidence_rate = tidyr::replace_na(incidence_rate, 0),
         incidence_rate_daily = incidence_rate/365,
         endpoint = "Mortality, All Cause")



write.csv(ct_incidence_mortality, file = "processed/ct_incidence_mortality_race.csv", row.names = FALSE)

