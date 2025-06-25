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
  pivot_longer(`Under 5 years`:ncol(.), names_to = "age_group", values_to = "pop")%>%
  filter(age_group %ni% c("16 years and over", 
                          "18 years and over", 
                          "21 years and over",
                          "65 years and over",
                          "62 years and over",
                          "Under 18 years"))%>%
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
county_incidence_age1yr <- read.delim("data/health/mortality/CDC_wonder/2018to23/county_age1yr.txt") %>%
  rename(age = Single.Year.Ages.Code)%>%
  filter(age != "NS")%>%
  mutate(County = str_remove(County, " County, UT"))%>%
  filter(Notes != "Total",
         County %in% county_pop$County
         )%>%
  full_join(county_age_pop_ACS, by = c("County"), relationship = "many-to-many")%>%
  filter(as.numeric(age) <= upper_age & as.numeric(age) >= lower_age)%>%
  group_by(County, upper_age, lower_age, pop)%>%
  summarise(Deaths = sum(Deaths, na.rm = T))%>%
  mutate(Deaths_annual = Deaths/6,
         incidence_rate = Deaths_annual/pop)%>%
  ungroup %>%
  select(-pop, -Deaths, -Deaths_annual)

# check comarability to 2020 data - Looks good!
county_incidence_age1yr_2020 <- read.delim("data/health/mortality/CDC_wonder/2020/county_age1yr.txt") %>%
  rename(age = Single.Year.Ages.Code)%>%
  filter(age != "NS")%>%
  mutate(County = str_remove(County, " County, UT"))%>%
  filter(Notes != "Total",
         County %in% county_pop$County
  )%>%
  full_join(county_age_pop_ACS, by = c("County"), relationship = "many-to-many")%>%
  filter(as.numeric(age) <= upper_age & as.numeric(age) >= lower_age)%>%
  group_by(County, upper_age, lower_age, pop)%>%
  summarise(Deaths = sum(Deaths, na.rm = T))%>%
  mutate(incidence_rate_2020 = Deaths/pop)%>%
  ungroup %>%
  select(-pop, -Deaths)%>%
  full_join(county_incidence_age1yr, by = c("County", "lower_age", "upper_age"))

# match back to census tracts
ct_incidence_mortality <- ct_ACS_age %>%
  full_join(county_incidence_age1yr, by = c("lower_age", "upper_age", "County"))%>%
  mutate(incidence_rate = replace_na(incidence_rate, 0),
         endpoint = "Mortality, All Cause")



write.csv(ct_incidence_mortality, file = "processed/ct_incidence_mortality.csv", row.names = FALSE)
ct_incidence_mortality <- read.csv("processed/ct_incidence_mortality.csv", stringsAsFactors =  FALSE)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### ASTHMA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# dc20_sheet = 1
# ct_dc20 <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = dc20_sheet)%>%
#   mutate_at(vars(4:ncol(.)), ~ ./`Total:`)%>%
#   select(-`Total:`)

ct_incidence_asthma <- read.csv("data/health/asthma/CDC_places/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2024_release_20250209.csv")%>%
  rename(FIPS = LocationName,
         County = CountyName,
         incidence_rate = Data_Value,
         pop = TotalPopulation,
         pop_over18 = TotalPop18plus)%>%
  filter(MeasureId == "CASTHMA")%>%
  select(FIPS, pop, pop_over18, incidence_rate)%>%
  mutate(pop_under18 = pop - pop_over18,
         endpoint = "Asthma",
         incidence_rate = incidence_rate/100)

write.csv(ct_incidence_asthma, file = "processed/ct_incidence_asthma.csv", row.names = FALSE)
ct_incidence_asthma <- read.csv("processed/ct_incidence_asthma.csv", stringsAsFactors =  FALSE)









