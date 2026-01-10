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
#### School loss days
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# census-tract population demographics (by age)

age_sheet = 3
ct_ACS_age_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = age_sheet)

ct_ACS_5to17 <- ct_ACS_age_wide %>%
  select(-`Total population`)%>%
  mutate(`15 to 17 years` = `Under 18 years` - `Under 5 years` - `5 to 9 years` - `10 to 14 years`)%>%
  pivot_longer(`Under 5 years`:ncol(.), names_to = "age_group", values_to = "pop")%>%
  filter(age_group %in% c("15 to 17 years", 
                          "5 to 9 years", 
                          "10 to 14 years"
                          )
  )%>%
  mutate(lower_age = str_sub(age_group,1,2),
         age_group = str_remove(age_group," years"),
         upper_age = str_sub(age_group,-2,-1),
         )

# get county by age population totals
county_age_pop_ACS <- ct_ACS_age %>%
  group_by(County, age_group, lower_age, upper_age)%>%
  summarise(pop = sum(pop, na.rm = T))


agebyrace_sheet = 11
ct_ACS_agebyrace_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = agebyrace_sheet)

ct_ACS_race_5to17 <- ct_ACS_agebyrace_wide %>%
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
         age_group = str_remove(agebyrace_group, paste0(race, " "))
  )%>%
  filter(age_group %in% c("15 to 17 years", 
                          "5 to 9 years", 
                          "10 to 14 years")
  )%>%
  mutate(lower_age = str_sub(age_group,1,2),
         age_group = str_remove(age_group," years"),
         upper_age = str_sub(age_group,-2,-1),
  )%>%
  select(- agebyrace_group)








# Read in school district shapefiles
districts.shp <- read_sf("data/gis/UtahSchoolDist/SchoolDistricts.shp") %>% select(NAME, TOTENROLL) %>%
  mutate(NAME = str_remove(tolower(NAME), " county"))
# read in census tract shapefiles
tracts.shp <- read_sf("data/gis/tracts/CensusTracts2020.shp")%>%
  rename(FIPS = GEOID20)%>%select(FIPS)


# Read in absence rates and merge with cts
attendance <- readxl::read_excel("data/schools/attendance23.xlsx", sheet = 5) %>%
  filter(Measure == "Attendance Rate", `LEA Type` == "District", LEA_name != "Utah Schools for Deaf & Blind") %>%
  select("LEA_name", `K-12 Overall`, 
         "AfAm/Black", "American Indian", "Asian", "Hispanic/Latino", "Multiple Race", "Pacific Islander", "White") %>%
  
  pivot_longer(cols = `K-12 Overall`:`White`, names_to = "Race", values_to = "Attendance Rate")%>%
  mutate(`School Loss Days` = 180 - `Attendance Rate`*180,
         incidence_rate_daily = `School Loss Days`/365,
         NAME = str_remove(tolower(LEA_name), " district"))%>%
  filter(`School Loss Days` != 180) %>%
  drop_na("Attendance Rate")%>%
  select(-`Attendance Rate`, -LEA_name, -`School Loss Days`)


jw <- comparator::JaroWinkler()

ct_attendance <- tracts.shp %>%
  st_centroid() %>%
  st_join(districts.shp) %>%
  fuzzyjoin::fuzzy_full_join(
    attendance, 
    by = "NAME",
    match_fun = function(x, y) { jw(x, y) > 0.1}
  )%>%
  mutate(jw = jw(NAME.x, NAME.y),
         FIPS = as.numeric(FIPS)) %>%
  group_by(NAME.x) %>%
  filter(jw == max(jw)) %>%
  ungroup %>%
  rename(District = NAME.y,
         District_enrollment = TOTENROLL) %>%
  select(FIPS, District, Race, District_enrollment, incidence_rate_daily) %>%
  st_drop_geometry() %>%
  mutate(race = case_when(
    str_detect(Race, "White") ~ "White NH",
    str_detect(Race, "Hispanic") ~ "Hispanic or Latino",
    str_detect(Race, "American Indian") ~ "American Indian and Alaska Native Alone",
    str_detect(Race, "Black") ~ "Black or African American Alone",
    str_detect(Race, "Asian") ~ "Asian alone",
    str_detect(Race, "Pacific Islander") ~ "Hawaiian and PI Alone",
    str_detect(Race, "Multi") ~ "Multi Racial"
  )
  )

ct_school <- ct_attendance %>%
  filter(Race == "K-12 Overall") %>%
  select(-Race, -race) %>%
  right_join(ct_ACS_5to17, by = "FIPS")

ct_school_race <- ct_attendance %>%
  mutate(race = ifelse(race == "K-12 Overall", "Other Race Alone", race)) %>%
  right_join(ct_ACS_race_5to17, by = c("FIPS", "race"))%>%
  mutate(Race = ifelse(race == "Other Race Alone", "Other Race", Race))%>%
  select(-race)

table(ct_school_race$Race)

write.csv(ct_school, file = "processed/ct_school.csv", row.names = FALSE)
write.csv(ct_school_race, file = "processed/ct_school_race.csv", row.names = FALSE)

