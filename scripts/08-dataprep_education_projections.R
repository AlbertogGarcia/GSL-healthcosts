# GSL dust costs: Mortality and population projections
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
               ggplot2
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### School Loss Days
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ACS_year_final = 2023

# Projected county-level population growth by age

# Match age groups to ACS and CDC data
county_pop_projections <- readxl::read_excel("data/population/GardnerPI_projections.xlsx", sheet = 2) %>%
  select(Geography, Year, 
         "Population Ages 5-17"
  )%>% 
  pivot_longer(`Population Ages 5-17`, names_to = "age_group", values_to = "pop")%>%
  rename(County = Geography)%>%
  mutate(age_group = str_replace(str_remove(age_group, "Population Ages "), "-", " to "),
         start.age = str_sub(age_group,1,2),
         end.age = ifelse(start.age == 85, 100, str_sub(age_group,-2,-1)),
         County = str_remove(County, " County")
  )%>%
  filter(Year >= ACS_year_final)%>%
  group_by(County, age_group)%>%
  mutate(growth_rate = ifelse(Year == ACS_year_final, 0, (pop/lag(pop))-1))%>%
  mutate(cum_growth = cumprod(1 + growth_rate))%>%
  ungroup%>%
  select(County, Year, start.age, end.age, cum_growth)%>%
  mutate_at(vars(start.age, end.age), ~ as.double(.))




ct_school <- read.csv("processed/ct_school.csv", stringsAsFactors =  FALSE)

ct_school_projections <- ct_school %>%
  fuzzyjoin::fuzzy_left_join(county_pop_projections,
                             by = c("County" = "County",
                                    "lower_age" = "start.age" ,
                                    "upper_age" = "end.age"),
                             match_fun = list(`==`,`>=`, `<=`))%>%
  mutate(County = dplyr::coalesce(County.x,County.y),
         pop = pop*cum_growth)%>%
  select(-County.x,-County.y, -cum_growth, -start.age, -end.age)

write.csv(ct_school_projections, file = "processed/ct_school_projections.csv", row.names = FALSE)

