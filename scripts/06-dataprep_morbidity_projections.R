# GSL dust costs: Morbidity and population projections
# albert.garcia@utah.edu
# created: 01/07/2026
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
#### MORTALITY
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ACS_year_final = 2023

ct_incidence_morbidity <- read.csv("processed/ct_incidence_morbidity.csv", stringsAsFactors =  FALSE)
table(ct_incidence_morbidity$age_group)

# Projected county-level population growth by age
gardner_pop_projections <- readxl::read_excel("data/population/GardnerPI_projections.xlsx", sheet = 2)
colnames(gardner_pop_projections)


# Match age groups to ACS and CDC data
county_pop_projections <- gardner_pop_projections %>%
  mutate("Population Ages 5-24" = `Population Ages 5-17` + `Population Ages 18-24`)%>%
  select(Geography, Year, "Population Ages 0-4":"Population Ages 85+"
  )%>% 
  pivot_longer(`Population Ages 0-4`:ncol(.), , names_to = "age_group", values_to = "pop")%>%
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

ct_incidence_projections <- data.frame()
for(e in unique(ct_incidence_morbidity$endpoint)){
  
  print(e)
  ct_incidence_projections <- ct_incidence_morbidity %>%
    filter(endpoint == e) %>%
    fuzzyjoin::fuzzy_left_join(county_pop_projections,
                               by = c("County" = "County",
                                      "lower_age" = "start.age" ,
                                      "upper_age" = "end.age"),
                               match_fun = list(`==`,`>=`, `<=`))%>%
    mutate(County = dplyr::coalesce(County.x,County.y),
           pop = pop*cum_growth)%>%
    select(-County.x,-County.y, -cum_growth, -start.age, -end.age) %>%
    rbind(ct_incidence_projections)
  
}

write.csv(ct_incidence_projections, file = "processed/ct_incidence_morbidity_projections.csv", row.names = FALSE)

