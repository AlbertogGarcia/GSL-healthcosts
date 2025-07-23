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
#### MORTALITY
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ACS_year_final = 2023

# Projected county-level population growth by age

gardner_pop_projections <- readxl::read_excel("data/population/GardnerPI_projections.xlsx", sheet = 2)

county_pop_projections <- gardner_pop_projections %>%
  pivot_longer(8:13, , names_to = "age_group", values_to = "pop")%>%
  select(Geography, Year, age_group, pop)%>%
  rename(County = Geography)%>%
  mutate(age_group = str_replace(str_remove(age_group, "Population Ages "), "-", " to "),
         lower_age = str_sub(age_group,1,2),
         upper_age = ifelse(lower_age == 85, 100, str_sub(age_group,-2,-1)),
         County = str_remove(County, " County")
  )%>%
  filter(Year >= ACS_year_final)%>%
  group_by(County, age_group)%>%
  mutate(growth_rate = ifelse(Year == ACS_year_final, 0, (pop/lag(pop))-1))%>%
  mutate(cum_growth = cumprod(1 + growth_rate))%>%
  ungroup%>%
  select(County, Year, lower_age, upper_age, growth_rate, cum_growth)%>%
  mutate_at(vars(lower_age, upper_age), ~ as.double(.))

gardner_pop_projections %>%
  mutate("Under 18" = `Population Ages 0-4` + `Population Ages 5-17`,
         "18 to 64" = `Population Ages 18-24` + `Population Ages 25-64`,
         "Over 65" = `Population Ages 65-84` + `Population Ages 85+`)%>%
  select(Year, "Under 18", "18 to 64", "Over 65")%>%
  pivot_longer(2:4, , names_to = "age_group", values_to = "pop")%>%
  filter(Year >= ACS_year_final)%>%
  mutate(lower_age = case_when(
    age_group == "Under 18" ~ 0,
    age_group == "18 to 64" ~ 18,
    age_group == "Over 65" ~ 65
  ))%>%
  group_by(age_group, lower_age, Year)%>%
  summarise(pop = sum(pop, na.rm = T))%>%
  mutate(growth_rate = ifelse(Year == ACS_year_final, 0, (pop/lag(pop))-1),
         cum_growth = cumprod(1 + growth_rate))%>%
  ungroup %>%
  ggplot(aes(x = Year, y = cum_growth, color = reorder(age_group, lower_age)))+
  geom_point() +
  geom_line()+
  scale_color_manual(values = c(palette$green, palette$blue, palette$red))+
  ylab("Population growth index") + ggtitle("Normalized Utah population projection by age group") +
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")


# census-tract population demographics (by age)

age_sheet = 3
ct_ACS_age_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = age_sheet)

ct_ACS_age <- ct_ACS_age_wide %>%
  select(-`Total population`)%>%
  mutate("5 to 17 years" = `Under 18 years` - `Under 5 years`,
         "18 to 24 years" = `20 to 24 years` + `15 to 19 years` + `10 to 14 years` + `5 to 9 years`- `5 to 17 years`,
         "25 to 64 years" = `25 to 34 years` + `35 to 44 years` + `45 to 54 years` + `55 to 59 years` + `60 to 64 years`,
         "65 to 84 years" = `65 to 74 years` + `75 to 84 years`
         )%>%
  pivot_longer(`Under 5 years`:ncol(.), names_to = "age_group", values_to = "pop")%>%
  filter(age_group %in% c("Under 5 years",
                          "5 to 17 years", 
                          "18 to 24 years", 
                          "25 to 64 years",
                          "65 to 84 years",
                          "85 and over")
  )%>%
  mutate(lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         age_group = str_remove(age_group," years"),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",100,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age) )## What it actually should be (under 5 year old)


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
         County %in% county_age_pop_ACS$County
  )%>%
  full_join(county_age_pop_ACS, by = c("County"), relationship = "many-to-many")%>%
  filter(as.numeric(age) <= upper_age & as.numeric(age) >= lower_age)%>%
  group_by(County, upper_age, lower_age, pop)%>%
  summarise(Deaths = sum(Deaths, na.rm = T))%>%
  mutate(Deaths_annual = Deaths/6,
         incidence_rate = Deaths_annual/pop)%>%
  ungroup %>%
  select(-pop, -Deaths, -Deaths_annual)


# match back to census tracts
ct_incidence_projections <- ct_ACS_age %>%
  left_join(county_incidence_age1yr, by = c("lower_age", "upper_age", "County"))%>%
  mutate(incidence_rate = tidyr::replace_na(incidence_rate, 0),
         endpoint = "Mortality, All Cause") %>%
  left_join(county_pop_projections, by = c("County", "lower_age", "upper_age"), relationship = "many-to-many")%>%
  mutate(pop = pop*cum_growth)%>%
  filter(Year >= 2024)
  
  
write.csv(ct_incidence_projections, file = "processed/ct_incidence_projections.csv", row.names = FALSE)


