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
               ggplot2,
               zoo # needed for interpolation of future adjusted mortality incidence
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
                "purple" = "#8da0cb"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### MORTALITY
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ACS_year_final = 2023

# Projected county-level population growth by age

gardner_pop_projections <- readxl::read_excel("data/population/GardnerPI_projections.xlsx", sheet = 2)

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
  theme_classic(12)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(filename = "age_projections.png", path = "figs",
       height = 5, width = 6)

# Match age groups to ACS and CDC data
county_pop_projections <- gardner_pop_projections %>%
  mutate("Population Ages 5-24" = `Population Ages 5-17` + `Population Ages 18-24`)%>%
  select(Geography, Year, 
         "Population Ages 0-4", 
         "Population Ages 5-24", 
         "Population Ages 25-64", 
         "Population Ages 65-84", 
         "Population Ages 85+"
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




ct_incidence_mortality <- read.csv("processed/ct_incidence_mortality.csv", stringsAsFactors =  FALSE) %>%
  filter(endpoint == "Mortality, All-cause")
table(ct_incidence_mortality$age_group)

ct_incidence_projections <- ct_incidence_mortality %>%
  fuzzyjoin::fuzzy_left_join(county_pop_projections,
                             by = c("County" = "County",
                                    "lower_age" = "start.age" ,
                                    "upper_age" = "end.age"),
                             match_fun = list(`==`,`>=`, `<=`))%>%
  mutate(County = dplyr::coalesce(County.x,County.y),
         pop = pop*cum_growth)%>%
  select(-County.x,-County.y, -cum_growth, -start.age, -end.age)

write.csv(ct_incidence_projections, file = "processed/ct_incidence_projections.csv", row.names = FALSE)


# Projected Mortality adjustment factors
mortality_adjustmentfactors_2013 <- readxl::read_excel("data/health/mortality/census_mortality_adjustmentfactors_2013relative.xlsx")
mortality_adjustmentfactors <- mortality_adjustmentfactors_2013 %>%
  left_join(mortality_adjustmentfactors_2013 %>% filter (Year == 2025) %>% rename(base = Factor) %>% select(-Year)
            , by = c("start.age", "end.age")) %>%
  mutate(Factor = Factor/base) %>%
  select(-base) %>%
  group_by(start.age, end.age) %>%
  complete(Year = min(Year):max(Year)) %>% # Fills in missing years, creates NAs in 'value'
  mutate(Factor = na.approx(Factor)) %>% # Linearly interpolates the NA values
  ungroup

ct_incidence_projections_adj <- mortality_adjustmentfactors %>%
  fuzzy_left_join(
    unique(ct_incidence_projections[,c('lower_age','upper_age')]),
    by = c("start.age" = "lower_age",
           "end.age" = "upper_age"),
    match_fun = list(`>=`, `<=`))%>%
  fuzzyjoin::fuzzy_left_join(
    unique(ct_incidence_projections[,c('lower_age','upper_age')]),
    by = c("start.age" = "lower_age" ,
           "end.age" = "upper_age"),
    match_fun = list(`<=`, `>=`))%>%
  mutate(upper_age = coalesce(upper_age.x,upper_age.y)) %>%
  select(Year, Factor, upper_age) %>%
  right_join(ct_incidence_projections, by = c("Year", "upper_age")) %>%
  mutate(incidence_rate = incidence_rate*Factor,
         incidence_rate_daily = incidence_rate_daily*Factor) %>%
  filter(Year >= 2025) %>%
  select(names(ct_incidence_projections), everything())

#ct_incidence_projections_adj <- ct_incidence_projections_adj[, names(ct_incidence_projections)]

write.csv(ct_incidence_projections_adj, file = "processed/ct_incidence_projections_adj.csv", row.names = FALSE)

