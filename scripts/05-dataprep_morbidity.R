# GSL dust costs: asthma impacts
# albert.garcia@utah.edu
# created: 05/28/2025
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
               readxl,
               cowplot
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Exported data from BenMAP
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#County name to merged with BenMAP row/col county indicators

ct <- read_sf("data/benmap/raw/County_def.shp")%>%
  filter(STATEFP == 49)

county <- as.data.frame(cbind(ct$NAME, ct$ROW, ct$COL), stringsAsFactors = F) 
colnames(county) <- c("county","row", "col")

county$row <- as.integer(county$row)
county$col <- as.integer(county$col)

# Mortality incidence data (2015 baseline)
morbidity_incidence <- read.csv("data/benmap/morbidity_incidence_2014.csv", stringsAsFactors = F)

morbidity_incidence %>%
  select(Row, Column) %>%
  distinct() %>% nrow()

morbidity_incidence_ut <- morbidity_incidence %>%
  select(Endpoint, Endpoint.Group, Start.Age, End.Age, Column, Row, Value)%>%
  right_join(county, by = c("Column"="col","Row"="row"))%>%
  rename(end.age = End.Age,
         start.age = Start.Age); str(morbidity_incidence_ut)
colnames(morbidity_incidence_ut) <- tolower(colnames(morbidity_incidence_ut))

table(morbidity_incidence_ut$endpoint)
table(morbidity_incidence_ut$endpoint.group)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### census-tract population demographics (by age)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

age_sheet = 3
ct_ACS_age_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = age_sheet) %>%
  select(-`Total population`)
colnames(ct_ACS_age_wide)

ct_ACS_age <- ct_ACS_age_wide %>%
  mutate(`18 to 19 years` = `Under 5 years` + `5 to 9 years` + `10 to 14 years` + `15 to 19 years` - `Under 18 years`,
         `15 to 17 years` = `15 to 19 years` - `18 to 19 years`,
         )%>%
  pivot_longer(`Under 5 years`:ncol(.), names_to = "age_group", values_to = "pop")%>%
  filter(age_group %ni% c("16 years and over", 
                          "18 years and over", 
                          "21 years and over",
                          "65 years and over",
                          "62 years and over",
                          "Under 18 years"
                          , "15 to 19 years"
  )
  )%>%
  mutate(lower_age = str_sub(age_group,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         age_group = str_remove(age_group," years"),
         upper_age = str_sub(age_group,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",99,upper_age)),
         upper_age = ifelse(upper_age==5,4,upper_age) ## What it actually should be (under 5 year old)
  )


table(ct_ACS_age$age_group)

# Add other relevant all-age incidence rates

incidence_BenMap_additional <- expand_grid("age_group" = unique(ct_ACS_age$age_group), 'county' = unique(ct_ACS_age$County)) %>%
  left_join(
    (ct_ACS_age %>% select(lower_age, upper_age, age_group) %>% distinct()),
            by = "age_group")%>%
  mutate(`Out of Hospital Cardiac Arrest (OHCA)` = case_when(
    county == "Salt Lake" & lower_age >= 18 ~ 76/100000/365,
    upper_age < 18 ~ 0.00000002,
    county != "Salt Lake" & lower_age >= 18 & upper_age <= 44 ~ 0.00000009,
    county != "Salt Lake" & lower_age >= 18 & upper_age <= 64 ~ 0.00000056,
    county != "Salt Lake" & lower_age >= 18 & upper_age > 64 ~ 0.00000133
    ),
    `Stroke` = ifelse(lower_age >= 65, 0.00446, 0),
    #`School Loss Days` = ifelse(upper_age < 18 & lower_age >= 5, 9.9/365, 0),
    `Work Loss Days` = case_when(
      age_group %in% c("18 to 19", "20 to 24") ~ 0.00540,
      lower_age >= 25 & upper_age <= 44 ~ 0.00678,
      lower_age >= 45 & upper_age <= 64 ~ 0.00492,
      upper_age < 18 | upper_age > 64 ~ 0,
    )
  )%>%
  rename(end.age = upper_age,
         start.age = lower_age) %>%
  pivot_longer(cols = `Out of Hospital Cardiac Arrest (OHCA)`:`Work Loss Days`, names_to = "endpoint", values_to = "value")%>%
  select(-age_group)

morbidity_incidence_ut <- morbidity_incidence_ut %>%
  select(start.age, end.age, endpoint, county, value) %>%
  rbind(incidence_BenMap_additional)



morbidity_parameters <- read_excel("data/health/morbidity_parameters.xlsx") %>%
  select(-reference) %>%
  filter(endpoint != "School Loss Days")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#####Fuzzy join (of incidence to pop)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

relevant_endpoints <- unique(morbidity_parameters$endpoint)

ct_incidence_ut <- data.frame()

for(i in relevant_endpoints){
  
  morbidity_incidence_temp <- morbidity_incidence_ut %>%
    filter(endpoint %in% i)
  
  these_morbidity_parameters <- morbidity_parameters %>%
    filter(endpoint == i) %>%
    select(-endpoint)
  
  print(i)
  
  this_incidence <- ct_ACS_age %>%
    rename(county = County) %>%
    fuzzyjoin::fuzzy_left_join(morbidity_incidence_temp,
                               by = c("county" = "county",
                                      "lower_age" = "start.age" ,
                                      "upper_age" = "end.age"),
                               match_fun = list(`==`,`>=`, `<=`))%>%
    mutate(county = dplyr::coalesce(county.x,county.y))%>%
    select(-county.x,-county.y)%>%
    fuzzyjoin::fuzzy_left_join(
      morbidity_incidence_temp,
      by = c("lower_age" = "start.age" ,
             "upper_age" = "end.age",
             "county"="county"
      ),
      match_fun = list(`<=`, `>=`, `==`))%>%
    mutate(endpoint = coalesce(endpoint.x,endpoint.y),
           County = coalesce(county.x,county.y),
           value = coalesce(value.x,value.y)) %>%
    select(FIPS:upper_age, County, endpoint, value) %>%
    fuzzyjoin::fuzzy_left_join(these_morbidity_parameters,
                               by = c("lower_age" = "lower_age" ,
                                      "upper_age" = "upper_age"),
                               match_fun = list(`>=`, `<=`))%>%
    mutate(lower_age = coalesce(lower_age.x,lower_age.y),
           upper_age = coalesce(upper_age.x,upper_age.y)) %>%
    select(-lower_age.x, -lower_age.y, -upper_age.x, -upper_age.y)%>%
    mutate(beta = case_when(
      parameter == "RR" ~ log(parameter_value)/dose,
      parameter == "OR" ~ log((parameter_value / (1 - value + (value*parameter_value))))/dose,
      parameter == "pct" ~ parameter_value/100/dose
    ))%>%
    drop_na(beta) %>%
    select(-c(parameter:dose))%>%
    pivot_wider(id_cols = FIPS:value, names_from = "pollutant", values_from = "beta")%>%
    rename(beta_pm25 = pm2.5,
           beta_pm10 = pm10)
  
  ct_incidence_ut <- ct_incidence_ut %>%
    rbind(this_incidence)
  
}


write.csv(ct_incidence_ut, file = "processed/ct_incidence_morbidity.csv", row.names = FALSE)



