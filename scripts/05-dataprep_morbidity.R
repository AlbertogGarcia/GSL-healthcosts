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
  rename(upper_age = End.Age,
         lower_age = Start.Age); str(incidence_ut)
colnames(morbidity_incidence_ut) <- tolower(colnames(morbidity_incidence_ut))

table(morbidity_incidence_ut$endpoint)
table(morbidity_incidence_ut$endpoint.group)

table(morbidity_incidence_ut$upper_age)
table(morbidity_incidence_ut$lower_age)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### census-tract population demographics (by age)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

age_sheet = 3
ct_ACS_age_wide <- readxl::read_excel("data/population/Demographic_Raw_Tables.xlsx", sheet = age_sheet) %>%
  select(-`Total population`)
colnames(ct_ACS_age_wide)

ct_ACS_age <- ct_ACS_age_wide %>%
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



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#####Fuzzy join (of incidence to pop)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ct_incidence_ca <- temp_ct_ca %>%
  fuzzyjoin::fuzzy_left_join(
    temp_incidence_ca,
    by = c("county" = "county",
           "lower_age" = "start.age" ,
           "upper_age" = "end.age"),
    match_fun = list(`==`,`>=`, `<=`))%>%
  mutate(county = coalesce(county.x,county.y))%>%
  select(-county.x,-county.y)%>%
  fuzzyjoin::fuzzy_left_join(
    temp_incidence_ca,
    by = c("lower_age" = "start.age" ,
           "upper_age" = "end.age",
           "county"="county"),
    match_fun = list(`<=`, `>=`, `==`))%>%
  mutate(start.age = coalesce(start.age.x,start.age.y),
         end.age = coalesce(end.age.x,end.age.y),
         county = coalesce(county.x,county.y),
         value = coalesce(value.x,value.y))%>%
  select(-start.age.x,-start.age.y,-end.age.x, -end.age.y,
         -county.x,-county.y,-value.x,-value.y)