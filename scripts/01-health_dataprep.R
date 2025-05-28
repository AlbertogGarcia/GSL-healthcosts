# GSL dust costs: Mortality impacts data processing
# albert.garcia@utah.edu
# created: 05/28/2025
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

# Load and process data #####################################

# External data

#census tract age-population

ct_raw <- read.csv("data/census/nhgis0001_ts_geog2010_tract.csv", stringsAsFactors = FALSE); str(ct_raw)
ct_age_desc <- read.csv("data/census/age_group_desc.csv", stringsAsFactors = FALSE); str(ct_age_desc)

ct_utah <- ct_raw %>%
  `colnames<-`(tolower(colnames(ct_raw)))%>%
  filter(state == "Utah")%>%
  gather(age_group_raw, pop, cn5aa2000:cn5aw2010)%>%
  mutate(year = str_sub(age_group_raw,-4,-1),
         age_group_str = toupper(str_sub(age_group_raw,4,5)))%>%
  filter(year==2010)%>%
  left_join(ct_age_desc, by = c("age_group_str"="age_group_str"))%>%
  mutate(lower_age = str_sub(age_group_desc,1,2),
         lower_age = as.numeric(ifelse(lower_age %in% "Un",0,lower_age)),
         age_group_desc = str_remove(age_group_desc," years"),
         upper_age = str_sub(age_group_desc,-2,-1),
         upper_age = as.numeric(ifelse(upper_age %in% "er",99,upper_age)),
         year = as.integer(year))%>%
  select(-geogyear,-age_group_raw,-age_group_str); str(ct_utah)

age_group_ct <- ct_utah %>%
  mutate(upper_age = ifelse(upper_age==5,4,upper_age))%>% ## What it actually should be (under 5 year old)
  group_by(lower_age,upper_age)%>%
  summarize();age_group_ct


# Exported data from BenMAP

#County name to merged with BenMAP row/col county indicators

ct <- read_sf("data/health/benmap/County_def.shp")

county <- as.data.frame(cbind(ct$NAME, ct$STATE_NAME, ct$ROW, ct$COL), stringsAsFactors = F) 
colnames(county) <- c("county","state", "row", "col")

county$row <- as.integer(county$row)
county$col <- as.integer(county$col)

# Mortality incidence data (2015 baseline)

incidence_utah <- read.csv("data/health/benmap/Mortality Incidence (2015).csv", stringsAsFactors = F) %>%
  filter(Endpoint == "Mortality, All Cause") %>%
  select(-Endpoint.Group,-Race:-Ethnicity, -Type)%>%
  left_join(county, by = c("Column"="col","Row"="row"))%>%
  filter(state %in% "Utah"); str(incidence_utah)

colnames(incidence_ca) <- tolower(colnames(incidence_ca))

#Fuzzy join of mortality incidence and population by age-groups

gc()

##Remove extra columns to make join faster

# Mortality 

temp_incidence_utah <- incidence_utah %>%
  select(-endpoint,-column,-row,-state)%>%
  ungroup(); temp_incidence_utah

temp_ct_utah <- ct_utah%>%
  select(-state,-statea,-tracta, -countya,-age_group_desc)%>%
  mutate(county = str_remove(county, " County"))%>%
  ungroup(); temp_ct_utah

##Fuzzy join (of incidence to pop)

ct_incidence_utah <- temp_ct_utah %>%
  fuzzyjoin::fuzzy_left_join(
    temp_incidence_utah,
    by = c("county" = "county",
           "lower_age" = "start.age" ,
           "upper_age" = "end.age"),
    match_fun = list(`==`,`>=`, `<=`))%>%
  mutate(county = coalesce(county.x,county.y))%>%
  select(-county.x,-county.y)%>%
  fuzzyjoin::fuzzy_left_join(
    temp_incidence_utah,
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


write.csv(ct_incidence_utah,file = "data/processed/ct_incidence_utah.csv", row.names = FALSE)
ct_incidence_utah <- read.csv("data/processed/ct_incidence_utah.csv", stringsAsFactors =  FALSE)
