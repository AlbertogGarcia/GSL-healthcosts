# GSL dust costs: Mortality and population projections
# albert.garcia@utah.edu
# created: 06/20/2025
# updated: 

# Set up environment ########################################

# load or install necessary libraries. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(measurements,  # convert units easily
               tidyverse, # tidyverse
               stringr,
               fuzzyjoin,
               ggplot2,
               cowplot
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function


# CPI in 2006 and 2024
# (https://fred.stlouisfed.org/series/CPALTT01USA661S)
CPI_2024 <- 132.34870
CPI_2000 <- 72.65302
CPI_0024 <- CPI_2024/CPI_2000

income_elasticity = 0.4

age_vsl_data <- read.csv("data/health/mortality/Aldy_Viscusi_2008_Fig1Data.csv", stringsAsFactors = F)%>%
  mutate(age_vsl_2000 = vslcohort3,
         age_min  = ifelse(age > 18,age,0), 
         age_max = ifelse(age<62,age,100))%>%
  select(age_min,age_max,age_vsl_2000)%>%
  mutate(year=2024)

## Load real GDP per cap growth rates

gdp_growth <- read.csv("data/health/realGDPpercapita_growth.csv", stringsAsFactors = F)%>%
  drop_na(rGDPpc)%>%
  mutate(growth_rate = rGDPpc/100,
         year = ifelse(str_sub(observation_date,-2,-1) > 25, paste0("19", str_sub(observation_date,-2,-1)), paste0("20", str_sub(observation_date,-2,-1)))
         )%>%
  select(year,growth_rate)

growth_0024 <- gdp_growth %>%
  filter(year > 2000) %>%
  mutate(year = as.character(year),
         cum_growth = cumprod(1 + growth_rate)) %>% 
  filter(year == 2024) %>% 
  select(cum_growth) %>% 
  pull()

## Combine

age_vsl_2024 <- age_vsl_data%>%
  mutate(age_vsl_2024 = age_vsl_2000*CPI_0024*growth_0024^income_elasticity)

age_vsl_2024 %>%
  summarise(age_vsl_2024 = mean(age_vsl_2024))

age_vsl_2024 %>%
  mutate(age_min = ifelse(age_min == 0, 18, age_min))%>%
  ggplot(aes(x=age_min, y = age_vsl_2024))+
  geom_point(size = 0.75)+
  geom_line()+
  theme_classic(16)+
  labs(x= "Age", y = "2024 Cohort-Adjusted VSL (million 2024 $) from Aldy & Viscusi (2008)")

age_vsl_2024 %>%
  select(age_min, age_max,year,age_vsl_2024) %>%
  write.csv("processed/age_based_VSL_2019.csv", row.names = F)
