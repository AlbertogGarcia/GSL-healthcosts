# GSL dust costs: Lifetime costs of asthma onset
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
               ggplot2
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function




# CPI in 2020 and 2024
# (https://fred.stlouisfed.org/series/CPALTT01USA661S)
CPI_2024 <- 132.34870
CPI_2020 <- 109.19520
CPI_2024 <- CPI_2024/CPI_2020

gdp_growth <- read.csv("data/health/realGDPpercapita_growth.csv", stringsAsFactors = F)%>%
  drop_na(rGDPpc)%>%
  mutate(growth_rate = rGDPpc/100,
         year = ifelse(str_sub(observation_date,-2,-1) > 25, paste0("19", str_sub(observation_date,-2,-1)), paste0("20", str_sub(observation_date,-2,-1)))
  )%>%
  select(year,growth_rate)

growth_rates <- gdp_growth %>%
  filter(year>2020)%>%
  mutate(cum_growth = cumprod(1 + growth_rate))

growth_GDPpc_2024 <- growth_rates %>% filter(year == 2024) %>% select(cum_growth) %>% pull()


# Maniloff and Fan (2023) and income elasticity  
LCAO_2020 <- 182681
#
income_elasticity_child <- 0.4
income_elasticity_adult <- 0.3

LCAO_24_child = LCAO_2020*CPI_2024*growth_GDPpc_2024^income_elasticity_child
LCAO_24_adult = LCAO_2020*CPI_2024*growth_GDPpc_2024^income_elasticity_adult

