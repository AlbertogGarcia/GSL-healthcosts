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

ACS_year = 2023
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

VSL_2024 = 12.57222

morbidity_valuations_2024 <- read.csv("data/health/morbidity_valuations_2024.csv", stringsAsFactors = F)

income_elasticity = 0.4
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CBO_projections <- readxl::read_excel("data/health/CBO_longterm_economic_2025.xlsx", sheet = 2)

gdp_growth_projected <- data.frame(Year = t(CBO_projections[7,2:ncol(CBO_projections)]),
                                   realGDPpc_growth = t(CBO_projections[13,2:ncol(CBO_projections)]),
                                   CPI_growth = t(CBO_projections[35,2:ncol(CBO_projections)])) %>%
  filter(Year > ACS_year)%>%
  mutate_at(vars(realGDPpc_growth, CPI_growth), ~ifelse(Year == 2024, 0, .))

rownames(gdp_growth_projected) <- NULL

through_2060 <- gdp_growth_projected %>%
  filter(Year >=2045) %>%
  summarise(CPI_growth  = mean(CPI_growth),
            realGDPpc_growth = mean(realGDPpc_growth))

growth_projected <- data.frame(Year = seq(2056, 2060),
                            CPI_growth = through_2060$CPI_growth,
                            realGDPpc_growth = through_2060$realGDPpc_growth) %>%
  rbind(gdp_growth_projected) %>%
  arrange(Year) %>%
  mutate(cum_growth_CPI = cumprod(1 + CPI_growth/100),
         cum_growth_GDP = cumprod(1 + realGDPpc_growth/100))
         
VSL_projected <- growth_projected %>%
  mutate(VSL_proj = VSL_2024*cum_growth_CPI*cum_growth_GDP^income_elasticity)%>%
  select(Year, VSL_proj)

morbidity_valuations_projected <- morbidity_valuations_2024 %>%
  select(Endpoint, COI_24) %>%
  slice(rep(row_number(), each = 2060-2024+1)) %>%
  mutate(Year = rep(2024:2060, times = nrow(morbidity_valuations_2024))) %>%
  full_join(growth_projected, by = "Year")%>%
  mutate(COI_proj = COI_24*cum_growth_CPI*cum_growth_GDP^income_elasticity)%>%
  select(Endpoint, Year, COI_proj)
  

write.csv(VSL_projected, file = "processed/VSL_projected.csv", row.names = FALSE)
write.csv(morbidity_valuations_projected, file = "processed/morbidity_valuations_projected.csv", row.names = FALSE)
