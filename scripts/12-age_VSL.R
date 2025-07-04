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
  mutate(age_vsl_2000 = vslcohort3)%>%
  select(age, age_vsl_2000)%>%
  right_join(
    data.frame(age = seq(from = 0, 100, by = 1))
  )%>%
  mutate(age_vsl_2000 = case_when(age < 18 ~ 3.388772,
                                  age > 62 ~ 5.091287,
                                  age >= 18 & age <= 62 ~ age_vsl_2000
  ),
         year=2024)

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

age_vsl_2024 <- age_vsl_data %>%
  mutate(age_vsl_2024 = age_vsl_2000*CPI_0024*growth_0024^income_elasticity)

age_vsl_2024 %>%
  filter(age >= 18 & age <= 62) %>%
  summarise(age_vsl_2024 = mean(age_vsl_2024))



age_vsl_2024 %>%
  mutate(vsl_2024 = 12.57222) %>%# EPA VSL - non-age-varying
  pivot_longer(cols = c("vsl_2024", "age_vsl_2024"), values_to = "VSL", names_to = "method")%>%
  filter((age >= 18 & age <= 62) | (method == "vsl_2024" & age <= 80)) %>%
  ggplot(aes(x=age, y = VSL, color = method))+
  geom_line(linewidth = 1.25)+
  theme_classic(16)+
  theme(legend.title = element_blank(),
        legend.position = "bottom")+
  scale_color_manual(labels = c("Cohort-adjusted\n(Aldy & Viscusi 2008)", "EPA-recommended"),
                     values = c("#A23B72", "#2E86AB")
                     )+
  labs(x= "Age", y = "VSL (million 2024 $)")

age_vsl_2024 %>%
  select(age, year ,age_vsl_2024) %>%
  write.csv("processed/age_based_VSL_2024.csv", row.names = F)
