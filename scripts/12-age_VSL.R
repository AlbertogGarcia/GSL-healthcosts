
income_elasticity_mort = 0.4

age_vsl_data <- fread("./Aldy & Viscusi 2008 Figure 1 Data.csv", stringsAsFactors = F)%>%
  mutate(age_vsl_2000 = vslcohort3,
         age_min  = age, 
         age_max = ifelse(age<62,age,100))%>%
  select(age_min,age_max,age_vsl_2000)%>%
  mutate(year=2024)

## Load real GDP per cap growth rates

gdp_growth <- fread("./realGDPpercapita_growth.csv", stringsAsFactors = F)%>%
  drop_na(GDP_per_cap_2017)%>%
  mutate(growth_rate = rGDPpc,
         year = as.integer(str_sub(observation_date,1,4)))%>%
  select(year,growth_rate)

growth_rates <- gdp_growth %>%
  filter(year>2000)%>%
  mutate(cum_growth = cumprod(1 + growth_rate))

## Combine

age_vsl_2019 <- age_vsl_data%>%
  left_join(growth_rates, by = c("year" = "year"))%>%
  mutate(
    age_VSL_2024 = future_WTP(
      income_elasticity_mort,
      (cum_growth - 1),
      age_vsl_2000)*1.484654)

age_vsl_2024 %>%
  summarise(age_vsl_2024 = mean(age_vsl_2024))

age_vsl_2024 %>%
  ggplot(aes(x=age_min, y = age_vsl_2024/1000000))+
  geom_point()+
  geom_line()+
  theme_cowplot(16)+
  labs(x= "Age", y = "2024 Cohort-Adjusted VSL (million 2024 $) from Aldy & Viscusi (2008)")

age_vsl_2024%>%
  select(age_min, age_max,year,age_vsl_2024)%>%
  write.csv("processed/age_based_VSL_2019.csv", row.names = F)
