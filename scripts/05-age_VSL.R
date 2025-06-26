

# Start w/ VSL (2024)
VSL_24 = 13.24189

# discount rate
r = 0.03

median_age_utah = 31.7
median_life_expectancy_utah = 77.2

L_a = median_life_expectancy_utah - median_age_utah

VSLY_24 = (r*VSL_24)/(1-(1+r)^(-L_a))

