

# CPI in 2006 and 2024
# (https://fred.stlouisfed.org/series/CPALTT01USA661S)
CPI_2024 <- 132.34870
CPI_2006 <- 85.05367
CPI_0624 <- CPI_2024/CPI_2006

# Real GDP in 2006 and 2024
# https://fred.stlouisfed.org/series/GDPCA
GDP_2024 <- 23305.023
GDP_2006 <- 16433.148
growth_0624 <- GDP_2024/GDP_2006

# US EPA recommended VSL (2013 dollars) and income elasticity  
VSL_2006 <- 7.4
income_elasticity <- 0.4 
# Note: EPA adjusts for changes in income using an income elasticity of 0.4,
# which is lower than other agencies, 
# but measuring income with per-capita GDP, which has grown much faster than the measure of median earnings used by other agencies.

# Formula:
VSL_24 = VSL_2006*CPI_0624*growth_0624^income_elasticity

# How does that compare to simply adjusting for inflation
# VSL_24_inflationonly = VSL_2006*CPI_0624

# Compare to Thivierge's code
# VSL_test = (income_elasticity * (growth_0624-1) * VSL_2006 + VSL_2006)*CPI_0624



