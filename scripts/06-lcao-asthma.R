

# CPI in 2020 and 2024
# (https://fred.stlouisfed.org/series/CPALTT01USA661S)
CPI_2024 <- 132.34870
CPI_2020 <- 109.19520
CPI_2024 <- CPI_2024/CPI_2020

# Real GDP in 2020 and 2024
# https://fred.stlouisfed.org/series/GDPCA
GDP_2024 <- 23305.023
GDP_2020 <- 20267.585
growth_GDP_2024 <- GDP_2024/GDP_2020

# Maniloff and Fan (2023) and income elasticity  
LCAO_2020 <- 182681
#
income_elasticity_child <- 0.4
income_elasticity_adult <- 0.3

LCAO_24_child = LCAO_2020*CPI_2024*growth_GDP_2024^income_elasticity_child
LCAO_24_adult = LCAO_2020*CPI_2024*growth_GDP_2024^income_elasticity_adult



LCAO_24_justinflation = LCAO_2020*CPI_2024
