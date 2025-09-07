# GSL dust costs: Mortality impacts
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
               ggplot2,
               readxl,
               cowplot,
               ggpubr
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
                "sc1278" = "grey50", 
                "sc1280" = "#abd9e9",
                "sc1281" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

relevant_scenarios <- c(1275, 1277, 1278, 1280, 1281) # note, excludes 1282 baseline

scenario_pal <- c(palette$sc1275, palette$sc1277, palette$sc1278, palette$sc1280, palette$sc1281)

n_storms_data = 2
n_storms_annual = 3

# main VSL and age-based VSL
VSL_24 = 12.57222

age_based_VSL_2024 <- read.csv("processed/age_based_VSL_2024.csv")%>%
  select(age, age_vsl_2024)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load and merge processed data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#1 Emissions scenarios by water-level
scenario_pm_deltas <- read.csv("processed/scenario_pm_deltas_event.csv", stringsAsFactors =  FALSE)%>%
  filter(scenario %in% relevant_scenarios)

#2 Population and incidence
ct_incidence_projections <- read.csv("processed/ct_incidence_projections.csv", stringsAsFactors =  FALSE)

#Merge w/ pollution deltas 
ct_projections <- ct_incidence_projections %>%
  left_join(scenario_pm_deltas, by = "FIPS", relationship = "many-to-many")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Mortality impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Coefficients from Orellano et al., (2020)
###IMPORTANT: all are for a 10 mcrogram increase
RR_pm25 = 1.0065 # PM2.5 RR from a short-term exposure
beta_pm25 <- log(RR_pm25)/10

RR_pm10 = 1.0041
beta_pm10 <- log(RR_pm10)/10

#Mortality impact
ct_mortality_projections <- ct_projections %>%
  mutate(incidence_rate_event = incidence_rate_daily*event_days,
         mortality_pm10 = ((1-(1/exp(beta_pm10*pm10_delta)))*incidence_rate_event*pop)*(n_storms_annual/n_storms_data),
         mortality_pm25 = ((1-(1/exp(beta_pm25*pm25_delta)))*incidence_rate_event*pop)*(n_storms_annual/n_storms_data),
         mortality = mortality_pm10 + mortality_pm25,
         FV_costs_VSL = mortality*VSL_24,
         PV_costs_VSL = FV_costs_VSL/(1+0.03)^(Year - 2024)
         )%>%
  drop_na(scenario)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total overall mortality
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
total_mortality_projections <- ct_mortality_projections %>%
  group_by(scenario, Year) %>%
  summarise(mortality_pm10 = sum(mortality_pm10, na.rm = T),
            mortality_pm25 = sum(mortality_pm25, na.rm = T),
            mortality = sum(mortality, na.rm = T),
            costs_VSL = sum(PV_costs_VSL, na.rm = T))%>%
  ungroup %>%
  group_by(scenario) %>%
  mutate(PV_cum_costs = cumsum(costs_VSL),
         cum_mortality = cumsum(mortality))
  
# 
# total_mortality_projections_relative <- total_mortality_projections %>%
#   left_join(total_mortality_projections %>%
#           filter(scenario == baseline_scenario)%>%
#           rename(baseline_mortality_pm10 = mortality_pm10,
#                  baseline_mortality_pm25 = mortality_pm25,
#                  baseline_mortality_pm = mortality_pm,
#                  baseline_costs_VSL = costs_VSL)%>%
#           dplyr::select(-scenario)
#           , by = "Year"
#   )%>%
#   mutate(relative_mortality_pm10 = mortality_pm10 - baseline_mortality_pm10,
#          relative_mortality_pm25 = mortality_pm25 - baseline_mortality_pm25,
#          relative_mortality = mortality_pm - baseline_mortality_pm,
#          relative_costs_VSL = costs_VSL - baseline_costs_VSL)%>%
#   group_by(scenario)%>%
#   mutate(PV_cum_relative_costs = cumsum(relative_costs_VSL),
#          cum_relative_mortality = cumsum(relative_mortality))%>%
#   filter(Year <= 2050)%>%
#   select(scenario, Year, relative_mortality, cum_relative_mortality, relative_costs_VSL, PV_cum_relative_costs)

mortality_proj_annual <- total_mortality_projections %>%
  ggplot(aes(x = Year, y = mortality, color = as.character(scenario)))+
  geom_line()+
  geom_point(size = 1.5)+
  scale_y_continuous(name = "Annual mortality",
                     limits = c(0, 9),
                     breaks = seq(0, 10, by = 2)
                    ) +
  ggtitle("Annual all-cause mortality (2025-2060)")+
  scale_color_manual(name = "GSL water level (mASL)", values = scenario_pal)+
  theme_cowplot(14)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size=16)
  )
mortality_proj_annual

ggsave("figs/mortality_projections_annual.png", width = 6, height = 5)


mortality_proj <- total_mortality_projections %>%
  ggplot(aes(x = Year, y = cum_mortality, color = as.character(scenario)))+
  #geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5)+
  geom_line()+
  geom_point(size = 1.5)+
  scale_y_continuous(name = "Cumulative premature mortality",
                     limits = c(0, 250),
                     breaks = seq(0, 250, by = 50)
                     ) +
  ggtitle("Projected mortality (2025-2060)")+
  scale_color_manual(name = "GSL water level (mASL)", values = scenario_pal)+
  theme_cowplot(16)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)#, size=16)
  )
mortality_proj



costs_proj <- total_mortality_projections %>%
  ggplot(aes(x = Year, y = PV_cum_costs, color = as.character(scenario)))+
  geom_line()+
  geom_point(size = 1.5)+
  scale_y_continuous(name = "Cumulative mortality costs (millions USD)",
                    # limits = c(0, 500),
                     breaks = seq(0, 2000, by = 250)
                     ) +
  ggtitle("Present value of projected costs (2025-2060)")+
  scale_color_manual(name = "GSL water level (mASL)", values = scenario_pal)+
  theme_cowplot(16)+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)#, size=16)
        )
costs_proj
ggarrange(mortality_proj, costs_proj,
          ncol = 1, nrow = 2,
          labels = c("A", "B"),
          legend = "bottom", common.legend = T)
ggsave("figs/mortality_projections.png", width = 6, height = 11)


A <- ggarrange(mortality_costs_plot, mortality_race,
               ncol = 2, nrow = 1,
               labels = c("A", "B"),
               legend = "top", common.legend = F,
               font.label = list(size = 22)
               )

B <- ggarrange(mortality_proj, costs_proj,
               ncol = 2, nrow = 1,
               labels = c("C", "D"),
               legend = "none", common.legend = F,
               font.label = list(size = 22)
)

library(patchwork)

leg <- plot_spacer() + get_legend(mortality_proj) + plot_spacer() + plot_layout(ncol = 3, nrow = 1, 
                                                                                widths = c(0.65, 1, 1))


(A / plot_spacer()/ B / leg) + plot_layout(ncol = 1, nrow = 4, 
                          heights = c(1, 0.025, 1, 0.05)
)
ggsave("figs/mortality_quad.png", width = 14, height = 14)

A_alt <- plot_spacer() + ggarrange(mortality_costs_plot,
               ncol = 1, nrow = 1,
               labels = c("A"), legend = "none",
               font.label = list(size = 22)
) + plot_spacer() + plot_layout(widths = c(1, 3.5, 1))
leg_A <- plot_spacer() + get_legend(mortality_costs_plot) + plot_spacer() + plot_layout(ncol = 3, nrow = 1, 
                                                                                  widths = c(0.8, 1, 1))

B_alt <- ggarrange(mortality_proj, costs_proj,
               ncol = 2, nrow = 1,
               labels = c("B", "C"),
               legend = "none", common.legend = F,
               font.label = list(size = 22)
)



(A_alt / leg_A / plot_spacer() / B_alt / leg) + plot_layout(ncol = 1, nrow = 5, 
                                           heights = c(1, 0.1, 0.05, 1, 0.1)
)
ggsave("figs/mortality_trio.png", width = 13, height = 13)
