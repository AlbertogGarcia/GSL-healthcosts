# GSL dust costs: Mortality and asthma data processing
# albert.garcia@utah.edu
# created: 06/20/2025
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
               cowplot,
               ggpubr,
               purrr,
               ggplot2
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
                "bad" = "#d7191c",
                "current" = "#fdae61",
                "target" = "#abd9e9",
                "avg" = "#2c7bb6"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### set base parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_scenarios <- seq(4182, 4203, by = 1) 
current_scenario = 4192
relevant_scenarios <- c(4183, current_scenario, 4198, 4200) 

scenario_descrip <- c("No conservation",
                      "Current lake    ",
                      "Minimum healthy\nlake          ",
                      "Recent historical\naverage      "
)

scenario_pal <- c(palette$bad, palette$current, palette$target, palette$avg)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Annual
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Mortality
total_mortality <- read.csv("processed/total_mortality.csv", stringsAsFactors =  FALSE) %>%
  filter(endpoint == "Mortality, All-cause",
         scenario %in% relevant_scenarios)%>%
  mutate(costs = costs_VSL*1000000)%>%
  select(scenario, costs) %>%
  mutate(endpoint_category = "Mortality")

#### School Loss Days
total_schoolloss <- read.csv("processed/total_schoolloss.csv", stringsAsFactors =  FALSE) %>%
  rename(costs = costs_SLD)%>%
  select(scenario, costs) 

#### All morbidity
total_morbidity <- read.csv("processed/total_morbidity.csv", stringsAsFactors =  FALSE) %>%
  group_by(scenario) %>%
  summarise(costs = sum(costs, na.rm = T))%>%
  ungroup %>%
  rbind(total_schoolloss) %>%
  group_by(scenario) %>%
  summarise(costs = sum(costs, na.rm = T))%>%
  ungroup %>%
  mutate(endpoint_category = "Morbidity")

totals_df <- total_morbidity %>%
  rbind(total_mortality) %>%
  mutate(costs_millions = costs / 1e6) %>%
  group_by(scenario) %>%
  summarise(total_costs = sum(costs_millions), .groups = "drop")

total_annual_costs_plot <- total_morbidity %>%
  rbind(total_mortality) %>%
  mutate(costs_millions = costs/1000000)%>%
  ggplot(aes(x=reorder(scenario, scenario, order = T), y=costs_millions, fill = endpoint_category)
  )+
  geom_bar(stat='identity')+
  geom_text(
    data = totals_df,
    aes(
      x = reorder(scenario, scenario, order = TRUE),
      y = total_costs,
      label = round(total_costs, 2)
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    size = 4
  )+
  scale_y_continuous(limits = c(0, max(totals_df$total_costs)*1.07))+
  scale_x_discrete(labels = scenario_descrip)+
  geom_hline(yintercept = 0, linewidth = 0.25)+
  ggtitle("Current annual dust-induced health costs") + 
  xlab("GSL water level (ftASL)")+
  ylab("Costs (millions USD)") +
  scale_fill_manual(values = c(palette$blue, palette$red, palette$green))+
  theme_cowplot(14)+
  theme(legend.position = "top",
        legend.justification = "center",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )+
  coord_flip()+
  guides(fill = guide_legend(title = NULL, reverse=T, hjust = 0.5))
total_annual_costs_plot
ggsave("figs/costs_mortality_vs_morbidity.png",
       width = 8, height = 6)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Separate plots by morbidity endpoint

# School Loss Days
total_schoolloss_endpoint <- read.csv("processed/total_schoolloss.csv", stringsAsFactors =  FALSE) %>%
  mutate(endpoint_subcategory = endpoint) %>%
  group_by(scenario, endpoint_subcategory) %>%
  summarise(costs = sum(costs_SLD, na.rm = T),
            morbidity = sum(SLD, na.rm = T))%>%
  ungroup

# All morbidity
total_morbidity_endpoint <- read.csv("processed/total_morbidity.csv", stringsAsFactors =  FALSE) %>%
  mutate(endpoint_subcategory = case_when(
    endpoint == "Work Loss Days" ~ endpoint,
    str_detect(endpoint, "HA") ~ "Hospital Admissions",
    str_detect(endpoint, "ER visits") ~ "Emergency Department Visits",
  )) %>%
  group_by(scenario, endpoint_subcategory) %>%
  summarise(costs = sum(costs, na.rm = T),
            morbidity = sum(morbidity, na.rm = T))%>%
  ungroup %>%
  rbind(total_schoolloss_endpoint)

endpoint_levels <- unique(total_morbidity_endpoint$endpoint_subcategory)
plot_list <- list()
i = 1

for(i in seq_along(endpoint_levels)){
  
  e <- endpoint_levels[i]
  
  this_endpoint <- total_morbidity_endpoint %>%
    filter(endpoint_subcategory == e) %>%
    mutate(cost_per_morbidity = costs / morbidity)
  
  c <- this_endpoint$cost_per_morbidity[1] / 1e6
  
  # Base plot
  p <- ggplot(
    this_endpoint,
    aes(
      x = reorder(scenario, scenario, order = TRUE),
      y = morbidity,
      fill = as.character(scenario)
    )
  ) +
    geom_bar(stat = "identity") +
    ggtitle(e) +
    scale_x_discrete(labels = scenario_descrip)+
    scale_y_continuous(
      "Dust-induced occurrences",
      labels = scales::label_comma(),
      sec.axis = sec_axis(~ . * c, name = "Costs (millions USD)")
    ) +
    scale_fill_manual(values = scenario_pal, 
                      name = "Lake elevation scenario",
                      labels = scenario_descrip)+
    theme_cowplot(14) +
    theme(
      plot.margin = margin(b = 20),
      plot.title = element_text(hjust = 0.5, margin = margin(b = 12)),
      legend.title.position = "top",
      legend.direction = "horizontal",
      legend.box.just = "center",  
      legend.justification = "center", 
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 13),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    coord_flip() + 
    guides(fill = guide_legend(reverse = T, hjust = 0.5, title.hjust = 0.5))
  
  # Column logic (2 columns)
  is_left  <- i %% 2 == 1
  is_right <- i %% 2 == 0
  # Row logic (2 rows)
  is_top_row    <- i <= 2
  is_bottom_row <- i >  2
  
  # Remove bottom axis on top row
  
  # if (is_top_row) {
  #   p <- p + theme(
  #     axis.title.x.bottom = element_blank()
  #   )
  # }
  # 
  # # Remove top axis on bottom row
  # if (is_bottom_row) {
  #   p <- p + theme(
  #     axis.title.x.top = element_blank()
  #   )
  # }
  
  # Remove unwanted axes
  if (!is_left) {
    p <- p + theme(
      axis.text.y.left = element_blank()
    )
  }
  
  if (!is_right) {
    p <- p + theme(
      axis.text.y.right = element_blank()
    )
  }
  
  plot_list[[i]] <- p
}

ggarrange(
  plotlist = plot_list,
  ncol = 2, widths = c(1.35, 1.01),
  nrow = 2,
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE,
  legend = "bottom"
)

ggsave("figs/morbidity_costs.png",
       width = 9, height = 9)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Racial distribution of costs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

total_mortality_race <- read.csv("processed/total_mortality_race.csv", stringsAsFactors =  FALSE) %>%
  filter(scenario %in% relevant_scenarios)
total_schoolloss_race <- read.csv("processed/total_schoolloss_race.csv", stringsAsFactors =  FALSE)
#total_morbidity_race <- 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Projections through 2060
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Mortality
total_mortality_projections <- read.csv("processed/total_mortality_projections.csv", stringsAsFactors =  FALSE) %>%
  filter(scenario %in% relevant_scenarios)%>%
  mutate(PV_costs = PV_costs_VSL,
         PV_cum_costs = PV_cum_costs_VSL)%>%
  select(scenario, Year, PV_costs, PV_cum_costs) %>%
  mutate(endpoint_category = "Mortality")


#### School Loss Days
total_schoolloss_projections <- read.csv("processed/total_schoolloss_projections.csv", stringsAsFactors =  FALSE) %>%
  rename(PV_costs = PV_costs_SLD,
         PV_cum_costs = PV_cum_costs_SLD)%>%
  select(scenario, Year, PV_costs, PV_cum_costs) 


#### Morbidity
total_morbidity_projections <- read.csv("processed/total_morbidity_projections.csv", stringsAsFactors =  FALSE) %>%
  group_by(scenario, Year) %>%
  summarise(PV_costs = sum(PV_costs_COI, na.rm = T),
            PV_cum_costs = sum(PV_cum_costs_COI, na.rm = T))%>%
  ungroup %>%
  rbind(total_schoolloss_projections) %>%
  group_by(scenario, Year) %>%
  summarise(PV_costs = sum(PV_costs, na.rm = T),
            PV_cum_costs = sum(PV_cum_costs, na.rm = T))%>%
  ungroup %>%
  mutate(endpoint_category = "Morbidity")


cum_costs_proj <- total_morbidity_projections %>%
  rbind(total_mortality_projections) %>%
  group_by(scenario, Year) %>%
  summarise(PV_costs = sum(PV_costs),
            PV_cum_costs = sum(PV_cum_costs),
            PV_costs_billions = sum(PV_costs/1000),
            PV_cum_costs_billions = sum(PV_cum_costs/1000))

costs_proj <- ggplot(data = cum_costs_proj,
       aes(x = Year, y = PV_costs, color = as.character(scenario))
)+
  geom_line()+
  geom_point(size = 1.5)+
  scale_y_continuous(name = "Present Value Costs (millions USD)"
  , breaks = seq(0, 250, 50)) +
  ggtitle("Projected changes in annual costs")+
  scale_color_manual(values = scenario_pal,
                     names = scenario_descrip)+
  theme_cowplot(12)+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)#, size=16)
  )
costs_proj
cum_costs_bar <- ggplot(data = cum_costs_proj,
       aes(x=reorder(scenario, scenario, order = T), y=PV_costs_billions, fill = as.character(scenario))
)+
  geom_bar(stat='identity')+
  ggtitle("Cumulative health costs through 2060") + 
  #xlab("GSL water level (ftASL)")+
  ylab("Present Value Costs (billions USD)") +
  scale_fill_manual(values = scenario_pal,
                    name = "Lake elevation scenario",
                     labels = scenario_descrip)+
  theme_cowplot(12)+
  theme(legend.title.position = "top",
    legend.direction = "horizontal",
    legend.box.just = "center",  
    legend.justification = "center", 
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )+
  coord_flip()+
  guides(fill = guide_legend(reverse = T, title.hjust = 0.5, hjust = 0.5))
cum_costs_bar
legend <- get_legend(cum_costs_bar)

cum_costs_bar <- cum_costs_bar +
  theme(legend.position = "none")

p1 <- ggarrange(costs_proj, cum_costs_bar,
  ncol = 2,
  labels = c("A", "B")
)

ggarrange(p1, legend,
  ncol = 1,
  heights = c(1, 0.2)
)

ggsave("figs/total_costs_projected.png",
       width = 9, height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Racial disparities in costs per capita
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_A <- c(
  "American Indian and Alaska Native Alone" = "American Indian",
  "Asian alone" = "Asian",
  "Black or African American Alone" = "Black/African American",
  "Hispanic or Latino" = "Hispanic/Latino",
  "Hawaiian and PI Alone" = "Pacific Islander",
  "White NH" = "White",
  "Multi Racial" = "Multiracial",
  "Other Race Alone" = "Other"
)
map_B <- c(
  "AfAm/Black" = "Black/African American",
  "Asian" = "Asian",
  "Hispanic/Latino" = "Hispanic/Latino",
  "Pacific Islander" = "Pacific Islander",
  "White" = "White"
)


total_mortality_race <- read.csv("processed/total_mortality_race.csv", stringsAsFactors =  FALSE) %>%
  filter(scenario %in% relevant_scenarios)%>%
  mutate(race_std = recode(race, !!!map_A),
         mortality_costs = costs_VSL*1000000)%>%
  select(scenario, race_std, mortality_costs, population, pm_delta)

total_schoolloss_race <- read.csv("processed/total_schoolloss_race.csv", stringsAsFactors =  FALSE) %>%
  mutate(race_std = recode(Race, !!!map_B)) %>%
  rename(costs = costs_SLD) %>%
  select(scenario, race_std, costs)

unique(total_mortality_race$race_std)
unique(total_schoolloss_race$race_std)

total_morbidity_race <- read.csv("processed/total_morbidity_race.csv", stringsAsFactors =  FALSE) %>%
  mutate(race_std = recode(race, !!!map_A)) %>%
  group_by(scenario, race_std) %>%
  summarise(costs = sum(costs, na.rm = T))%>%
  ungroup %>%
  rbind(total_schoolloss_race) %>%
  group_by(scenario, race_std) %>%
  summarise(morbidity_costs = sum(costs, na.rm = T))%>%
  ungroup 

total_costs_race <- total_mortality_race %>%
  full_join(total_morbidity_race, by = c("race_std", "scenario")) %>%
  mutate(costs = morbidity_costs + mortality_costs,
         costs_per_capita = costs/population,
         costs_per_delta = costs_per_capita/pm_delta) %>%
  drop_na(mortality_costs) %>%
  rename(Race = race_std)

total_costs_race %>%
  ggplot(aes(x=scenario, color = reorder(Race, -costs_per_capita), y=costs_per_capita))+
  geom_line(linewidth = 1)+
  geom_point(size = 2.5)+
  scale_y_continuous(name = "Health costs per capita",
                     labels = label_dollar())+
  scale_x_continuous(
    breaks = relevant_scenarios,
    name = "GSL water level (ftASL)")+
  scale_color_manual(values = c(palette$red, palette$green, palette$orange, palette$blue, palette$dark)
  )+
  ggtitle("Per-capita health costs by race")+
  theme_cowplot(14)+
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.justification = "center"
  )
ggsave("figs/race_costs.png",
       width = 9, height = 6)

cost_plot_current <- total_costs_race %>%
  filter(scenario == 4192) %>%
  ggplot(aes(
    x = costs_per_capita,
    y = reorder(Race, costs_per_capita)
  )) +
  geom_point(size = 5, color = palette$dark) +
  scale_x_continuous(
    name = "Per-capita health costs",
    labels = label_dollar()
  ) +
  scale_y_discrete(name = NULL) +
  ggtitle("Health costs by race") +
  theme_cowplot(13) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    axis.line = element_line(color = "grey50"),
    plot.title = element_text(hjust = 0.5)
  )
cost_plot_current

pm_plot_current <- total_costs_race %>%
  filter(scenario == 4192) %>%
  ggplot(aes(
    x = pm_delta,
    y = reorder(Race, costs_per_capita)
  )) +
  geom_point(size = 5, color = palette$dark) +
  scale_x_continuous(
    name = "Avg. PM exposure per dust storm"
  ) +
  scale_y_discrete(name = NULL) +
  ggtitle("Dust exposure by race") +
  theme_cowplot(13) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.25),
    axis.line = element_line(color = "grey50"),
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
pm_plot_current

ggarrange(cost_plot_current, pm_plot_current,
          ncol = 2, widths = c(1.3,1)
)

ggsave("figs/race_disparity_dotplot.png",
       width = 9, height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Output total costs per capita at census-tract for maps
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Mortality
ct_mortality <- read.csv("processed/ct_mortality_map.csv", stringsAsFactors =  FALSE) %>%
  filter(scenario %in% relevant_scenarios)%>%
  mutate(mortality_costs = costs_VSL*1000000)%>%
  select(FIPS, County, scenario, mortality_costs, population, pm_delta)

#### School Loss Days
ct_schoolloss <- read.csv("processed/ct_schoolloss_map.csv", stringsAsFactors =  FALSE) %>%
  rename(costs = costs_SLD)%>%
  select(FIPS, scenario, costs) 

#### All morbidity
ct_morbidity <- read.csv("processed/ct_morbidity_map.csv", stringsAsFactors =  FALSE) %>%
  group_by(FIPS, scenario) %>%
  summarise(costs = sum(costs, na.rm = T))%>%
  ungroup %>%
  rbind(ct_schoolloss) %>%
  group_by(FIPS, scenario) %>%
  summarise(morbidity_costs = sum(costs, na.rm = T))%>%
  ungroup 

ct_totals <- ct_mortality %>%
  full_join(ct_morbidity, by = c("FIPS", "scenario")) %>%
  mutate(costs = morbidity_costs + mortality_costs,
         costs_per_capita = costs/population) 

write.csv(ct_totals, file = "processed/ct_totals.csv", row.names = FALSE)
