# GSL dust costs: Figures using census tracts or other gis data
# albert.garcia@utah.edu
# created: 06/20/2025
# updated: 

# Set up environment ########################################

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
               tmap,
               basemaps
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load census tracts and match back to results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Utah_munic <- read_sf("data/gis/UtahMunic/Municipalities_Carto.shp")

SLC <- Utah_munic %>%
  filter(NAME %in% c("Salt Lake City"))

cities2 <- Utah_munic %>%
  filter(NAME %in% c("Logan", "Provo"))

tracts.shp <- read_sf("data/gis/tracts/CensusTracts2020.shp")%>%
  mutate(FIPS = as.double(GEOID20))%>%select(FIPS)

ct_mortality.shp <- tracts.shp %>%
  st_as_sf()%>%
  right_join(read.csv("processed/ct_mortality.csv", stringsAsFactors =  FALSE)
            , by = "FIPS")

ct_mortality_1278 <- ct_mortality.shp %>%
  filter(scenario == 1278)

ct_mortality_relative.shp <- tracts.shp %>%
  right_join(read.csv("processed/ct_mortality_relative.csv", stringsAsFactors =  FALSE)
            , by = "FIPS")

ct_mortality_relative_1278 <- ct_mortality_relative.shp %>%
  filter(scenario == 1278)



get_maptypes()

bg <- basemaps::basemap_terra(ext=ct_mortality.shp %>%
                                filter(County %in% c("Utah", "Salt Lake", "Davis", "Weber")), 
                              map_service = "carto", map_type = "light_no_labels"
)


bg_labels <- basemaps::basemap_terra(ext=ct_mortality.shp, 
                                     map_service = "carto", map_type = "dark_only_labels"
)

ct_mortality_1278 <- ct_mortality_1278 %>%
  st_transform(terra::crs(bg)) %>% st_as_sf()

ct_mortality_relative_1278 <- ct_mortality_relative_1278 %>%
  st_transform(terra::crs(bg)) %>% st_as_sf()

tm_shape(bg) + tm_rgb()+
  tm_shape(ct_mortality_1278) + tm_fill(col = "pm10",
                                     style = "cont",
                                     breaks = c(seq(0, 20, 5), 25),
                                     labels = c(seq(0, 20, 5), "25 or more"))+
  # tm_shape(SLC) + tm_borders()+
  # tm_shape(cities2) + tm_borders()+
  tm_shape(SLC %>% st_centroid() %>% st_jitter(1)) + tm_text("NAME", size = 3/4, col = "black")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")
  

tm_shape(bg) + tm_rgb()+
  tm_shape(ct_mortality_relative_1278) + tm_fill(col = "relative_mortality",
                                        style = "cont"
                                        # breaks = c(seq(0, 20, 5), 25),
                                        # labels = c(seq(0, 20, 5), "25 or more")
                                        )+
  # tm_shape(SLC) + tm_borders()+
  # tm_shape(cities2) + tm_borders()+
  tm_shape(SLC %>% st_centroid() %>% st_jitter(1)) + tm_text("NAME", size = 3/4, col = "black")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")
