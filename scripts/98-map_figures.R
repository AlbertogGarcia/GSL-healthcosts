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

ct_mortality_relative <- read.csv("processed/ct_mortality_relative.csv", stringsAsFactors =  FALSE)

fips_county <- read.csv("data/gis/fips_codes/county_fips_master.csv")%>%
  filter(state_abbr %in% c("UT"))%>%
  mutate(County = str_remove(county_name, " County"),
         fips_county = as.character(county))%>%
  select(County, fips_county)

tracts.shp <- read_sf("data/gis/tracts/CensusTracts2020.shp")%>%
  mutate(FIPS = as.double(GEOID20),
         fips_county = str_remove(COUNTYFP20, "^0+")
  )%>%
  select(FIPS, fips_county)

utah.shp <- tracts.shp %>% st_union() %>% st_as_sf()

ct_mortality_relative.shp <- tracts.shp %>%
  left_join(fips_county, by = "fips_county")%>%
  filter(County %in% ct_mortality_relative$County)%>%
  select(FIPS)%>%
  left_join(ct_mortality_relative, by = "FIPS")%>% 
  mutate_at(vars(relative_pm10:ncol(.)), ~replace_na(., 0))%>%
  st_as_sf()

ct_mortality_relative_1278 <- ct_mortality_relative.shp %>%
  filter(scenario == 1278 | is.na(scenario))



get_maptypes()

bg <- basemaps::basemap_terra(ext=ct_mortality_relative.shp %>%
                                filter(County %in% c("Utah", "Salt Lake", "Davis"#, "Weber"
                                )), 
                              map_service = "carto", map_type = "light_no_labels"
)


bg_labels <- basemaps::basemap_terra(ext=ct_mortality_relative.shp, 
                                     map_service = "carto", map_type = "dark_only_labels"
)

ct_mortality_relative_1278 <- ct_mortality_relative_1278 %>%
  st_transform(terra::crs(bg)) %>% 
  mutate(relative_pm = relative_pm10 + relative_pm25)%>%
  st_as_sf()

hist(ct_mortality_relative_1278$relative_pm)

pm_map <- tm_shape(bg) + tm_rgb(alpha = 0)+#tm_rgb()+
  tm_shape(ct_mortality_relative_1278) + 
  tm_fill(col = "relative_pm",
          title = "PM Exposure",
          style = "cont",
          breaks = c(0, 2, 3.5),
          labels = c(0, 2, "3.5 or more")
  )+
  tm_shape(SLC %>% st_centroid()) + tm_text("NAME", size = 3/4, col = "black")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")+
  tm_layout(legend.outside = FALSE,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) 


cost_map <- tm_shape(bg) + tm_rgb(alpha = 0)+#tm_rgb()+
  tm_shape(ct_mortality_relative_1278) + 
  tm_fill(col = "relative_costs_VSL",
          title = "Mortality costs\n(millions USD)",
          style = "cont"
  )+
  tm_shape(SLC %>% st_centroid() %>% st_jitter(2)) + tm_text("NAME", size = 3/4, col = "black")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")+
  tm_layout(legend.outside = FALSE,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) +
  tm_scale_bar(breaks = c(0, 25, 50))+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))
cost_map

bg_utah <- basemaps::basemap_terra(ext=utah.shp, returnclass = 'raster', 
                                   map_service = "carto", map_type = "voyager_no_labels")%>%
  raster::mask(utah.shp)


e <- as(raster::extent(-12522831, -12340605, 4833572, 5076642), "SpatialPolygons")%>%
  st_as_sf()%>%
  st_set_crs(3857)
#proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

utah_inset  <- tm_shape(bg_utah) + tm_rgb()+
  tm_shape(utah.shp) + tm_borders(col = "black")+
  tm_shape(e) + tm_borders(lwd = 1.5, col = "red")+
  tm_layout(legend.outside = FALSE,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0),
            frame = FALSE,
            bg.color = "transparent") 
utah_inset




#%%%%%%%%%%%%%%%%%%%%%%%%
#### Combine all maps into single png
#%%%%%%%%%%%%%%%%%%%%%%%%

library(grid)
# 1. Open png file
png("results/figs/map.png"
    , width = 24.5, height = 17, res = 300, units = "cm")

grid.newpage()

print(pm_map, vp = viewport(0.25, 0.5)) #x,y coordinates of the grid here
print(cost_map, vp = viewport(0.75, 0.5)) #x,y coordinates of the grid here
print(utah_inset, vp = viewport(0.41, 0.15, height = 0.25))
# 3. Close the file
dev.off()


