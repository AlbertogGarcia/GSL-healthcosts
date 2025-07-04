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

ct_mortality_relative_1275 <- ct_mortality_relative.shp %>%
  filter(scenario == 1275 | is.na(scenario))

GSL_shoreline <- read_sf("data/gis/GSLShoreline/GSLShoreline.shp")%>%
  mutate(ELEVMETERS = ELEVATION*0.3048)%>%
  filter(ELEVMETERS == 1280.160)

get_maptypes()

e_bg <- as(raster::extent(-12590000, -12400000, 4880000, 5080000), "SpatialPolygons")%>%
  st_as_sf()%>%
  st_set_crs(3857)

bg <- basemaps::basemap_terra(ext= e_bg,
                                # ct_mortality_relative.shp %>%
                                # filter(County %in% c("Utah", "Salt Lake", "Davis"
                                # )), 
                              map_service = "carto", map_type = "light_no_labels"
)


bg_labels <- basemaps::basemap_terra(ext=ct_mortality_relative.shp, 
                                     map_service = "carto", map_type = "dark_only_labels"
)

ct_mortality_relative_1275 <- ct_mortality_relative_1275 %>%
  st_transform(terra::crs(bg)) %>% 
  mutate(relative_pm = relative_pm10 + relative_pm25)%>%
  st_as_sf()

hist(ct_mortality_relative_1275$relative_pm)


pm_map <- tm_shape(e_bg) + tm_fill("white")+
  #tm_shape(bg) + tm_rgb(alpha = 0)+#tm_rgb()+
  tm_shape(ct_mortality_relative_1275) + 
  tm_polygons(col = "relative_pm10",
          title = "PM10 Exposure\n(\u03bcg/m3)",
          style = "cont",
          lwd = 0.05,
          breaks = c(0, 2, 3),
          labels = c(0, 2, "3 or more")
  )+
  tm_shape(GSL_shoreline)+ tm_fill(col = "#e0f3f8")+#"#abd9e9")+
  #tm_shape(GSL_shoreline_1275)+ tm_lines()+#"#2c7bb6")+
  tm_shape(SLC %>% st_centroid()) + tm_text("NAME", size = 3/4, col = "black")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")+
  tm_layout(legend.outside = FALSE,
            legend.position = c("right", "top"),
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) 
pm_map

cost_map <- tm_shape(e_bg) + tm_fill("white")+
  #tm_shape(bg) + tm_rgb(alpha = 0)+#tm_rgb()+
  tm_shape(ct_mortality_relative_1275) + 
  tm_polygons(col = "relative_costs_VSL",
          title = "Mortality costs\n(millions USD)",
          style = "cont",
          lwd = 0.05,
          breaks = c(0, 0.5, 1),
          labels = c(0, 0.5, "1 or more")
  )+
  tm_shape(GSL_shoreline)+ tm_fill(col = "#e0f3f8")+#"#abd9e9")+
  tm_shape(SLC %>% st_centroid() %>% st_jitter(2)) + tm_text("NAME", size = 3/4, col = "black")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")+
  tm_layout(legend.outside = FALSE,
            legend.position = c("right", "top"),
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) +
  tm_scale_bar(breaks = c(0, 25, 50))+
  tm_compass(type = "4star", size = 2, position = c("left", "top"))
cost_map

bg_utah <- basemaps::basemap_terra(ext=utah.shp, returnclass = 'raster', 
                                   map_service = "carto", map_type = "voyager_no_labels")%>%
  raster::mask(utah.shp)


# e <- as(raster::extent(-12522831, -12340605, 4833572, 5076642), "SpatialPolygons")%>%
#   st_as_sf()%>%
#   st_set_crs(3857)
#proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

utah_inset  <- tm_shape(bg_utah) + tm_rgb()+
  tm_shape(utah.shp) + tm_borders(col = "black")+
  tm_shape(e_bg) + tm_borders(lwd = 1.5, col = "red")+
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
    , width = 32, height = 17, res = 500, units = "cm")

grid.newpage()

print(pm_map, vp = viewport(0.25, 0.5)) #x,y coordinates of the grid here
print(cost_map, vp = viewport(0.75, 0.5)) #x,y coordinates of the grid here
print(utah_inset, vp = viewport(0.065, 0.15, height = 0.25))
# 3. Close the file
dev.off()


