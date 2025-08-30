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
               basemaps,
               terra
)

options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
options(java.parameters = "-Xmx8000m") 
`%ni%` <- Negate(`%in%`)  # "not in" function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Load census tracts and match back to results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Utah_munic <- read_sf("data/gis/UtahMunic/Municipalities_Carto.shp")

GSL_geotif <- terra::rast("data/gis/Great_Salt_Lake_TBDEM_100m_bilinear.tif")

## from-to-becomes
# classify the values into three groups 
# all values >= 0 and <= 0.25 become 1, etc.
m <- c(0, 4192.913, 1278,
       # 4192.913, 4210, 1282,
       # 4210, Inf, NA
       4192.913, 4207, 1282,
       4207, Inf, NA
       )
rclmat <- matrix(m, ncol=3, byrow=TRUE)
GSL_rcl <- terra::as.polygons(classify(GSL_geotif, rclmat, include.lowest=FALSE))%>%
  st_as_sf %>%
  mutate(elevation = case_when(Band_1 == 1282 ~ "Long-term avg: 1282mASL",
                               Band_1 == 1278 ~ "Current level: 1278mASL")
         )

SLC <- Utah_munic %>%
  filter(NAME %in% c("Salt Lake City"))

cities2 <- Utah_munic %>%
  filter(NAME %in% c("Logan", "Provo"))

ct_mortality <- read.csv("processed/ct_mortality.csv", stringsAsFactors =  FALSE)

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

ct_mortality.shp <- tracts.shp %>%
  left_join(fips_county, by = "fips_county")%>%
  select(FIPS)%>%
  left_join(ct_mortality, by = "FIPS")%>% 
  filter(County %in% ct_mortality$County
         , endpoint == "Mortality, All-cause"
         )%>%
  mutate_at(vars(pm_delta:ncol(.)), ~replace_na(., 0))%>%
  st_as_sf()

get_maptypes()

e_bg <- as(raster::extent(-12595000, -12370000, 4890000, 5125000), "SpatialPolygons")%>%
  st_as_sf()%>%
  st_set_crs(3857)
e_bg

bg <- basemaps::basemap_terra(ext= e_bg,
                                map_service = "carto", map_type = "light_no_labels"
)


bg_labels <- basemaps::basemap_terra(ext=ct_mortality.shp, 
                                     map_service = "carto", map_type = "dark_only_labels"
)

ct_mortality_1278 <- ct_mortality.shp %>%
  mutate(mortality_per100k = mortality/pop*100000,
         mortality_per100k = replace_na(mortality_per100k, 0)) %>%
  filter(scenario == 1278 | is.na(scenario)) %>%
  st_transform(terra::crs(bg)) %>% 
  st_as_sf()

hist(ct_mortality_1278$pm_delta)

pm_map <- tm_shape(e_bg) + tm_fill("#f5f9f9")+
  #tm_shape(bg) + tm_rgb(alpha = 0.9)+#tm_rgb()+
  tm_shape(ct_mortality_1278) + 
  tm_polygons(col = "pm_delta",
          title = "PM Exposure\n(\u03bcg/m3)",
          style = "cont",
          pal = "-magma",
          alpha = 0.85,
          lwd = 0.2,
          breaks = c(0, 1000, 2000),
          labels = c(0, 1000, "2000 or more")
  )+
  tm_shape(GSL_rcl) + tm_fill("elevation",
                              title = "GSL elevation",
                              palette = c("#64a2b8", "#cdb9a6")
                              ) + 
  tm_legend(frame = T,
            bg.color = "#f9f9f9")+
  tm_shape(SLC %>% st_centroid()) + tm_text("NAME", size = 1, col = "white", fontface = "bold")+
  tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 3/4, col = "black")+
  tm_layout(legend.outside = FALSE,
            legend.position = c("right", "top"),
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) 
pm_map

cost_map <- tm_shape(e_bg) + tm_fill("#f5f9f9")+
  #tm_shape(bg) + tm_rgb(alpha = 0.9)+#tm_rgb()+
  tm_shape(ct_mortality_1278) + 
  tm_polygons(col = "mortality_per100k",
              title = "Expected mortality\n(per 100k)",
              style = "cont",
              pal = "-magma",
              alpha = 0.85,
              lwd = 0.15,
              breaks = c(0, 0.05, 0.1, .15),
              labels = c(0, 0.05, 0.1, "0.15 or more")
  )+
  tm_shape(GSL_rcl) + tm_fill("elevation",
                              palette = c("#64a2b8", "#cdb9a6"),
                              legend.show = FALSE
  ) + 
  tm_legend(frame = T,
            bg.color = "#f9f9f9")+
  # tm_shape(SLC %>% st_centroid()) + tm_text("NAME", size = 3/4, col = "white", fontface = "bold")+
  # tm_shape(cities2 %>% st_centroid()) + tm_text("NAME", size = 2/3, col = "black")+
  tm_layout(legend.outside = FALSE,
            legend.position = c("right", "top"),
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) 
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
png("figs/map.png"
    , width = 32.5, height = 17, res = 1000, units = "cm")

grid.newpage()

print(pm_map, vp = viewport(0.25, 0.5)) #x,y coordinates of the grid here
print(cost_map, vp = viewport(0.75, 0.5)) #x,y coordinates of the grid here
print(utah_inset, vp = viewport(0.065, 0.15, height = 0.27))
# 3. Close the file
dev.off()

