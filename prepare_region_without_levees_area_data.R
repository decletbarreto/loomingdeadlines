source("~/coastal_deadline/scripts/configuration.R")
library(sf)

start.time <- Sys.time()

the.crs <- assign.crs(.state.abbreviation = NA)
east.sf <- read_sf("~/coastal_deadline/data/regions/east_coast_region_polygon_minus_leveed_areas.gpkg") %>%
           st_transform(., the.crs) %>%
           st_zm()
west.sf <- read_sf("~/coastal_deadline/data/regions/west_coast_region_polygon_minus_leveed_areas.gpkg") %>%
           st_transform(., the.crs) %>%
           st_zm()

states.sf <- read_sf(dsn = paste(data.dir,"/Census/CoastalCounties_official_dissolve_by_state.gpkg",sep="")) %>%
             st_transform(., the.crs)

east.states.intersect <- st_intersection(x = east.sf, y= states.sf)
west.states.intersect <- st_intersection(x = west.sf, y= states.sf)
east.west.states.intersect <- rbind(west.states.intersect, east.states.intersect) %>%
                              select(stateusps)
write_sf(obj = east.west.states.intersect,
         dsn = paste(data.dir, "/regions/region_minus_leveed_areas_by_state.gpkg", sep=""), driver="GPKG", delete_layer = TRUE)


