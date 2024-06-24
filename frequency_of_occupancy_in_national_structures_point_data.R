source("~/coastal_deadline/scripts/configuration.R")

#point data
coastal.states <- coastal.states[!(coastal.states %in% c("AK","HI","MP"))]
coastal.states.name <- states.df %>% filter(state.abbreviation %in% coastal.states) %>%
         select(state.name)
shps <- paste("/home/rama/coastal_deadline/data/national_structures/shp/STRUCT_",unlist(coastal.states.name), "_State_Shape/Shape/Struct_Point.shp",sep="")
all.natl.map.codes <- data.frame(fcode = as.integer(), n=as.integer())
for(shp in shps)
{
  print(shp)
  curr.shp <- read_sf(shp)

  frequency1 <- curr.shp %>%
                st_drop_geometry() %>%
                group_by(fcode)    %>%
                count()
  all.codes <- rbind(all.natl.map.codes, frequency1)

  #pull out occupancies and put them in separate shapefiles
  #fire stations
  # fire.stations.sf <- curr.shp %>%
  #                     filter(fcode == 74026) %>%
  #                     write_sf(obj=.,dsn=paste(data.dir,"/output/occupancy",sep=""), layer="fire_stations_points", driver="ESRI Shapefile")

}

natl.map.codes <- all.codes %>%
                  group_by(fcode) %>%
                  summarise(n = sum(n)) %>%
                  left_join(x=., y=fcode.df, by=c("fcode" = "fcode")) %>%
                  select(fcode, fcode_desc) %>%
                  arrange(fcode, fcode_desc)
View(natl.map.codes)
write.csv(natl.map.codes, paste(data.dir,"/other/fcodes_in_coastal_states_in_national_map_point_data.csv", sep=""),row.names = FALSE)
#missing <- all.codes2 %>% filter(is.na(FCODE_DESC)) %>% select(fcode) %>% distinct()


#
# #fema
# enriched.shps <- list.files("~/coastal_deadline/data/output/coastal_structures_projected", pattern="shp$", full.names = TRUE)
# all.df <- data.frame(OCC_CLS = as.character(),PRIM_OCC=as.character(), n = as.integer())
# for (shp in enriched.shps)
# {
#   print(shp)
#   curr.sf <- read_sf(shp)
#   summary1 <- curr.sf %>% st_drop_geometry() %>% group_by(OCC_CLS,PRIM_OCC) %>% count() %>% arrange() %>% select(OCC_CLS,PRIM_OCC,n)
#   all.df <- rbind(all.df, summary1)
# }
#
# fema.footprints.codes <- all.df %>% group_by(OCC_CLS,PRIM_OCC) %>% summarise(n = sum(n))
# write.csv(fema.footprints.codes, paste(data.dir,"/other/fcodes_in_fema_footprints_data.csv", sep=""),row.names = FALSE)

# curr.sf.idx <- curr.sf %>% st_as_sfc() %>% st_sfc()
# write_sf(curr.sf.idx, dsn=tmp.dir,layer="WA_idx", driver="ESRI Shapefile")
