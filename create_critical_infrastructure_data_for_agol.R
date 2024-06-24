source("~/coastal_deadline/scripts/configuration.R")
#critical infrastructure maps
ci.list <- list.files(paste(output.dir, "/coastal_infrastructure",sep=""), pattern=".gpkg$", full.names = TRUE)
all.ci.sf <- read_sf(ci.list[1]) %>% st_transform(., 4326)
for(ci in ci.list[2: length(ci.list)])
#for(ci in ci.list[2:4])
{
  print(ci)
  ci.sf     <- read_sf(ci) %>% st_transform(., 4326)
  all.ci.sf <- rbind(all.ci.sf, ci.sf)
}
nrow(all.ci.sf)

npl.sf <- read_sf(paste(data.dir,"/EPA/SuperfundSiteBoundaries.gpkg", sep="")) %>% st_centroid() %>%
          mutate(id =EPA_ID,
                 name = SITE_NAME,
                 address = STREET_ADDR_TXT,
                 city = CITY_NAME,
                 zipcode = ZIP_CODE,
                 state = STATE_CODE,
                 type = "SUP",
                 county =  "",
                 levee_flg =  "",
                 cs_name = "") %>%
          select(id, name, address, city, zipcode, state, type, county, levee_flg, cs_name)

shp <- paste(output.dir, "/agol/all_critical_infrastructure",sep="")
agol.cols <- c("id", "name", "address", "city", "state", "zipcode", "type", "cs_name", "county","levee_flg")
all.ci.sf.final <-  all.ci.sf %>%
                    mutate(type      = infrastructure_type,
                           cs_name   = county.sub.name,
                           county    = countyname,
                           levee_flg = inside_leveed_area) %>%
                    select(all_of(agol.cols))              %>%
                    rbind(npl.sf) %>%
                    write_sf(., dsn = shp, driver="ESRI Shapefile", delete_layer = T)
shp.zip <- paste(output.dir, "/agol/all_critical_infrastructure.zip",sep="")
shps <- list.files(path = paste(output.dir, "/agol/all_critical_infrastructure",sep=""), pattern= "all_critical_infrastructure.*", full.names = T)
zip(shp.zip,shps)

