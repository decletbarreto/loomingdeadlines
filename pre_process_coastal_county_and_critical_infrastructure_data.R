source("~/coastal_deadline/scripts/configuration.R")
library(doParallel)
library(pryr)
start.time <- Sys.time()

coastal.counties.for.join.df <- coastal.counties.sf %>% st_drop_geometry() %>% select(countyfips, countyname) %>% arrange(countyfips)

# #1. Prepare county subdivision data -------------------------------------
#CONUS
# cs.conus.sf <- cs.sf %>%
#       mutate(county.fips = substr(GEOID,1,5)) %>% #create county fips columnn
#       filter(geog== "conus" & county.fips %in% coastal.counties.fips) %>%  #subset cs in conus and coastal counties
#       filter(!(COUSUBNS == "00000000")) %>% #eliminate county subdivisions over water and w/out population
#       st_transform(st_crs(crs.list[1])) %>% #project
#       st_write(.,dsn=paste(census.output.dir, "county_subdivisions_conus.gpkg", sep="/"),  driver="GPKG",delete_layer = TRUE) %>%
#       st_write(.,dsn=paste(census.output.dir, "county_subdivisions_conus", sep="/"),  driver="ESRI Shapefile",delete_layer = TRUE)
#
# #PR
# cs.pr.sf <- cs.sf %>%
#       mutate(county.fips = substr(GEOID,1,5)) %>% #create county fips column
#       filter(abb== "PR" & county.fips %in% coastal.counties.fips) %>% #subset cs in PR and coastal counties
#       filter(!(COUSUBNS == "00000000")) %>% #eliminate county subdivisions over water and w/out population
#       st_transform(st_crs(crs.list[2])) %>% #project
#       st_write(.,dsn=paste(census.output.dir, "county_subdivisions_pr.gpkg", sep="/"),  driver="GPKG",delete_layer = TRUE)
# #VI
# cs.vi.sf <- cs.sf %>%
#   mutate(county.fips = substr(GEOID,1,5)) %>% #create county fips columnn
#   filter(abb %in% c("VI") & county.fips %in% coastal.counties.fips) %>% #subset cs in VI and coastal counties
#   filter(!(COUSUBNS == "00000000")) %>% #eliminate county subdivisions over water and w/out population
#   st_transform(st_crs(crs.list[2])) %>% #project
#   st_write(.,dsn=paste(census.output.dir, "county_subdivisions_vi.gpkg", sep="/"),  driver="GPKG",delete_layer = TRUE)
# #GU
# cs.gu.sf <- cs.sf %>%
#        mutate(county.fips = substr(GEOID,1,5)) %>% #create county fips columnn
#        filter(abb %in% c("GU") & county.fips %in% coastal.counties.fips) %>% #subset cs in GU and coastal counties
#        filter(!(COUSUBNS == "00000000")) %>% #eliminate county subdivisions over water and w/out population
#        st_transform(st_crs(crs.list[3])) %>% #project
#        st_write(.,dsn=paste(census.output.dir, "county_subdivisions_gu.gpkg", sep="/"),  driver="GPKG",delete_layer = TRUE)


# preprocess.coastal.county.and.infrastructure.data ---------------
cores <- 10
preprocess.coastal.county.and.infrastructure.data.logfile <- paste(log.dir,"/preprocess_coastal_county_and_infrastructure_data_logfile.log",sep="")
cl <- makeCluster(cores, outfile = preprocess.coastal.county.and.infrastructure.data.logfile)
registerDoParallel(cl)
#stopCluster(cl)
tic()
state.abb = "LA"
#foreach(state.abb.idx = icount(length(states.in.analysis)), .packages = c("dplyr","sf")) %dopar%
#foreach(state.abb.idx = icount(length( c("GU"))), .packages = c("dplyr","sf")) %dopar%
foreach(state.abb.idx = icount(length(c("PR")))) %do%
{
  start.time.loop <- Sys.time()
  #state.abb <- states.in.analysis[state.abb.idx]
  state.abb <- "LA"
  state.fullname <- states.df %>% filter(state.abbreviation == state.abb) %>% select(state.name) %>% pull() #get state name to build shapefile path
  ns.fc.name <- paste(ns.shapefiles.dir, "/STRUCT_", state.fullname, "_State_Shape/Shape/Struct_Point.shp",sep="") #points shapefile name

  the.crs <- assign.crs(state.abb) #assign CRS

  #critical infrastructure points
  print(paste("reading ", state.abb, " ", ns.fc.name,sep=""))
  state.ns.sf <- read_sf(ns.fc.name) %>%
    st_transform(x = .,crs = the.crs)  %>% #reproject
    filter(fcode %notin% fcodes.to.exclude) %>% #exclude some FCODEs
    dplyr::left_join(x  = .,
                     y  = fcode.df,
                     by = "fcode") %>%
    mutate(provenance = "NAT",  #this feature came from The National Map dataset
           id = as.character(permanent_),
           name       = as.character(name),
           address    = as.character(address),
           city       = as.character(city),
           state      = as.character(state),
           zipcode    = as.character(zipcode),
           provenance = as.character(provenance),
           ftype      = as.character(ftype),
           fcode      = as.character(fcode),
           fcode_desc = as.character(fcode_desc),
           ftype_desc = as.character(ftype_desc),
           infrastructure_type = as.character(NA)) %>%
           mutate(infrastructure_type = case_when(
                                        fcode_desc %in% all.k12.fcode.desc           ~ k12.code, #recode Elem, Middle, High Schools into K-12
                                        fcode_desc %in% all.higher.ed.fcode.desc     ~ higher.ed.code, #recode Colleges, Universities, Trade and Technical shools into Higher Ed Institutions
                                        fcode_desc == "Ambulance Service"            ~ amb.code,
                                        fcode_desc == "Court House"                  ~ cor.code,
                                        fcode_desc == "Department of Public Works"   ~ city.hall.code,
                                        fcode_desc == "Fire Station"                 ~ fir.code,
                                        fcode_desc == "Headquarters"                 ~ natl.park.hq.code,
                                        fcode_desc == "Hospital/Medical Center"      ~ hos.code,
                                        fcode_desc == "Law Enforcement"              ~ law.code,
                                        fcode_desc == "Post Office"                  ~ pos.code ,
                                        fcode_desc == "Prison/Correctional Facility" ~ pri.code,
                                        fcode_desc == "State Capitol"                ~ scap.code,
                                        fcode_desc == "State Supreme Court"          ~ ssc.code,
                                        fcode_desc == "Ranger Station"               ~ rngr.code,
                                        fcode_desc %in% c("US Capitol", "US Supreme Court","White House") ~ gov.code)) %>%
            select(all_of(ns.point.cols.list))

  #VA hospital
  print("VA")
  state.va.sf <- va.sf %>%
                 filter(STATE==state.abb) %>%
                 st_transform(x = .,crs = the.crs) %>%
                 mutate(id         = as.character(UNIQUE_ID),
                        name       = as.character(NAME),
                        address    = as.character(ADDRESS),
                        city       = as.character(CITY),
                        state      = as.character(STATE),
                        zipcode    = as.character(ZIP),
                        provenance = as.character(va.code),
                        ftype      = as.character(NA),
                        fcode      = as.character(NA),
                        fcode_desc = as.character(NA),
                        ftype_desc = as.character(NA),
                        infrastructure_type = as.character("VA")) %>%
                select(all_of(ns.point.cols.list))

  #Public housing
  print("PH")
  state.ph.sf <- ph.sf %>%
    filter(STD_ST==state.abb) %>%
    st_transform(x = .,crs = the.crs) %>%
    mutate(id = paste("PH", row_number(),sep=""),
           name       = as.character(PROJECT_NA),
           address    = as.character(STD_ADDR),
           city       = as.character(STD_CITY),
           state      = as.character(STD_ST),
           zipcode    = as.character(STD_ZIP5),
           provenance = as.character(ph.code),
           ftype      = as.character(NA),
           fcode      = as.character(NA),
           fcode_desc = as.character(NA),
           ftype_desc = as.character(NA),
           infrastructure_type = as.character("PH")) %>%
    select(all_of(ns.point.cols.list))

  #Wastewater treatment plants
  print("WT")
  state.wwtp.sf <- wwtp.sf %>%
    filter(STATE_CODE == state.abb) %>%
    st_transform(x = .,crs = the.crs) %>%
    mutate(id         = as.character(REGISTRY_ID),
           name       = as.character(PRIMARY_NAME),
           address    = as.character(LOCATION_ADDRESS),
           city       = as.character(CITY_NAME),
           state      = as.character(STATE_CODE),
           zipcode    = as.character(POSTAL_CODE),
           provenance = as.character("WT"),
           ftype      = as.character(NA),
           fcode      = as.character(NA),
           fcode_desc = as.character(NA),
           ftype_desc = as.character(NA),
           infrastructure_type = as.character(wt.code)) %>%
    st_set_geometry("geometry") %>%
    select(all_of(ns.point.cols.list))

  #TRI sites
  print("TRI")
  state.tri.sf <- tri.sf %>%
    filter(`8. ST` ==state.abb) %>%
            st_transform(x = .,crs = the.crs) %>%
            mutate(id         = as.character(`2. TRIFD`),
                   name       = as.character(`4. FACILITY NAME`),
                   address    = as.character(`5. STREET ADDRESS`),
                   city       = as.character(`6. CITY`),
                   state      = as.character(`8. ST`),
                   zipcode    = as.character(`9. ZIP`),
                   provenance = as.character(tri.code),
                   ftype      = as.character(NA),
                   fcode      = as.character(NA),
                   fcode_desc = as.character(NA),
                   ftype_desc = as.character(NA),
                   infrastructure_type = as.character("TRI")) %>%
    select(all_of(ns.point.cols.list))

  #substations sites
  print("SUB")
  state.substations.sf <- substations.sf %>%
    filter(STATE ==state.abb) %>%
    st_set_geometry("geometry") %>%
    st_transform(x = .,crs = the.crs) %>%
    mutate(id         = as.character((ID)),
           name       = as.character(NAME),
           address    = as.character(NA), #no address field on substations data
           city       = as.character(CITY),
           state      = as.character(STATE),
           zipcode    = as.character(ZIPCODE),
           provenance = as.character(sub.code),
           ftype      = as.character(NA),
           fcode      = as.character(NA),
           fcode_desc = as.character(NA),
           ftype_desc = as.character(NA),
           infrastructure_type = as.character(sub.code)) %>%
    select(all_of(ns.point.cols.list))

  #power plants
  print("POW")
  state.plants.sf <- power.plants.sf %>%
                     filter(Plant.State ==state.abb) %>%
                     st_transform(x = .,crs = the.crs) %>%
                     select(Plant.ID, Plant.Name, Street.Address, City, Plant.State, Zip) %>%
                     mutate(id         = as.character(paste("POW", as.character(Plant.ID),sep = "")),
                           name       = as.character(Plant.Name),
                           address    = as.character(Street.Address),
                           city       = as.character(City),
                           state      = as.character(Plant.State),
                           zipcode    = as.character(Zip),
                           provenance = as.character(pow.code),
                           ftype      = as.character(NA),
                           fcode      = as.character(NA),
                           fcode_desc = as.character(NA),
                           ftype_desc = as.character(NA),
                           infrastructure_type = as.character("POW")) %>%
                    select(all_of(ns.point.cols.list))

  #brownfields
  print("BRO")
  state.brownfields.sf <- brownfields.sf %>%
    filter(STATE_CODE ==state.abb) %>%
    st_set_geometry("geometry") %>%
    st_transform(x = .,crs = the.crs) %>%
    mutate(id         = as.character(REGISTRY_ID),
           name       = as.character(PRIMARY_NAME),
           address    = as.character(LOCATION_ADDRESS), #no address field on substations data
           city       = as.character(CITY_NAME),
           state      = as.character(STATE_CODE),
           zipcode    = as.character(POSTAL_CODE),
           provenance = as.character(bro.code),
           ftype      = as.character(NA),
           fcode      = as.character(NA),
           fcode_desc = as.character(NA),
           ftype_desc = as.character(NA),
           infrastructure_type = as.character("BRO")) %>%
    select(all_of(ns.point.cols.list))

  #affordable housing
  print("AFF")
  state.aff.sf <- aff.sf %>%
    filter(State == state.abb) %>%
    st_set_geometry("geometry") %>%
    st_transform(x = .,crs = the.crs) %>%
    mutate(id         = as.character(NHPDPropertyID),
           name       = as.character(PropertyName),
           address    = as.character(PropertyAddress),
           city       = as.character(City),
           state      = as.character(State),
           zipcode    = as.character(Zip),
           provenance = as.character(aff.code),
           ftype      = as.character(NA),
           fcode      = as.character(NA),
           fcode_desc = as.character(NA),
           ftype_desc = as.character(NA),
           infrastructure_type = as.character("AFF")) %>%
    select(all_of(ns.point.cols.list))

  combined.sf <-  rbind(state.ns.sf, state.va.sf ,state.ph.sf, state.wwtp.sf, state.tri.sf, state.substations.sf, state.plants.sf,state.brownfields.sf, state.aff.sf) %>%
                  mutate(name    = stringr::str_to_title(name), #change all text to title case
                         address = stringr::str_to_title(address),
                         city    = stringr::str_to_title(city),
                         zipcode = substring(zipcode, 1,5))

  #nrow(combined.sf)
  #nrow(combined.not.in.leveed.areas.sf)

  #keep only infrastructure within coastal county subdivisions
  state.cs.sf <- get.county.subdivision.sf(state.abb)
  combined.sf <- combined.sf %>%
                 st_join(x    = .,
                         y    = state.cs.sf,
                         join = st_within,
                         left = FALSE) %>%
                left_join(x = .,
                          y = coastal.counties.for.join.df,
                          by = c("county.fips" = "countyfips")) %>%
                 rename(county.sub.geoid = GEOID,
                        county.sub.name  = NAMELSAD) %>%
                 select(-c(state_geoi, abb, geog, COUSUBNS))

  #add flag for infrastructure located within or outside leveed areas
  regions.without.leveed.areas.sf <- st_transform(regions.without.leveed.areas.sf, the.crs)
  state.regions.without.leveed.areas.sf <- regions.without.leveed.areas.sf %>%
                                           filter(stateusps == state.abb) %>%
                                           mutate(inside_leveed_area = "n") %>%
                                           select(inside_leveed_area)
  combined.not.in.leveed.areas.sf <- st_join(x    = combined.sf,
                                             y    = state.regions.without.leveed.areas.sf,
                                             join = st_within,
                                             left = TRUE) %>%
                                    mutate(inside_leveed_area = if_else(is.na(inside_leveed_area),"y",inside_leveed_area))

  combined.sf <- combined.not.in.leveed.areas.sf

  combined.sf <- combined.sf %>%
                 #if PR, VI, AS, or GU, set the leveed areas flag to no
                 mutate(inside_leveed_area = if_else(state %in% c("AS", "PR", "VI", "GU"), "n", inside_leveed_area)) %>%
                 filter(id %notin% c(manual.nat.ids.to.exclude, nat.ids.to.exclude)) #remove duplicate VA hospitals

  write_sf(combined.sf, dsn=paste(infrastructure.output.dir,"/", state.abb, "_critical_infrastructure.gpkg",sep=""), driver="GPKG", delete_layer = TRUE)
}
stopCluster(cl)

#calculate infrastructure totals
ci.list <- list.files(infrastructure.output.dir, pattern = "*.gpkg$", full.names = TRUE)
all.ci.df <- read_sf(ci.list[1]) %>% st_drop_geometry()
for(ci in ci.list[2:length(ci.list)])
{
  print(ci)
  current.ci.df <- read_sf(ci) %>% st_drop_geometry()
  all.ci.df <- rbind(all.ci.df, current.ci.df)
}

# national frequency summary ----------------------------------------------
#first get Superfund sites frequency summary
#prep county subdivisions for spatial filtering
cs.coastal.conus.for.npl.sf <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_conus.gpkg",sep="")) %>% st_transform(., st_crs(4326))
cs.coastal.pr.for.npl.sf    <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_pr.gpkg",sep=""))    %>% st_transform(., st_crs(4326))
cs.coastal.vi.for.npl.sf    <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_vi.gpkg",sep=""))    %>% st_transform(., st_crs(4326))
cs.coastal.gu.for.npl.sf    <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_gu.gpkg",sep=""))    %>% st_transform(., st_crs(4326))
cs.for.npl.sf               <- rbind(cs.coastal.conus.for.npl.sf, cs.coastal.pr.for.npl.sf, cs.coastal.vi.for.npl.sf, cs.coastal.gu.for.npl.sf)
#spatially filter in NPS sites within coastal county subdivisions
all.npl.national.summary <- npl.sf %>%
                            st_join(x    = .,
                                   y    = cs.for.npl.sf,
                                   join = st_within,
                                   left = FALSE) %>%
                            st_drop_geometry() %>%
                            summarize(infrastructure_type = "SUP", frequency = n())

#now create national frequency summary
all.ci.national.summary    <- all.ci.df %>%
                       group_by(infrastructure_type) %>%
                       summarise(frequency = n()) %>%
                       rbind(., all.npl.national.summary) %>%
                       arrange(infrastructure_type)
write.csv(all.ci.national.summary,    paste(output.dir,"critical_infrastructure_frequencies_national.csv",sep="/"), row.names = FALSE)

# state frequency summary ----------------------------------------------------------------------
all.ci.formatted.state <- all.ci.df %>%
                          group_by(state, infrastructure_type) %>%
                          summarise(frequency = n()) %>%
                          pivot_wider(id_cols = c(state), names_from = infrastructure_type, values_from = frequency) %>%
                          mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
                          arrange(state)

# Superfund site frequency by state ----------------------------------------------------------------------
all.npl.df <- npl.sf %>%
              st_join(x    = .,
                y    = cs.for.npl.sf,
                join = st_within,
                left = FALSE) %>%
              st_drop_geometry()
all.npl.formatted.state <-  all.npl.df %>%
                            group_by(STATE_CODE) %>%
                            summarise(SUP = n())

all.npl.formatted.state <- (sort(all.npl.formatted.state,"STATE_CODE"))
all.formatted.state <- cbind(all.ci.formatted.state, all.npl.formatted.state)
write.csv(all.formatted.state, paste(output.dir,"critical_infrastructure_frequencies_state.csv",sep="/"), row.names = TRUE)

end.time <- Sys.time()
elapsed.time <- end.time - start.time


