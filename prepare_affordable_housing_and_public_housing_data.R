rm(list=ls())

library(dplyr)
library(sf)
library(fedmatch)
states.in.analysis       <- c("AL", "CA", "CT", "DC", "DE", "FL", "GA", "GU", "LA", "MA", "MD", "ME", "MS","NC","NH","NJ","NY","OR","PA","PR","RI","SC","TX","VA","VI","WA")
ph.cols.list             <- c("PROJECT_NA", "STD_ADDR", "STD_CITY", "STD_ST", "STD_ZIP5") #Columns list for public housing data
data.dir <- "~/coastal_deadline/data"
output.dir                <- paste(data.dir,"output",sep="/")
census.output.dir         <- paste(output.dir,"census",sep="/")

cs.conus.sf <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_conus.gpkg",sep="")) %>% st_transform(., st_crs(4326))
cs.pr.sf    <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_pr.gpkg",sep=""))    %>% st_transform(., st_crs(4326))
cs.vi.sf    <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_vi.gpkg",sep=""))    %>% st_transform(., st_crs(4326))
cs.gu.sf    <- read_sf(dsn=paste(census.output.dir, "/county_subdivisions_gu.gpkg",sep=""))    %>% st_transform(., st_crs(4326))
cs.sf      <- rbind(cs.conus.sf, cs.pr.sf, cs.vi.sf, cs.gu.sf)
`%notin%` <- Negate(`%in%`)

coastal.counties.sf   <- sf::read_sf(dsn=paste(data.dir,"/Census/CoastalCounties.gdb",sep=""), layer="CoastalCounties_official") %>%
  dplyr::filter(stateusps %in% states.in.analysis)
coastal.counties.fips <- unique(coastal.counties.sf$countyfips)

#AFF
#https://preservationdatabase.org/wp-content/uploads/2022/01/Data-Dictionary.pdf
#https://preservationdatabase.org/wp-content/uploads/2024/01/Data-Sources.pdf
#https://preservationdatabase.org/wp-content/uploads/2017/09/User-Guide-9.20.17.pdf


aff.df <- read.csv(paste(data.dir,"/national_housing_preservation/properties_with_active_subsidies.csv", sep="")) %>%
          select(NHPDPropertyID, PropertyName, PropertyAddress, City, State, Zip, Latitude, Longitude) %>%
          filter(State %in% states.in.analysis) %>%
          filter(!is.na(Latitude))  %>%
          filter(!is.na(Longitude)) %>%
          filter(!(Latitude ==""))  %>%
          filter(!(Longitude =="")) %>%
          st_as_sf(x      = .,
                   coords = c("Longitude", "Latitude"),
                   crs    = 4326) %>%
          st_join(x    = ., #keep only AFF within coastal county subdivisions
                  y    = cs.sf,
                  join = st_within,
                  left = FALSE)

#write_sf(obj = aff.df, dsn=paste(data.dir,"/national_housing_preservation/nhp_properties.gpkg",sep=""), driver="GPKG", delete_layer = TRUE)

ph.query <- paste(paste("SELECT ", paste(c("FID", ph.cols.list), collapse=","), " FROM PUBLIC_HOUSING_BUILDING", sep="")) #query
ph.sf    <- read_sf(dsn=paste(data.dir, "HUD",sep="/"), query = ph.query) %>%
            distinct() %>% #eliminate duplicate geometries
            filter(STD_ST %in% states.in.analysis) %>%
            st_join(x    = ., #keep only PH within coastal county subdivisions
                  y    = cs.sf,
                  join = st_within,
                  left = FALSE) %>%
            select(-FID)

all.aff.ids.to.exclude <- c()

#state <- states.in.analysis[1]
for(state in states.in.analysis)
{
  print(state)
  #open PH data
  ph.address <- ph.sf %>%
    st_drop_geometry() %>%
    mutate(full.address = paste(STD_ADDR, STD_CITY, STD_ST,STD_ZIP5,sep=" "),
           ph.id = paste("PH", row_number(),sep="")) %>%
    select(ph.id, full.address, STD_ST) %>%
    filter(STD_ST == state) #filter to state

  ph.address$full.address <- clean_strings(ph.address$full.address) #clean addresses

  #open AFF data
  aff.address <- aff.df %>%
    st_drop_geometry() %>%
    mutate(Zip = substr(Zip,1,5)) %>% #ZIP data in this file has ZIP+4. Truncate it.
    mutate(full.address = paste(PropertyAddress, City, State, Zip, sep=" "),
           aff.id = as.character(NHPDPropertyID)) %>%
    select(aff.id, full.address, State) %>%
    filter(State == state) #filter to state
  aff.address$full.address <- clean_strings(aff.address$full.address) #clean addresses

  #remove apartment number info
  aff.address$full.address <-  str_replace_all(aff.address$full.address, "\\bapt\\s*(\\w+)\\b", "") #this pattern removes the string 'apt' and a string containing an apt number
  ph.address$full.address  <-  str_replace_all(ph.address$full.address,  "\\bapt\\s*(\\w+)\\b", "")

  ph.address$full.address  <- clean_strings(ph.address$full.address) #clean addresses again to remove spaces
  aff.address$full.address <- clean_strings(aff.address$full.address)

  fuzzy.result <- merge_plus(data1 = aff.address,
                             data2 = ph.address,
                             by.x = "full.address",
                             by.y = "full.address",
                             match_type = "fuzzy",
                             fuzzy_settings = build_fuzzy_settings(maxDist = .05,method = "lv"),
                             unique_key_1 = "aff.id",
                             unique_key_2 = "ph.id"
  )
  aff.ids.to.exclude <- fuzzy.result$matches$aff.id #these points from AFF will be excluded in order to keep the same ones from PH
  #aff.address <- aff.address %>% filter(aff.id %notin% aff.ids.to.exclude) #exclude AFFs duplicated in PH
  all.aff.ids.to.exclude <- c(all.aff.ids.to.exclude, aff.ids.to.exclude) #one df to bind them all

  #View(fuzzy_result$matches)
  aff.address.nomatch <- fuzzy.result$data1_nomatch %>% rename(id = aff.id) %>% mutate(provenance = "AFF")
  ph.address.nomatch  <- fuzzy.result$data2_nomatch %>% rename(id = ph.id, State = STD_ST) %>% mutate(provenance = "PH")
  #View(rbind(aff.address.nomatch, ph.address.nomatch))
}

#write final GPKGs
aff.final.df <- aff.df %>%
               select(NHPDPropertyID, PropertyName, PropertyAddress,City, State, Zip) %>%
               mutate(NHPDPropertyID = as.character(NHPDPropertyID)) %>%
               filter(NHPDPropertyID %notin% all.aff.ids.to.exclude) %>%  #exclude AFF points found to be duplicative of PH
               write_sf(obj = ., dsn=paste(data.dir,"/national_housing_preservation/nhp_properties.gpkg",sep=""), driver="GPKG", delete_layer = TRUE)

aff.df %>%
  select(NHPDPropertyID, PropertyName, PropertyAddress,City, State, Zip) %>%
  mutate(NHPDPropertyID = as.character(NHPDPropertyID)) %>%
  filter(NHPDPropertyID %in% all.aff.ids.to.exclude) %>%  #exclude AFF points found to be duplicative of PH
  write_sf(obj = ., dsn=paste(data.dir,"/national_housing_preservation/nhp_properties_duplicated.gpkg",sep=""), driver="GPKG", delete_layer = TRUE)

ph.sf %>%
      select(PROJECT_NA, STD_ADDR, STD_CITY, STD_ST, STD_ZIP5) %>%
      st_set_geometry(., "geometry") %>%
      write_sf(obj = ., dsn=paste(data.dir,"/HUD/public_housing_buildings_processed.gpkg",sep=""), driver="GPKG", delete_layer = TRUE)

