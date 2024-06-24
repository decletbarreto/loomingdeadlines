rm(list=ls())

library(dplyr)
library(sf)
library(fedmatch)
states.in.analysis       <- c("AL", "CA", "CT", "DC", "DE", "FL", "GA", "GU", "LA", "MA", "MD", "ME", "MS","NC","NH","NJ","NY","OR","PA","PR","RI","SC","TX","VA","VI","WA")
ph.cols.list             <- c("PROJECT_NA", "STD_ADDR", "STD_CITY", "STD_ST", "STD_ZIP5") #Columns list for public housing data
data.dir <- "~/coastal_deadline/data"
output.dir                <- paste(data.dir,"output",sep="/")
census.output.dir         <- paste(output.dir,"census",sep="/")
infrastructure.output.dir <- paste(data.dir, "/output/coastal_infrastructure", sep="")

coastal.counties.sf   <- sf::read_sf(dsn=paste(data.dir,"/Census/CoastalCounties.gdb",sep=""), layer="CoastalCounties_official") %>%
  dplyr::filter(stateusps %in% states.in.analysis)
coastal.counties.fips <- unique(coastal.counties.sf$countyfips)

#Open VA data
va.sf <- read_sf(dsn=paste(data.dir,"VA_hospitals", sep="/"), layer="Veterans_Health_Administration_Medical_Facilities") %>%
  mutate(VA_ID = UNIQUE_ID) %>% #rename VA ID
  filter(FIPS %in% coastal.counties.fips) #subset only VA hospitals in coastal counties

#Open NAT data and grab those with hospital FCODEs
state <- "SC"
all.nat.ids.to.exclude <- c()
all.addresses.df <- c()
all.addresses.no.match <- c()
for(state in states.in.analysis)
{
  print(state)
  curr.nat.sf <- read_sf(paste(infrastructure.output.dir, "/", state, "_critical_infrastructure.gpkg",sep="")) %>%
                 filter(fcode_desc == "Hospital/Medical Center") %>%
                 mutate(nat.id = id,
                        nat.name = name,
                        full.address = paste(address, city, state, sep=" "),
                        provenance = "NAT")

  nat.address <- curr.nat.sf %>%
                 st_drop_geometry() %>%
                 select(nat.id, nat.name,full.address, provenance)

  curr.va.sf <- va.sf %>%
                filter(STATE == state) %>%
                mutate(va.id = UNIQUE_ID,
                       va.name = NAME,
                       full.address = paste(ADDRESS, CITY, STATE, sep=" "),
                       provenance = "VA")

  va.address <- curr.va.sf %>%
                st_drop_geometry() %>%
                select(va.id, va.name, full.address, provenance)

  nat.address$full.address  <- clean_strings(nat.address$full.address) #clean addresses again to remove spaces
  va.address$full.address   <- clean_strings(va.address$full.address)

  nat.address2 <- nat.address %>% rename(id = nat.id, name = nat.name) %>% mutate(provenance = "NAT")
  va.address2  <- va.address  %>% rename(id = va.id,  name = va.name) %>% mutate(provenance = "VA")
  all.addresses.df <- rbind(all.addresses.df, nat.address2,va.address2)

  fuzzy.result <- merge_plus(data1 = nat.address,
                             data2 = va.address,
                             by.x = "full.address",
                             by.y = "full.address",
                             match_type = "fuzzy",
                             fuzzy_settings = build_fuzzy_settings(maxDist = .05,method = "lv"),
                             unique_key_1 = "nat.id",
                             unique_key_2 = "va.id"
  )

  curr.nat.address <- fuzzy.result$data1_nomatch %>% rename(id = nat.id, name = nat.name) %>% mutate(provenance = "NAT")
  va.address.nomatch  <- fuzzy.result$data2_nomatch %>% rename(id = va.id, name = va.name ) %>% mutate(provenance = "VA")

  nat.ids.to.exclude <- fuzzy.result$matches$nat.id #these points from NAT will be excluded in order to keep the same ones from VA
  all.nat.ids.to.exclude <- c(all.nat.ids.to.exclude, nat.ids.to.exclude) #one df to bind them all

  nat.address.nomatch <- fuzzy.result$data1_nomatch %>% rename(id = nat.id, name = nat.name) %>% mutate(provenance = "NAT")
  va.address.nomatch  <- fuzzy.result$data2_nomatch %>% rename(id = va.id, name = va.name ) %>% mutate(provenance = "VA")
  all.addresses.no.match <- rbind(all.addresses.no.match, nat.address.nomatch, va.address.nomatch)
}
#replicated in configuration.R
manual.nat.ids.to.exclude <- c("6625a2f9-1163-49af-94df-b34be7cce8f4",  #same address but city is Sepulveda/North Hills CA
                               "0aa19402-a60a-495e-b465-0484958f6abd")  #same address but city is La Jolla/San Diego CA
nat.ids.to.exclude <- c("2803f503-34f7-4d8b-82aa-ae55777addab", "{FA4DCCE1-4B6A-466E-9DB0-CF4FBD78D041}", "807bc9f6-79be-4fdd-bf12-ed3db431940c",   "0e42dae2-927d-4877-aabf-f310b27e1abd",
                        "{1F00C9A7-5B53-4036-8C43-74D7832A336E}", "c3cd4ae4-3b7a-41fa-b351-aeec36a78fd5")

