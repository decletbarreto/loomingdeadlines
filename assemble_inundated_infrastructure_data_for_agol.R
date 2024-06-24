source("~/coastal_deadline/scripts/configuration.R")
#critical infrastructure maps

#critical infrastructure to join type back
ci.df <- read_sf(paste(output.dir, "/agol/all_critical_infrastructure",sep="")) %>% st_drop_geometry() %>% select(all_of(c("id", "type"))) %>% distinct(id, .keep_all = TRUE)

##inundated infra not including superfund ----
inundated.list <- list.files(paste(output.dir, "/inundated_named_infrastructure",sep=""), pattern=".gpkg$", full.names = TRUE)
#all.inundated.sf <- read_sf(inundated.list[1]) %>% st_transform(., 4326)
all.inundated.sf <- st_sf(data.frame(), geometry = st_sfc(),crs = st_crs(4326)) #empty sf with correct crs
#inundated <- inundated.list[1]
for(inundated in inundated.list)
{
  inundated.sf     <- read_sf(inundated) %>% st_transform(., 4326)

  base.filename <- file_path_sans_ext(basename(inundated))
  base.string <- substr(base.filename, 16,nchar(base.filename))
  year.string <- substr(base.string,1,4)
  scenario.base.string <- substr(base.string, 6,nchar(base.string))
  scenario.index.end <- gregexpr("[0-9]", scenario.base.string)[[1]][1] #grab index of first occurrence of number
  scenario.string    <-substr(scenario.base.string, 1,  scenario.index.end - 2)
  frequency.string <- substr(base.string, scenario.index.end + 5 , scenario.index.end + 6)
  print(paste(inundated, year.string, scenario.string, frequency.string, sep="|"))
  inundated.sf <- inundated.sf %>%
                  mutate(year = year.string,
                         scenario = scenario.string,
                         inundation.frequency = frequency.string)

  all.inundated.sf <- rbind(all.inundated.sf, inundated.sf)
}
all.inundated.df <- all.inundated.sf %>%
                    mutate(lat = st_coordinates(.)[, "Y"],
                           lon = st_coordinates(.)[, "X"]) %>%
                    st_drop_geometry() %>%
                    select(all_of(c("id", "name","address","city","state","zipcode", "lat", "lon")))

#join infrastructure type from critical infrastructure b/c i didn´t bring it in the state summaries scripts.
all.inundated.type.join.sf <- all.inundated.sf %>%
                              sp::merge(x = .,
                                        y = ci.df,
                                        by = "id",
                                        duplicateGeoms=TRUE,
                                        all.x = TRUE)

all.inundated.type.wide <- all.inundated.type.join.sf %>%
                           mutate(scenario = recode(scenario,
                                                    "high"    = "hi",
                                                    "int_low" = "il",
                                                    "int"     = "in")) %>%
                           mutate (ysf = paste(year, scenario, inundation.frequency, sep="-"),
                                   row.number = row_number()) %>%
                           st_drop_geometry()                 %>%
                           pivot_wider(., id_cols = id, names_from = ysf, values_from = row.number, values_fn = list(row.number = length)) %>%
                           #mutate_if(is.numeric, ~ifelse(. > 0, 1, .))    %>% #recode frequency of combination to a 1
                           mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% #recode NAs to zero  #nrow 12989
                           inner_join(x  = .,  #join name, address, city, state, zip, lat, lon
                                      y  = all.inundated.df,
                                      by = "id",
                                      multiple = "first") %>%
                           inner_join(x  = .,
                                      y  = ci.df,
                                      by = "id",
                                      multiple = "first") %>%
                           st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
                           select(id,type, name, address, city, zipcode, state, everything()) %>%
                           mutate(type = case_when(
                              type == "BRO" ~ "Brownfields",
                              type == "PH" ~ "Public Housing",
                              type == "POW" ~ "Power Plant",
                              type == "SUB" ~ "Electrical Substation",
                              type == "TRI" ~ "Toxics Release Inventory Site",
                              type == "VA" ~ "Veterans' Administration Hospital",
                              type == "WT" ~ "Wastewater Treatment Plant",
                              type == "K12" ~ "K-12 School",
                              type == "COL" ~ "College, University, Technical or Trade School",
                              type == "AMB" ~ "Ambulance Service",
                              type == "FIR" ~ "Fire Station",
                              type == "LAW" ~ "Law Enforcement",
                              type == "PRI" ~ "Prison or Jail",
                              type == "POS" ~ "Post Office",
                              type == "HOS" ~ "Hospital",
                              type == "SCAP" ~ "State Capitol",
                              type == "SSC" ~ "State Supreme Court",
                              type == "COR" ~ "Courthouse",
                              type == "HQ"  ~ "Nat'l Park Headquarters",
                              type == "RNGR" ~ "Nat'l Park Ranger Station",
                              type == "DPW" ~ "City Hall",
                              type == "AFF" ~ "Affordable Housing",
                              type == "SUP" ~ "Superfund Site",
                              type == "AFF" ~ "Affordable Housing",
                              TRUE ~ "Other"
                            )) #%>%
                          #write_sf(., dsn = paste(output.dir, "/agol/inundated_critical_infrastructure", sep=""), driver="ESRI Shapefile", delete_layer = TRUE)


##inundated superfund sites ----
inundated.npl.list <- list.files(paste(output.dir, "/npl_inundated",sep=""), pattern=".gpkg$", full.names = TRUE)
#all.inundated.sf <- read_sf(inundated.list[1]) %>% st_transform(., 4326)
all.inundated.npl.sf <- st_sf(data.frame(), geometry = st_sfc(),crs = st_crs(4326)) #empty sf with correct crs
#inundated <- inundated.list[1]
for(inundated.npl in inundated.npl.list)
{
  inundated.npl.sf     <- read_sf(inundated.npl) %>% st_transform(., 4326)
  base.filename <- file_path_sans_ext(basename(inundated.npl))
  base.string <- substr(base.filename, 16,nchar(base.filename))
  year.string <- substr(base.string,1,4)
  scenario.base.string <- substr(base.string, 6,nchar(base.string))
  scenario.index.end <- gregexpr("[0-9]", scenario.base.string)[[1]][1] #grab index of first occurrence of number
  scenario.string    <-substr(scenario.base.string, 1,  scenario.index.end - 2)
  frequency.string <- substr(base.string, scenario.index.end + 5 , scenario.index.end + 6)
  print(paste(inundated, year.string, scenario.string, frequency.string, sep="|"))
  inundated.npl.sf <- inundated.npl.sf %>%
    mutate(year = year.string,
           scenario = scenario.string,
           inundation.frequency = frequency.string)

  all.inundated.npl.sf <- rbind(all.inundated.npl.sf, inundated.npl.sf)
}

all.inundated.npl.centroids.df <- all.inundated.npl.sf %>%
                                  st_centroid(.) %>%
                                  mutate(lat = st_coordinates(.)[, "Y"],
                                         lon = st_coordinates(.)[, "X"]) %>%
                                  select(EPA_ID, SITE_NAME, STREET_ADDR_TXT, CITY_NAME, STATE_CODE, ZIP_CODE, lat,lon) %>%
                                  distinct(EPA_ID, .keep_all = T) %>%
                                  st_drop_geometry()

all.inundated.npl.df <- all.inundated.npl.sf %>%
                    st_drop_geometry() %>%
                    select("EPA_ID", "SITE_NAME","STREET_ADDR_TXT","CITY_NAME","STATE_CODE","ZIP_CODE", everything()) %>%
                    mutate(SITE_NAME = stringr::str_to_title(SITE_NAME)) %>%
                    mutate(STREET_ADDR_TXT = stringr::str_to_title(STREET_ADDR_TXT)) %>%
                    mutate(CITY_NAME = stringr::str_to_title(CITY_NAME))

all.inundated.npl.type.wide <- all.inundated.npl.df %>%
                              mutate(scenario = recode(scenario,
                                                       "high"    = "hi",
                                                       "int_low" = "il",
                                                       "int"     = "in")) %>%
                              mutate (ysf = paste(year, scenario, inundation.frequency, sep="-"),
                                      row.number = row_number()) %>%
                              st_drop_geometry()                 %>%
                              pivot_wider(., id_cols = EPA_ID, names_from = ysf, values_from = row.number, values_fn = list(row.number = length)) %>%
                              mutate_if(is.numeric, ~ifelse(. > 0, 1, .))    %>% #recode frequency of combination to a 1
                              mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>%
                              left_join(x = .,
                                      y = all.inundated.npl.centroids.df,
                                      by = c("EPA_ID"= "EPA_ID")) %>%
                              rename(id      = EPA_ID,
                                     name    = SITE_NAME,
                                     address = STREET_ADDR_TXT,
                                     city    = CITY_NAME,
                                     state   = STATE_CODE,
                                     zipcode = ZIP_CODE) %>%
                              mutate(type    = "Superfund") %>%
                              select(id, name, type, address, city, state, zipcode,everything()) %>%
                              st_as_sf(., coords = c("lon", "lat"), crs = 4326) %>%
                              write_sf(., dsn = paste(output.dir, "/agol/inundated_npl", sep=""), driver="ESRI Shapefile", delete_layer = TRUE)

##zip it up -----
shp.zip <- paste(output.dir, "/agol/inundated_critical_infrastructure.zip",sep="")
shps <- list.files(path = paste(output.dir, "/agol/inundated_critical_infrastructure",sep=""), pattern= "inundated_critical_infrastructure*", full.names = T)
zip(shp.zip,shps)




