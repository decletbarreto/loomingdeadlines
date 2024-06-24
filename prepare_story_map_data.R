rm(list=ls())
source("~/coastal_deadline/scripts/configuration.R")
library(sf)
library(dplyr)
library(openxlsx)

public.affordable.housing <- c("Affordable Housing", "Public Housing")
ed <- c("K-12 School","College, University, Technical or Trade School")
public.safety <- c("Ambulance Service","Fire Station","Hospital", "Veterans' Administration Hospital","Law Enforcement", "Wastewater Treatment Plant")
industrial <- c("Brownfields","Superfund", "Toxics Release Inventory Site")
energy     <- c("Power Plant", "Electrical Substation")
government <- c("Courthouse","City Hall", "Post Office","Nat'l Park Ranger Station", "Prison or Jail", "Nat'l Park Headquarters")

ci.sf <- read_sf(paste(output.dir, "/agol/all_critical_infrastructure",sep="")) %>% mutate(key = paste(id, name, address, state, sep="|"))
ci.for.join.df <- ci.sf %>% st_drop_geometry() %>% mutate(key = paste(id, name, address, city, state,sep="|")) %>% select(id, type,cs_name,county,key) %>% distinct(key, .keep_all = T)

#assemble all inundated CI + NPL
# first CI no NPL
inundated.list  <- list.files(paste(output.dir, "/inundated_named_infrastructure",sep=""), pattern=".gpkg$", full.names = TRUE)
inundated.ci.sf <- st_sf(data.frame(), geometry = st_sfc(),crs = st_crs(4326)) #empty sf with correct crs
for(inundated in inundated.list)
{
  curr.inundated.ci.sf     <- read_sf(inundated) %>% st_transform(., 4326)
  base.filename <- file_path_sans_ext(basename(inundated))
  base.string <- substr(base.filename, 16,nchar(base.filename))
  year.string <- substr(base.string,1,4)
  scenario.base.string <- substr(base.string, 6,nchar(base.string))
  scenario.index.end <- gregexpr("[0-9]", scenario.base.string)[[1]][1] #grab index of first occurrence of number
  scenario.string    <-substr(scenario.base.string, 1,  scenario.index.end - 2)
  frequency.string <- substr(base.string, scenario.index.end + 5 , scenario.index.end + 6)
  print(paste(inundated, year.string, scenario.string, frequency.string, sep="|"))
  curr.inundated.ci.sf <- curr.inundated.ci.sf %>%
                          mutate(year = year.string,
                                 scenario = scenario.string,
                                 inundation.frequency = frequency.string)
  inundated.ci.sf <- rbind(inundated.ci.sf, curr.inundated.ci.sf)
}

#now NPL
inundated.npl.list <- list.files(paste(output.dir, "/npl_inundated",sep=""), pattern=".gpkg$", full.names = TRUE)
inundated.npl.sf   <- st_sf(data.frame(), geometry = st_sfc(),crs = st_crs(4326)) #empty sf with correct crs
for(inundated.npl in inundated.npl.list)
{
  curr.inundated.npl.sf     <- read_sf(inundated.npl) %>% st_transform(., 4326)
  base.filename <- file_path_sans_ext(basename(inundated.npl))
  base.string <- substr(base.filename, 16,nchar(base.filename))
  year.string <- substr(base.string,1,4)
  scenario.base.string <- substr(base.string, 6,nchar(base.string))
  scenario.index.end <- gregexpr("[0-9]", scenario.base.string)[[1]][1] #grab index of first occurrence of number
  scenario.string    <-substr(scenario.base.string, 1,  scenario.index.end - 2)
  frequency.string <- substr(base.string, scenario.index.end + 5 , scenario.index.end + 6)
  print(paste(inundated, year.string, scenario.string, frequency.string, sep="|"))
  curr.inundated.npl.sf <- curr.inundated.npl.sf %>%
    mutate(year = year.string,
           scenario = scenario.string,
           inundation.frequency = frequency.string)
  inundated.npl.sf <- rbind(inundated.npl.sf, curr.inundated.npl.sf)
}

#merge inundated CI non NPL with NPL
#first get NPL centroids
inundated.npl.centroids.sf <- inundated.npl.sf %>%
                              st_centroid() %>%
                              rename(id      = EPA_ID,
                                     name    = SITE_NAME,
                                     address = STREET_ADDR_TXT,
                                     city    = CITY_NAME,
                                     state   = STATE_CODE,
                                     zipcode = ZIP_CODE) %>%
                              mutate(provenance    = "SUP") %>%
                              select(id, name, address, city, state, zipcode, provenance, year, scenario, inundation.frequency)
#merge
inundated.ci.plus.npl.sf <- rbind(inundated.ci.sf, inundated.npl.centroids.sf) %>%
                            mutate(lat = st_coordinates(.)[, "Y"],
                                   lon = st_coordinates(.)[, "X"]) %>%
                            mutate(key   = paste(id, name, address, city, state,sep="|")) %>%
                            st_drop_geometry() %>%
                            left_join(x  =.,
                                       y  =  ci.for.join.df[,c("key","type")],
                                       by = "key") %>%
                            mutate(type = if_else(provenance =="AFF","AFF", type)) %>%
                            mutate(type = if_else(provenance =="BRO","BRO", type)) %>%
                            mutate(type = if_else(provenance =="SUP","SUP", type)) %>%
                            mutate(type = if_else(provenance =="WT" ,"WT",  type)) %>%
                            mutate(scenario = recode(scenario,
                                                     "high"    = "hi",
                                                     "int_low" = "il",
                                                     "int"     = "in")) %>%
                            mutate (ysf = paste(year, scenario, inundation.frequency, sep="-"))

inundated.ci.plus.npl.normalized.text.sf <- inundated.ci.plus.npl.sf %>% st_drop_geometry() %>%
                                            mutate(name = tools::toTitleCase(tolower(name)),
                                                   city = tools::toTitleCase(tolower(city))) %>%
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
                                              TRUE ~ "Other"))

inundated.ci.plus.npl.final <- inundated.ci.plus.npl.normalized.text.sf %>%
                                             mutate(type_r = type) %>%
                                             mutate(type_r = ifelse(type_r %in% public.affordable.housing, "Public or affordable housing",type_r)) %>%
                                             mutate(type_r = ifelse(type_r %in% ed, "Educational institution",type_r))                             %>%
                                             mutate(type_r = ifelse(type_r %in% public.safety, "Public safety or health facility",type_r))         %>%
                                             mutate(type_r = ifelse(type_r %in% industrial, "Industrial contamination site",type_r))               %>%
                                             mutate(type_r = ifelse(type_r %in% energy, "Energy infrastructure",type_r))                           %>%
                                             mutate(type_r = ifelse(type_r %in% government, "Government facility",type_r))
#table(inundated.ci.plus.npl.normalized.text.sf2$type_r, useNA = 'always')

#create public spreadsheets for all years, all data -----
#spreadsheet filenames
spreadsheet.2020.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2020.xlsx",sep="")
spreadsheet.2030.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2030.xlsx",sep="")
spreadsheet.2050.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2050.xlsx",sep="")
spreadsheet.2100.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2100.xlsx",sep="")

#2020 ------
spreadsheet.2020.wb <- createWorkbook()
#low scen X floods per year
y2020.tabs <- c("2020-hi-02", "2020-hi-12", "2020-hi-26",
                "2020-in-02", "2020-in-12", "2020-in-26",
                "2020-il-02", "2020-il-12", "2020-il-26")
y2020.tabs.label<- c("high scen 2 floods per year", "high scen 12 floods per year", "high scen 26 floods per year",
                     "med scen 2 floods per year",  "med scen 12 floods per year",  "med scen 26 floods per year",
                     "low scen 2 floods per year",  "low scen 12 floods per year",  "low scen 26 floods per year")

for(i in seq(1: length(y2020.tabs)))
{
  inundated.ci.2020.df <- inundated.ci.plus.npl.final %>%
                        st_drop_geometry() %>%
                        filter(ysf == y2020.tabs[i]) %>%
                        select(name, address, city, state, zipcode, type, type_r, lat,lon) %>%
                        rename(Name = name,
                               Address = address,
                               City = city,
                               State = state,
                               Zip = zipcode,
                               `Infrastructure category`    = type_r,
                               `Infrastructure subcategory` = type)

  addWorksheet(spreadsheet.2020.wb, sheetName = y2020.tabs.label[i])
  writeDataTable(spreadsheet.2020.wb, sheet = y2020.tabs.label[i],x = inundated.ci.2020.df)
  saveWorkbook(spreadsheet.2020.wb, file = spreadsheet.2020.filename, overwrite = TRUE)
}

##2030 ----
spreadsheet.2030.wb <- createWorkbook()
#low scen X floods per year
y2030.tabs <- c("2030-hi-02", "2030-hi-12", "2030-hi-26",
                "2030-in-02", "2030-in-12", "2030-in-26",
                "2030-il-02", "2030-il-12", "2030-il-26")
y2030.tabs.label<- c("high scen 2 floods per year", "high scen 12 floods per year", "high scen 26 floods per year",
                     "med scen 2 floods per year",  "med scen 12 floods per year",  "med scen 26 floods per year",
                     "low scen 2 floods per year",  "low scen 12 floods per year",  "low scen 26 floods per year")

for(i in seq(1: length(y2030.tabs)))
{
  inundated.ci.2030.df <- inundated.ci.plus.npl.final %>%
    st_drop_geometry() %>%
    filter(ysf == y2030.tabs[i]) %>%
    select(name, address, city, state, zipcode, type, type_r, lat,lon) %>%
    rename(Name = name,
           Address = address,
           City = city,
           State = state,
           Zip = zipcode,
           `Infrastructure category`    = type_r,
           `Infrastructure subcategory` = type)

  addWorksheet(spreadsheet.2030.wb, sheetName = y2030.tabs.label[i])
  writeDataTable(spreadsheet.2030.wb, sheet = y2030.tabs.label[i],x = inundated.ci.2030.df)
  saveWorkbook(spreadsheet.2030.wb, file = spreadsheet.2030.filename, overwrite = TRUE)
}

##2050 ----
spreadsheet.2050.wb <- createWorkbook()
#low scen X floods per year
y2050.tabs <- c("2050-hi-02", "2050-hi-12", "2050-hi-26",
                "2050-in-02", "2050-in-12", "2050-in-26",
                "2050-il-02", "2050-il-12", "2050-il-26")
y2050.tabs.label<- c("high scen 2 floods per year", "high scen 12 floods per year", "high scen 26 floods per year",
                     "med scen 2 floods per year",  "med scen 12 floods per year",  "med scen 26 floods per year",
                     "low scen 2 floods per year",  "low scen 12 floods per year",  "low scen 26 floods per year")

for(i in seq(1: length(y2050.tabs)))
{
  inundated.ci.2050.df <- inundated.ci.plus.npl.final %>%
    st_drop_geometry() %>%
    filter(ysf == y2050.tabs[i]) %>%
    select(name, address, city, state, zipcode, type, type_r, lat,lon) %>%
    rename(Name = name,
           Address = address,
           City = city,
           State = state,
           Zip = zipcode,
           `Infrastructure category`    = type_r,
           `Infrastructure subcategory` = type)

  addWorksheet(spreadsheet.2050.wb, sheetName = y2050.tabs.label[i])
  writeDataTable(spreadsheet.2050.wb, sheet = y2050.tabs.label[i],x = inundated.ci.2050.df)
  saveWorkbook(spreadsheet.2050.wb, file = spreadsheet.2050.filename, overwrite = TRUE)
}

##2100 ----
spreadsheet.2100.wb <- createWorkbook()
#low scen X floods per year
y2100.tabs <- c("2100-hi-02", "2100-hi-12", "2100-hi-26",
                "2100-in-02", "2100-in-12", "2100-in-26",
                "2100-il-02", "2100-il-12", "2100-il-26")
y2100.tabs.label<- c("high scen 2 floods per year", "high scen 12 floods per year", "high scen 26 floods per year",
                     "med scen 2 floods per year",  "med scen 12 floods per year",  "med scen 26 floods per year",
                     "low scen 2 floods per year",  "low scen 12 floods per year",  "low scen 26 floods per year")

for(i in seq(1: length(y2100.tabs)))
{
  inundated.ci.2100.df <- inundated.ci.plus.npl.final %>%
    st_drop_geometry() %>%
    filter(ysf == y2100.tabs[i]) %>%
    select(name, address, city, state, zipcode, type, type_r, lat,lon) %>%
    rename(Name = name,
           Address = address,
           City = city,
           State = state,
           Zip = zipcode,
           `Infrastructure category`    = type_r,
           `Infrastructure subcategory` = type)

  addWorksheet(spreadsheet.2100.wb, sheetName = y2100.tabs.label[i])
  writeDataTable(spreadsheet.2100.wb, sheet = y2100.tabs.label[i],x = inundated.ci.2100.df)
  saveWorkbook(spreadsheet.2100.wb, file = spreadsheet.2100.filename, overwrite = TRUE)
}

#2100 lo and hi scenario layers for AGOL -----
y2100.il <- c("2100-il-02", "2100-il-12", "2100-il-26")
y2100.hi <- c("2100-hi-02", "2100-hi-12", "2100-hi-26")

#IL
inundated.ci.2100.il.2x.df <- inundated.ci.plus.npl.final %>%
                              st_drop_geometry() %>%
                              filter(ysf == "2100-il-02")

inundated.ci.2100.il.12x.df <- inundated.ci.plus.npl.final %>%
                               st_drop_geometry() %>%
                               filter(ysf == "2100-il-12")

inundated.ci.2100.il.26x.df <- inundated.ci.plus.npl.final %>%
                               st_drop_geometry() %>%
                               filter(ysf == "2100-il-26")
inundated.ci.2100.il.df <- rbind(inundated.ci.2100.il.2x.df,inundated.ci.2100.il.12x.df,inundated.ci.2100.il.26x.df) %>%
                           select(name, address, city, state, zipcode, type, type_r, ysf, key, lat,lon) %>%
                           rename(Name = name,
                                  Address = address,
                                  City = city,
                                  State = state,
                                  Zip = zipcode,
                                  `Infrastructure category`    = type_r,
                                  `Infrastructure subcategory` = type) %>%
                          mutate(row.number = row_number())

inundated.ci.2100.il.wide.df <- inundated.ci.2100.il.df %>%
                                pivot_wider(., id_cols = key, names_from = ysf, values_from = row.number, values_fn = list(row.number = length)) %>%
                                inner_join(x  = .,
                                          y  = rbind(inundated.ci.2100.il.2x.df,inundated.ci.2100.il.12x.df,inundated.ci.2100.il.26x.df),
                                          by = "key") %>%
                                distinct(key, .keep_all = T) %>%
                                st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # specify the coordinate reference system (CRS)
                                select(name, address, city, state, zipcode, type, type_r, `2100-il-02`, `2100-il-12`, `2100-il-26`) %>%
                                mutate(type_r = ifelse(type =="Superfund Site","Industrial contamination site", type_r))

inundated.ci.2100.il.wide.df %>% write_sf(., dsn="/home/rama/coastal_deadline/data/output/agol/story_map", layer="inundated_critical_infrastructure_2100_intermediate_low", driver="ESRI Shapefile", delete_layer = T)

#3. zip it up
shp.zip <- "/home/rama/coastal_deadline/data/output/agol/story_map/inundated_critical_infrastructure_2100_intermediate_low.zip"
file.remove(shp.zip)
shps    <- list.files(path = "/home/rama/coastal_deadline/data/output/agol/story_map", pattern= "inundated_critical_infrastructure_2100*", full.names = T)
zip(shp.zip,shps)

#HI
inundated.ci.2100.hi.2x.df <- inundated.ci.plus.npl.final %>%
  st_drop_geometry() %>%
  filter(ysf == "2100-hi-02")

inundated.ci.2100.hi.12x.df <- inundated.ci.plus.npl.final %>%
  st_drop_geometry() %>%
  filter(ysf == "2100-hi-12")

inundated.ci.2100.hi.26x.df <- inundated.ci.plus.npl.final %>%
  st_drop_geometry() %>%
  filter(ysf == "2100-hi-26")

inundated.ci.2100.hi.df <- rbind(inundated.ci.2100.hi.2x.df,inundated.ci.2100.hi.12x.df,inundated.ci.2100.hi.26x.df) %>%
  select(name, address, city, state, zipcode, type, type_r, ysf, key, lat,lon) %>%
  rename(Name = name,
         Address = address,
         City = city,
         State = state,
         Zip = zipcode,
         `Infrastructure category`    = type_r,
         `Infrastructure subcategory` = type) %>%
  mutate(row.number = row_number())

inundated.ci.2100.hi.wide.df <- inundated.ci.2100.hi.df %>%
  pivot_wider(., id_cols = key, names_from = ysf, values_from = row.number, values_fn = list(row.number = length)) %>%
  inner_join(x  = .,
             y  = rbind(inundated.ci.2100.hi.2x.df,inundated.ci.2100.hi.12x.df,inundated.ci.2100.hi.26x.df),
             by = "key") %>%
  distinct(key, .keep_all = T) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% # specify the coordinate reference system (CRS)
  select(name, address, city, state, zipcode, type, type_r, `2100-hi-02`, `2100-hi-12`, `2100-hi-26`) %>%
  mutate(type_r = ifelse(type =="Superfund Site","Industrial contamination site", type_r)) %>%
  mutate(type_r = ifelse(type =="State Capitol","Government facility", type_r)) %>%
  mutate(type_r = ifelse(type =="State Supreme Court","Government facility", type_r))

inundated.ci.2100.hi.wide.df %>% write_sf(., dsn="/home/rama/coastal_deadline/data/output/agol/story_map", layer="inundated_critical_infrastructure_2100_hi", driver="ESRI Shapefile", delete_layer = T)

#3. zip it up
shp.zip <- "/home/rama/coastal_deadline/data/output/agol/story_map/inundated_critical_infrastructure_2100_hi.zip"
file.remove(shp.zip)
shps    <- list.files(path = "/home/rama/coastal_deadline/data/output/agol/story_map", pattern= "inundated_critical_infrastructure_2100_hi*", full.names = T)
zip(shp.zip,shps)







inundated.ci.2100.il.wide.df %>% filter(`2100-il-02`==1)
inundated.ci.2100.il.wide.df %>% filter(`2100-il-12`==1)
inundated.ci.2100.il.wide.df %>% filter(`2100-il-26`==1)



#
# inundated.npl.2020.sf <- read_sf(paste(output.dir, "/agol/inundated_npl/inundated_npl.shp", sep="")) %>%
#   mutate(type_r = "Superfund") %>%
#   filter(`2020-in-02` == 1 | `2020-in-12` == 1 | `2020-in-26` == 1 | `2020-il-02` == 1 | `2020-il-12` == 1 | `2020-il-26` == 1 | `2020-hi-02` == 1 | `2020-hi-12` == 1 | `2020-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2020-in-02`, `2020-in-12`, `2020-in-26`, `2020-il-02`, `2020-il-12`, `2020-il-26`, `2020-hi-02`, `2020-hi-12`, `2020-hi-26`)) %>%
#   rename(geom = geometry) %>%
#   mutate(address = str_to_title(address),
#          city    = str_to_title(city))
#
# inundated.npl.2030.sf <- read_sf(paste(output.dir, "/agol/inundated_npl/inundated_npl.shp", sep="")) %>%
#   mutate(type_r = "Superfund") %>%
#   filter(`2030-in-02` == 1 | `2030-in-12` == 1 | `2030-in-26` == 1 | `2030-il-02` == 1 | `2030-il-12` == 1 | `2030-il-26` == 1 | `2030-hi-02` == 1 | `2030-hi-12` == 1 | `2030-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2030-in-02`, `2030-in-12`, `2030-in-26`,`2030-il-02`, `2030-il-12`, `2030-il-26`,`2030-hi-02`, `2030-hi-12`, `2030-hi-26`)) %>%
#   rename(geom = geometry) %>%
#   mutate(address = str_to_title(address),
#          city    = str_to_title(city))
#
# inundated.npl.2050.sf <- read_sf(paste(output.dir, "/agol/inundated_npl/inundated_npl.shp", sep="")) %>%
#   mutate(type_r = "Superfund") %>%
#   filter(`2050-in-02` == 1 | `2050-in-12` == 1 | `2050-in-26` == 1 | `2050-il-02` == 1 | `2050-il-12` == 1 | `2050-il-26` == 1 | `2050-hi-02` == 1 | `2050-hi-12` == 1 | `2050-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2050-in-02`, `2050-in-12`, `2050-in-26`,`2050-il-02`, `2050-il-12`, `2050-il-26`, `2050-hi-02`, `2050-hi-12`, `2050-hi-26`)) %>%
#   rename(geom = geometry) %>%
#   mutate(address = str_to_title(address),
#          city    = str_to_title(city))
#
# inundated.npl.2100.sf <- read_sf(paste(output.dir, "/agol/inundated_npl/inundated_npl.shp", sep="")) %>%
#   mutate(type_r = "Superfund") %>%
#   filter(`2100-in-02` == 1 | `2100-in-12` == 1 | `2100-in-26` == 1 | `2100-il-02` == 1 | `2100-il-12` == 1 | `2100-il-26` == 1 | `2100-hi-02` == 1 | `2100-hi-12` == 1 | `2100-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2100-in-02`, `2100-in-12`, `2100-in-26`, `2100-il-02`, `2100-il-12`, `2100-il-26`,`2100-hi-02`, `2100-hi-12`, `2100-hi-26`)) %>%
#   rename(geom = geometry) %>%
#   mutate(address = str_to_title(address),
#          city    = str_to_title(city))

#3. get inundated rest of the CI, join to NPL
# inundated.ci.2020.sf <- all.inundated.ci.sf %>%
#   filter(`2020-in-02` == 1 | `2020-in-12` == 1 | `2020-in-26` == 1 | `2020-il-02` == 1 | `2020-il-12` == 1 | `2020-il-26` == 1 | `2020-hi-02` == 1 | `2020-hi-12` == 1 | `2020-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2020-in-02`, `2020-in-12`, `2020-in-26`, `2020-il-02`, `2020-il-12`, `2020-il-26`, `2020-hi-02`, `2020-hi-12`, `2020-hi-26`)) %>%
#   rbind(x = ., y = inundated.npl.2020.sf) #merge NPL
#
# inundated.ci.2030.sf <- all.inundated.ci.sf %>%
#   filter(`2030-in-02` == 1 | `2030-in-12` == 1 | `2030-in-26` == 1 | `2030-il-02` == 1 | `2030-il-12` == 1 | `2030-il-26` == 1 | `2030-hi-02` == 1 | `2030-hi-12` == 1 | `2030-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2030-in-02`, `2030-in-12`, `2030-in-26`, `2030-il-02`, `2030-il-12`, `2030-il-26`, `2030-hi-02`, `2030-hi-12`, `2030-hi-26`)) %>%
#   rbind(x = ., y = inundated.npl.2030.sf) #merge NPL
#
# inundated.ci.2050.sf <- all.inundated.ci.sf %>%
#   filter(`2050-in-02` == 1 | `2050-in-12` == 1 | `2050-in-26` == 1 | `2050-il-02` == 1 | `2050-il-12` == 1 | `2050-il-26` == 1 | `2050-hi-02` == 1 | `2050-hi-12` == 1 | `2050-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2050-in-02`, `2050-in-12`, `2050-in-26`, `2050-il-02`, `2050-il-12`, `2050-il-26`, `2050-hi-02`, `2050-hi-12`, `2050-hi-26`)) %>%
#   rbind(x = ., y = inundated.npl.2050.sf) #merge NPL
#
# inundated.ci.2100.sf <- all.inundated.ci.sf %>%
#   filter(`2100-in-02` == 1 | `2100-in-12` == 1 | `2100-in-26` == 1 | `2100-il-02` == 1 | `2100-il-12` == 1 | `2100-il-26` == 1 | `2100-hi-02` == 1 | `2100-hi-12` == 1 | `2100-hi-26` == 1) %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2100-in-02`, `2100-in-12`, `2100-in-26`, `2100-il-02`, `2100-il-12`, `2100-il-26`, `2100-hi-02`, `2100-hi-12`, `2100-hi-26`)) %>%
#   rbind(x = ., y = inundated.npl.2100.sf) #merge NPL
#
# inundated.ci.2020.df <- inundated.ci.2020.sf %>% st_drop_geometry()
# inundated.ci.2030.df <- inundated.ci.2030.sf %>% st_drop_geometry()
# inundated.ci.2050.df <- inundated.ci.2050.sf %>% st_drop_geometry()
# inundated.ci.2100.df <- inundated.ci.2100.sf %>% st_drop_geometry()
#
# #spreadsheet filenames
# spreadsheet.2020.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2020.xlsx",sep="")
# spreadsheet.2030.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2030.xlsx",sep="")
# spreadsheet.2050.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2050.xlsx",sep="")
# spreadsheet.2100.filename <- paste(output.dir, "/public_spreadsheets/inundated_infrastructure_2100.xlsx",sep="")
#
# write.xlsx(inundated.ci.2030.df, file = spreadsheet.2030.filename)
# write.xlsx(inundated.ci.2030.df, file = spreadsheet.2030.filename)
# write.xlsx(inundated.ci.2030.df, file = spreadsheet.2030.filename)
# write.xlsx(inundated.ci.2030.df, file = spreadsheet.2030.filename)
#
#
#

#
#
# inundated.ci.sf <- all.inundated.ci.sf %>%
#   rbind(x = ., y = inundated.npl.2050.sf) %>% #merge NPL
#   mutate(type_r = type) %>%
#   mutate(type_r = ifelse(type_r %in% public.affordable.housing, "Public or affordable housing",type_r)) %>%
#   mutate(type_r = ifelse(type_r %in% ed, "Educational institution",type_r))                             %>%
#   mutate(type_r = ifelse(type_r %in% public.safety, "Public safety or health facility",type_r))         %>%
#   mutate(type_r = ifelse(type_r %in% industrial, "Industrial contamination site",type_r))               %>%
#   mutate(type_r = ifelse(type_r %in% energy, "Energy infrastructure",type_r))                           %>%
#   mutate(type_r = ifelse(type_r %in% government, "Government facility",type_r))                         %>%
#   select(-id)
#
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2020-in-02"))
#
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2030-il-02"))
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2050-il-02"))
# fl <- inundated.ci.plus.npl.sf %>% filter(ysf=="2050-il-02" & state=="FL")
# fl
# View(table(fl$key))
# inundated.ci.plus.npl.sf %>% filter(ysf=="2050-il-02") %>% group_by(state) %>% summarise(n())
#
#
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2100-il-02"))
#
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2030-in-02"))
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2050-in-02"))
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2100-in-02"))
#
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2030-hi-02"))
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2050-hi-02"))
# nrow(inundated.ci.plus.npl.sf %>% filter(ysf=="2100-hi-02"))
#
# inundated.ci.plus.npl.sf



#
# inundated.npl.df     <- read_sf(paste(output.dir, "/agol/inundated_npl/inundated_npl.shp", sep="")) %>% st_drop_geometry() %>% mutate(type_r = "")
# all.inundated.ci.df  <- read_sf("//home/rama/coastal_deadline/data/output/agol/story_map/inundated_critical_infrastructure_edited_attributes.gpkg") %>%
#                         mutate(type_r = type) %>%
#                         mutate(type_r = ifelse(type_r %in% public.affordable.housing, "Public or affordable housing",type_r)) %>%
#                         mutate(type_r = ifelse(type_r %in% ed, "Educational institution",type_r))                             %>%
#                         mutate(type_r = ifelse(type_r %in% public.safety, "Public safety or health facility",type_r))         %>%
#                         mutate(type_r = ifelse(type_r %in% industrial, "Industrial contamination site",type_r))               %>%
#                         mutate(type_r = ifelse(type_r %in% energy, "Energy infrastructure",type_r))                           %>%
#                         mutate(type_r = ifelse(type_r %in% government, "Government facility",type_r)) %>%
#                         st_drop_geometry() %>%
#                         rbind(., inundated.npl.df) %>%
#                         mutate(key = paste(id, name, address, state, sep="|")) %>%
#                         mutate (ysf = paste(year, scenario, inundation.frequency, sep="-"))

# ci2.sf <- sp::merge(x = ci.sf[,"key"],
#                     y = all.inundated.ci.df,
#                     by = "key",
#                     all.x = FALSE)
#
# View(ci2.sf %>% st_drop_geometry() %>% filter(`2030-il-02`==1) %>% group_by(state) %>% summarise(n = n()))
# ci2.sf %>% st_drop_geometry() %>% filter(`2030-il-02`==1) %>% group_by(state) %>% summarise(n = n()) %>% pull(n) %>% sum()
# View(ci2.sf %>% filter(`2030-il-02`==1 & state =="LA"))
# View(ci2.sf %>% filter(`2030-il-02`==1 & state =="DE"))
#
#
# nrow(ci2.sf)
# View(ci2.sf %>% filter(id=="PH395"))
# ci.sf %>% filter(key=="Southbridge|519 S Locust St|DE")
#
# all.inundated.ci.df %>% filter(id=="PH395")
#
# View(ci.sf %>% filter(state =="DE"))
#

# replace_llc_with_uppercase <- function(text) {
#   str_replace_all(text, "llc", "LLC")
# }

#2050 intermediate, all frequencies for AGOL map -----
# #1. get inundated NPL
# inundated.npl.2050.in.sf <- read_sf(paste(output.dir, "/agol/inundated_npl/inundated_npl.shp", sep="")) %>%
#   filter(`2050-in-02` ==1 | `2050-in-12`==1 | `2050-in-26`==1) %>%
#   mutate(type_r = "Industrial contamination site") %>%
#   select(c(id, type, type_r, name, address, city,zipcode, state, `2050-in-02`, `2050-in-12`, `2050-in-26`)) %>%
#   rename(geom = geometry) %>%
#   mutate(address = str_to_title(address),
#          city    = str_to_title(city))
# #2. get inundated rest of the CI, join to NPL
# inundated.ci.2050.in.sf <- all.inundated.ci.sf %>% filter(`2050-in-02` ==1 | `2050-in-12`==1 | `2050-in-26`==1)           %>%
#                            select(c(id, type, type_r, name, address, city,zipcode, state, `2050-in-02`, `2050-in-12`, `2050-in-26`)) %>%
#                            rbind(x = ., y = inundated.npl.2050.in.sf) %>% #merge NPL
#                            select(c(id, type, type_r, name, address, city,zipcode, state, `2050-in-02`, `2050-in-12`, `2050-in-26`)) %>%
#                            write_sf(., dsn="/home/rama/coastal_deadline/data/output/agol/story_map", layer="inundated_critical_infrastructure_2050_intermediate", driver="ESRI Shapefile", delete_layer = T)
# #3. zip it up
# shp.zip <- "/home/rama/coastal_deadline/data/output/agol/story_map/inundated_critical_infrastructure_2050_intermediate.zip"
# file.remove(shp.zip)
# shps    <- list.files(path = "/home/rama/coastal_deadline/data/output/agol/story_map", pattern= "inundated_critical_infrastructure*", full.names = T)
# zip(shp.zip,shps)

