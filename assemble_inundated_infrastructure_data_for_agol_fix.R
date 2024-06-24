source("~/coastal_deadline/scripts/configuration.R")
ci.sf <- read_sf(paste(output.dir, "/agol/all_critical_infrastructure",sep="")) %>% select(id)

##inundated infra not including superfund ----
inundated.list <- list.files(paste(output.dir, "/inundated_named_infrastructure",sep=""), pattern=".gpkg$", full.names = TRUE)
all.inundated.sf <- st_sf(data.frame(), geometry = st_sfc(),crs = st_crs(4326)) #empty sf with correct crs
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
                    mutate (ysf = paste(year, scenario, inundation.frequency, sep="-")) %>%
                    select(c(id,lat,lon,ysf))

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
                                  mutate (ysf = paste(year, scenario, inundation.frequency, sep="-")) %>%
                                  select(EPA_ID, lat,lon, ysf) %>%
                                  distinct(EPA_ID, .keep_all = T) %>%
                                  st_drop_geometry() %>%
                                  rename(id = EPA_ID)

all.inundated.plus.npl.df <- rbind(all.inundated.df,all.inundated.npl.centroids.df)

#all.inundated.plus.npl.df <- rbind(all.inundated.df,all.inundated.npl.centroids.df) %>% write.csv(., paste(tmp.dir, "all_inundated_infra_fix.csv",sep="/"),row.names = FALSE)

#rbind(all.inundated.df,all.inundated.npl.centroids.df)  %>% filter(id=="{002C6CF5-D1E2-4158-97E3-9713635B608F}")
#rbind(all.inundated.df,all.inundated.npl.centroids.df)  %>% filter(id=="110038742331")


