#Loiza inundated infrastructure analysis
source("~/coastal_deadline/scripts/configuration.R")
library(doParallel)
library(parallel)
library(foreach)
# .county.subdivision.geoid =  "7208752775"
# .inundation.scenario      = "is_flooded_sea-2030-high-02-PRVI"
# .infrastructure.sf        = read_sf("/home/rama/coastal_deadline/data/Loiza/other_infrastructure_loiza/other_infrastructure_loiza.shp")

calculate.frequency.of.inundation.custom.ci <- function(.county.subdivision.geoid = NA, .inundation.scenario = NA, .infrastructure.sf = NA, .provenance = "CUS")
{
  npl.inundated.summary <- NULL  #not implemented yer for custom CI
  inundation.summary    <- NULL  #not implemented yer for custom CI
  stopifnot(nchar(.county.subdivision.geoid) == 10) #CS geoid has 10 characters

  provenance.not.present.value <- 0
  round.factor <- 2

  state.geoid <- substr(.county.subdivision.geoid, 1,2)
  state.abb   <- states.df %>% filter(state.fips == state.geoid) %>% select(state.abbreviation) %>% pull() %>% as.character()

  inundation.layer.name <- paste(cropped.output.dir, "/",.inundation.scenario, "/",  .inundation.scenario, "_", .county.subdivision.geoid, "_crop.gpkg",sep="")
  infrastructure.layer.name <- paste(infrastructure.output.dir, "/", state.abb, "_critical_infrastructure.gpkg",sep="")

  # process inundation layer ------
  if (file.exists(inundation.layer.name))
  {
    inundation.layer.sf <- read_sf(dsn=inundation.layer.name) %>%
                           st_transform(., crs=assign.crs(state.abb))

    .infrastructure.sf <- .infrastructure.sf %>%
                          st_transform(., crs=assign.crs(state.abb))
    intersection.sf <- st_join(x    = .infrastructure.sf,
                               y    = inundation.layer.sf,
                               join = st_within,
                               left = FALSE) %>%
                      mutate(id         = as.character(fid),
                             name       = as.character(name),
                             address    = as.character(address),
                             city       = as.character(city),
                             state      = as.character(state),
                             zipcode    = as.character(zipcode),
                             provenance = "LOI") %>%
      select(all_of(c("id", "name","address", "city","state","zipcode","provenance")))

      inundated.named.infrastructure <- intersection.sf
#
#     infrastructure.type.inundation.summary <- intersection.sf      %>%
#                                               st_drop_geometry()     %>%
#                                               group_by(infrastructure_type)   %>%
#                                               summarise(total = n()) %>%
#                                               pivot_wider(names_from = infrastructure_type, values_from = total) %>%
#                                               as.data.frame()
  }else
  {
    print(paste("Inundation layer ", inundation.layer.name, " does not exist.", sep=""))
    inundated.named.infrastructure <- NULL
    npl.inundated.summary          <- NULL
  }

  return(list(inundation.summary            = inundation.summary,
             inundated.named.infrastructure = inundated.named.infrastructure,
             npl.inundated.summary          = npl.inundated.summary))
}

#Calculate frequency of inundation ======
cores <- 12
loiza.cs.fips <- cs.pr.sf %>% st_drop_geometry() %>% filter(county.fips == "72087") %>% select(GEOID) %>% pull()
#coastal.inundation.layers <- list.files(path = paste(output.dir, "/inundation_layers_crop",sep= ""), pattern = "*.gpkg$", recursive = TRUE, full.names = TRUE)
# loiza.coastal.inundation.layers <- coastal.inundation.layers[grep("72087", coastal.inundation.layers)]
# inundation.layer <- loiza.coastal.inundation.layers[1]
scenario.counter <- 1
pr.vi.scenario <- pr.vi.inundation.scenarios.list[28]
loiza.ci.sf = read_sf("/home/rama/coastal_deadline/data/Loiza/other_infrastructure_loiza/other_infrastructure_loiza.shp")

for(pr.vi.scenario in pr.vi.inundation.scenarios.list[28])
{
  # Initialize objects to store results
  loiza.inundation.summary             <- list()
  loiza.inundated.named.infrastructure <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("PR"))) #empty sf with correct crs
  loiza.npl.inundated                  <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("PR"))) #empty sf with correct crs

  loiza.inundation.layer.summary.logfile <- file.path(paste(log.dir, "/", pr.vi.scenario, "_loiza_inundation_layer_summary.log",sep=""))
  unlink(loiza.inundation.layer.summary.logfile)
  # Loop in parallel through county subdivisions
  # Register parallel backend
  cl <- makeCluster(cores, outfile = loiza.inundation.layer.summary.logfile)
  registerDoParallel(cl)
  results <- foreach(cs.fips = loiza.cs.fips,
                     .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
                     .verbose  = TRUE
  ) %do%
    {
      tic()
      r <- calculate.frequency.of.inundation.custom.ci(.county.subdivision.geoid =  cs.fips,
                                                       .inundation.scenario      = pr.vi.scenario,
                                                       .infrastructure.sf = loiza.ci.sf,
                                                       .provenance = "LOI")
      print(paste("Scenario ", scenario.counter, "/", length(pr.vi.inundation.scenarios.list),"|",pr.vi.scenario  ,"|", cs.fips,"|",toc(quiet = TRUE)[4], sep=""))
      return(r)
    }

  # Combine the results
  for (res in results) {
    loiza.inundation.summary             <- rbind(loiza.inundation.summary,             res$inundation.summary)
    loiza.inundated.named.infrastructure <- rbind(loiza.inundated.named.infrastructure, res$inundated.named.infrastructure)
    loiza.npl.inundated                  <- rbind(loiza.npl.inundated,                  res$npl.inundated)
  }

  #write inundated summary to GPKG
  # cs.loiza.summary.layer.name <- paste(output.dir, "/loiza/", loiza.scenario, "_summary.gpkg", sep="")
  # print(paste("writing provenance inundated summary to GPKG: ", cs.loiza.summary.layer.name, sep=""))
  # cs.loiza.summary.sf <- sp::merge(x = cs.loiza.for.join.sf, y = loiza.inundation.summary, by.x = "GEOID", by.y = "cs.geoid") %>%
  #   write_sf(obj = ., dsn=cs.loiza.summary.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated named infrastructure to GPKG
  cs.loiza.inundated.named.infrastructure.layer.name <- paste(output.dir, "/loiza/", pr.vi.scenario, "_inundated_named_infrastructure.gpkg", sep="")
  print(paste("writing inundated named infrastructure to GPKG: ", cs.loiza.inundated.named.infrastructure.layer.name, sep=""))
  write_sf(obj = loiza.inundated.named.infrastructure, dsn = cs.loiza.inundated.named.infrastructure.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated NPL sites to GPKG
  # cs.loiza.npl.inundated.layer.name <- paste(output.dir, "/loiza/", loiza.scenario, "_npl_inundated.gpkg", sep="")
  # print(paste("writing inundated NPL sites to GPKG: ", cs.loiza.npl.inundated.layer.name, sep=""))
  # write_sf(obj = loiza.npl.inundated, dsn = cs.loiza.npl.inundated.layer.name, driver = "GPKG", delete_layer = TRUE)

  # Close the parallel backend
  stopCluster(cl)

  print(paste("FINISHED ", pr.vi.scenario, sep=""))
  #increase the scenario counter
  scenario.counter <- scenario.counter + 1
}

#Assemble AGOL inundated CI data
ci.df <-  read_sf("/home/rama/coastal_deadline/data/Loiza/other_infrastructure_loiza/other_infrastructure_loiza.shp") %>%
          st_drop_geometry() %>%
          select(all_of(c("fid", "type"))) %>%
          rename(id = fid) %>%
          mutate(id = as.character(id))


inundated.list <- list.files(paste(output.dir, "/loiza",sep=""), pattern=".gpkg$", full.names = TRUE)
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
    mutate(year     = year.string,
           scenario = scenario.string,
           inundation.frequency = frequency.string)

  all.inundated.sf <- rbind(all.inundated.sf, inundated.sf)
}
all.inundated.df <- all.inundated.sf %>%
  mutate(lat = st_coordinates(.)[, "Y"],
         lon = st_coordinates(.)[, "X"]) %>%
  st_drop_geometry()

all.inundated.type.wide <- all.inundated.df %>%
  mutate(scenario = recode(scenario,
                           "high"    = "hi",
                           "int_low" = "il",
                           "int"     = "in")) %>%
  mutate (ysf = paste(year, scenario, inundation.frequency, sep="-"),
          row.number = row_number()) %>%
  st_drop_geometry()                 %>%
  pivot_wider(., id_cols = id, names_from = ysf, values_from = row.number, values_fn = list(row.number = length)) %>%
  mutate_if(is.numeric, ~ifelse(. > 0, 1, .))    %>% #recode frequency of combination to a 1
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
  select(id,name, address, city, zipcode, state, type, everything()) %>%
  select(-c(provenance,year, scenario, inundation.frequency))  %>%
  write_sf(., dsn = paste(output.dir, "/loiza/loiza_inundated_critical_infrastructure", sep=""), driver="ESRI Shapefile", delete_layer = TRUE)

shp.zip <- paste(output.dir, "/loiza/loiza_inundated_critical_infrastructure.zip",sep="")
shps <- list.files(path = paste(output.dir, "/loiza/loiza_inundated_critical_infrastructure",sep=""), pattern= "inundated_critical_infrastructure*", full.names = T)
zip(shp.zip,shps)




#Assemble all Loiza CI data ------
ci.list <- list.files(paste(output.dir, "/loiza/",sep=""), pattern=".gpkg$", full.names = TRUE)
all.ci.sf <- read_sf(ci.list[1]) %>% st_transform(., 4326)
for(ci in ci.list[2: length(ci.list)])
  #for(ci in ci.list[2:4])
{
  print(ci)
  ci.sf     <- read_sf(ci) %>% st_transform(., 4326)
  all.ci.sf <- rbind(all.ci.sf, ci.sf)
}
shp <- paste(output.dir, "/agol/all_critical_infrastructure",sep="")
agol.cols <- c("id", "name", "address", "city", "state", "zipcode", "type", "cs_name", "county","levee_flg")
all.ci.sf.final <-  all.ci.sf %>%
  mutate(type      = infrastructure_type,
         cs_name   = county.sub.name,
         county    = countyname,
         levee_flg = inside_leveed_area) %>%
  select(all_of(agol.cols))              %>%
  write_sf(., dsn = shp, driver="ESRI Shapefile", delete_layer = T)
shp.zip <- paste(output.dir, "/agol/all_critical_infrastructure.zip",sep="")
shps <- list.files(path = paste(output.dir, "/agol/all_critical_infrastructure",sep=""), pattern= "all_critical_infrastructure.*", full.names = T)
zip(shp.zip,shps)



