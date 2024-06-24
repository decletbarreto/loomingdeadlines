#comunidades especiales inundated infrastructure analysis
source("~/coastal_deadline/scripts/configuration.R")

ce.sf <- read_sf("/media/rama/datastore3/Census/borinquen/comunidades_especiales/comunidades_especiales.gpkg") %>%
         select(nombre, municipio, barrio) %>% st_transform(., crs = assign.crs("PR"))

# .county.subdivision.geoid =  "7208752775"
# .inundation.scenario      = "is_flooded_sea-2030-high-02-PRVI"
# .polygons.sf  = ce.sf
cores <- 12
get.percent.inundated.polygons <- function(.county.subdivision.geoid, .inundation.scenario, .polygons.sf)
{
  stopifnot(nchar(.county.subdivision.geoid) == 10) #CS geoid has 10 characters

  provenance.not.present.value <- 0
  round.factor <- 2

  state.geoid <- substr(.county.subdivision.geoid, 1,2)
  state.abbreviation   <- states.df %>% filter(state.fips == state.geoid) %>% select(state.abbreviation) %>% pull() %>% as.character()

  inundation.layer.name <- paste(cropped.output.dir, "/",.inundation.scenario, "/",  .inundation.scenario, "_", .county.subdivision.geoid, "_crop.gpkg",sep="")
  #infrastructure.layer.name <- paste(infrastructure.output.dir, "/", state.abb, "_critical_infrastructure.gpkg",sep="")

  if (file.exists(inundation.layer.name))
  {
    inundation.layer.sf <- read_sf(dsn=inundation.layer.name) %>%
      st_transform(., crs=assign.crs(state.abbreviation))

    .polygons.sf <- .polygons.sf %>%
                    st_transform(., crs = assign.crs(state.abbreviation)) %>% #project to right crs
                    mutate(polygon.area = as.numeric(st_area(.)))

    intersection.sf <- st_intersection(x = .polygons.sf,
                                       y = inundation.layer.sf)    %>%
                      mutate(sliver.area = as.numeric(st_area(.))) %>%
                      mutate(per.inundated = round((sliver.area/polygon.area) * 100, 10))

    inundated.comunidades.especiales <- st_drop_geometry(intersection.sf) %>%
                                        select(-c("municipio","barrio")) %>%
                                        merge(x = ce.sf,
                                              y = .,
                                              by = "nombre")
  }else
  {
    print(paste("Inundation layer ", inundation.layer.name, " does not exist. Returning summary dataframe with totals only.", sep=""))
    inundated.comunidades.especiales <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs(state.abbreviation))) #empty sf with correct crs
  }
  return(inundated.comunidades.especiales)
}

# get.percent.inundated.polygons(.county.subdivision.geoid =  "7208752775",
#                                .inundation.scenario      = "is_flooded_sea-2030-high-02-PRVI",
#                                .polygons.sf  = ce.sf)

#inundation summaries ------
#pr.vi.scenario <- pr.vi.inundation.scenarios.list[1]
for(pr.vi.scenario in pr.vi.inundation.scenarios.list)
{
  # Initialize objects to store results
  loiza.inundation.summary <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("PR"))) #empty sf with correct crs

  # Loop in parallel through county subdivisions
  # Register parallel backend
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  results <- foreach(cs.fips = cs.pr.fips,
                     .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
                     .verbose  = TRUE
  ) %dopar%
    {
      r <-get.percent.inundated.polygons(.county.subdivision.geoid = cs.fips, .inundation.scenario = pr.vi.scenario, .polygons.sf = ce.sf)
      return(r)
    }

  # Combine the results
  for (res in results) {
    loiza.inundation.summary <- rbind(loiza.inundation.summary, res)
  }

  #write inundated comunidades_especiales to GPKG
  cs.loiza.inundated.named.infrastructure.layer.name <- paste(output.dir, "/loiza/comunidades_especiales/", pr.vi.scenario, "_inundated_comunidades_especiales.gpkg", sep="")
  print(paste("writing inundated named infrastructure to GPKG: ", cs.loiza.inundated.named.infrastructure.layer.name, sep=""))
  if(!(is.null(loiza.inundation.summary)))
  {
  write_sf(obj = loiza.inundation.summary, dsn = cs.loiza.inundated.named.infrastructure.layer.name, driver = "GPKG", delete_layer = TRUE)
  }

  # Close the parallel backend
  stopCluster(cl)

  print(paste("FINISHED ", pr.vi.scenario, sep=""))
  #increase the scenario counter
  #scenario.counter <- scenario.counter + 1
}

#assemble inundated comunidades especiales summaries -----
inundated.list <- list.files(paste(output.dir, "/loiza/comunidades_especiales",sep=""), pattern=".gpkg$", full.names = TRUE)
all.inundated.sf <- st_sf(data.frame(), geometry = st_sfc(),crs = st_crs(4326)) #empty sf with correct crs
#inundated <- inundated.list[1]
for(inundated in inundated.list)
{
  inundated.sf     <- read_sf(inundated) %>% st_transform(., 4326)

  if(nrow(inundated.sf) ==0)
  {
    print("empty sf")
    next
  }

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
                         inundation.frequency = frequency.string) %>%
                         select(nombre, municipio, barrio, polygon.area, sliver.area, per.inundated, year, scenario, inundation.frequency, geom)

  all.inundated.sf <- rbind(all.inundated.sf, inundated.sf)
}
all.inundated.df <- all.inundated.sf %>%st_drop_geometry()

all.inundated.type.wide <- all.inundated.df %>%
  mutate(scenario = recode(scenario,
                           "high"    = "hi",
                           "int_low" = "il",
                           "int"     = "in")) %>%
  mutate (ysf = paste(year, scenario, inundation.frequency, sep="-"),
          row.number = row_number()) %>%
  st_drop_geometry()                 %>%
  pivot_wider(., id_cols = nombre, names_from = ysf, values_from = per.inundated, values_fn = list(per.inundated = mean)) %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .)) %>% #recode NAs to zero
  inner_join(x  = .,  #join name, address, city, state, zip, lat, lon
             y  = all.inundated.df,
             by = "nombre",
             multiple = "first") %>%
             select(-c(municipio, barrio, polygon.area, sliver.area, per.inundated, year, scenario, inundation.frequency))

all.inundated.distinct.sf <- all.inundated.sf %>%
                             distinct(nombre, .keep_all = TRUE) %>%
                             merge(x = .,
                                   y = all.inundated.type.wide,
                                   by = "nombre") %>%
  select(-c(year, scenario, inundation.frequency,polygon.area, sliver.area))  %>%
  rename(per_i = per.inundated) %>%
  write_sf(., dsn = paste(output.dir, "/PR/borinquen_inundated_comunidades_especiales", sep=""), driver="ESRI Shapefile", delete_layer = TRUE)

shp.zip <- paste(output.dir, "/PR/borinquen_inundated_comunidades_especiales",sep="")
shps <- list.files(path = paste(output.dir, "/PR/borinquen_inundated_comunidades_especiales",sep=""), pattern= "borinquen_inundated_comunidades_especiales*", full.names = T)
zip(shp.zip,shps)

#https://cpprbib.wordpress.com/biblioteca-virtual/guias-tematicas/comunidades-especiales/comunidades-especiales-de-puerto-rico/loiza/
