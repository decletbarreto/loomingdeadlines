source("~/coastal_deadline/scripts/configuration.R")
library(tictoc)
library(foreach)
library(doParallel)
library(logr)
cores <- 12
#exclude.vars <- c("cs.coastal.conus.sf","cs.coastal.conus.fips","cs.coastal.east.sf","cs.coastal.west.sf")


# WEST --------------------------------------------------------------------
scenario.counter <- 1
#unlink(east.inundation.layer.summary.msgfile)
#east.scenario <- east.inundation.scenarios.list[1]
for(west.scenario in west.inundation.scenarios.list) #PRIORITY
{
  # Initialize lists to store results
  west.inundation.summary             <- list()
  west.inundated.named.infrastructure <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("WEST"))) #empty sf with correct crs
  west.npl.inundated                  <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("WEST"))) #empty sf with correct crs

  #cs.counter <- 1
  west.inundation.layer.summary.logfile <- file.path(paste(log.dir, "_west_inundation_layer_summary.log",sep=""))
  unlink(west.inundation.layer.summary.logfile)
  # Loop in parallel through county subdivisions
  # Register parallel backend
  cl <- makeCluster(cores, outfile = west.inundation.layer.summary.logfile)
  registerDoParallel(cl)
  results <- foreach(cs.fips = cs.west.fips,
                     .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
                     .verbose  = TRUE
                     # .export   = configuration.variables,
                     # .noexport = exclude.vars
  ) %dopar%
    {
      r <- calculate.frequency.parallel(cs.fips, west.scenario)
      print(paste("Scenario ", scenario.counter, "/", length(west.inundation.scenarios.list),"|",west.scenario  ,"|", cs.fips,"|",toc(quiet = TRUE)[4], sep=""))
      return(r)
    }

  # Combine the results
  for (res in results) {
    west.inundation.summary             <- rbind(west.inundation.summary,             res$inundation.summary)
    west.inundated.named.infrastructure <- rbind(west.inundated.named.infrastructure, res$inundated.named.infrastructure)
    west.npl.inundated                  <- rbind(west.npl.inundated,                  res$npl.inundated)
  }

  #write inundated summary to GPKG
  cs.west.summary.layer.name <- paste(inundation.summaries.dir, "/", west.scenario, "_summary.gpkg", sep="")
  print(paste("writing inundated summary to GPKG: ", cs.west.summary.layer.name, sep=""))
  cs.west.summary.sf <- sp::merge(x = cs.west.sf, y = west.inundation.summary, by.x = "GEOID", by.y = "cs.geoid") %>%
    write_sf(obj = ., dsn=cs.west.summary.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated named infrastructure to GPKG
  cs.west.inundated.named.infrastructure.layer.name <- paste(inundated.named.infrastructure.dir, "/", west.scenario, "_inundated_named_infrastructure.gpkg", sep="")
  print(paste("writing inundated named infrastructure to GKPG: ", cs.west.inundated.named.infrastructure.layer.name, sep=""))
  write_sf(obj = west.inundated.named.infrastructure, dsn=cs.west.inundated.named.infrastructure.layer.name, driver="GPKG", delete_layer = TRUE)

  #write inundated NPL sites to GPKG
  cs.west.npl.inundated.layer.name <- paste(npl.inundated.dir, "/", west.scenario, "_npl_inundated.gpkg", sep="")
  print(paste("writing inundated NPL sites to GPKG: ", cs.west.npl.inundated.layer.name, sep=""))
  write_sf(obj = west.npl.inundated, dsn = cs.west.npl.inundated.layer.name, driver = "GPKG", delete_layer = TRUE)

  # Close the parallel backend
  stopCluster(cl)

  #increase the scenario counter
  scenario.counter <- scenario.counter + 1
}

# EAST --------------------------------------------------------------------
scenario.counter <- 1
#unlink(east.inundation.layer.summary.msgfile)
#east.scenario <- east.inundation.scenarios.list[1]
#second.east.list <- east.inundation.scenarios.list[east.inundation.scenarios.list %notin% east.inundation.layers.priority.list]
for(east.scenario in east.inundation.scenarios.list) #PRIORITY
{
  # Initialize lists to store results
  east.inundation.summary             <- list()
  east.inundated.named.infrastructure <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("EAST"))) #empty sf with correct crs
  east.npl.inundated                  <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("EAST"))) #empty sf with correct crs

  #cs.counter <- 1
  east.inundation.layer.summary.logfile <- file.path(paste(log.dir, "/", east.scenario, "_east_inundation_layer_summary.log",sep=""))
  unlink(east.inundation.layer.summary.logfile)
  # Loop in parallel through county subdivisions
  # Register parallel backend
  cl <- makeCluster(cores, outfile = east.inundation.layer.summary.logfile)
  registerDoParallel(cl)
  results <- foreach(cs.fips = cs.east.fips,
                     .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
                     .verbose  = TRUE
                     # .export   = configuration.variables,
                     # .noexport = exclude.vars
                     ) %dopar%
  {
    r <- calculate.frequency.parallel(cs.fips, east.scenario)
    #print(paste("Scenario ", scenario.counter, "/", length(east.inundation.scenarios.list),"|",east.scenario  ,"|", cs.fips,"|",elapsed_time$callback_msg, sep=""))
    print(paste("Scenario ", scenario.counter, "/", length(east.inundation.scenarios.list),"|",east.scenario  ,"|", cs.fips,"|",toc(quiet = TRUE)[4], sep=""))
    return(r)
  }

  # Combine the results
  for (res in results) {
    east.inundation.summary             <- rbind(east.inundation.summary,             res$inundation.summary)
    east.inundated.named.infrastructure <- rbind(east.inundated.named.infrastructure, res$inundated.named.infrastructure)
    east.npl.inundated                  <- rbind(east.npl.inundated,                  res$npl.inundated)
  }

  #write provenance inundated summary to GPKG
  cs.east.summary.layer.name <- paste(inundation.summaries.dir, "/", east.scenario, "_summary.gpkg", sep="")
  print(paste("writing provenance inundated summary to GPKG: ", cs.east.summary.layer.name, sep=""))
  cs.east.summary.sf <- sp::merge(x = cs.east.sf, y = east.inundation.summary, by.x = "GEOID", by.y = "cs.geoid") %>%
    write_sf(obj = ., dsn=cs.east.summary.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated named infrastructure to GPKG
  cs.east.inundated.named.infrastructure.layer.name <- paste(inundated.named.infrastructure.dir, "/", east.scenario, "_inundated_named_infrastructure.gpkg", sep="")
  print(paste("writing inundated named infrastructure to GPKG: ", cs.east.inundated.named.infrastructure.layer.name, sep=""))
  write_sf(obj = east.inundated.named.infrastructure, dsn=cs.east.inundated.named.infrastructure.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated NPL sites to GPKG
  cs.east.npl.inundated.layer.name <- paste(npl.inundated.dir, "/", east.scenario, "_npl_inundated.gpkg", sep="")
  print(paste("writing inundated NPL sites to GPKG: ", cs.east.npl.inundated.layer.name, sep=""))
  write_sf(obj = east.npl.inundated, dsn=cs.east.npl.inundated.layer.name, driver = "GPKG", delete_layer = TRUE)

  # Close the parallel backend
  stopCluster(cl)

  #increase the scenario counter
  scenario.counter <- scenario.counter + 1
}

