source("~/coastal_deadline/scripts/configuration.R")
library(tictoc)
library(foreach)
library(doParallel)
library(logr)
cores <- 12
exclude.vars <- c("cs.coastal.conus.sf","cs.coastal.conus.fips","cs.coastal.east.sf","cs.coastal.west.sf")
#pr.vi.scenario <-pr.vi.inundation.scenarios.list[1]

cs.gu.for.join.sf    <- cs.gu.sf    %>% select(GEOID)
cs.pr.vi.for.join.sf <- cs.pr.vi.sf %>% select(GEOID)

# PR-VI -------------------------------------------------------------------
scenario.counter <- 1
#debug
#pr.vi.scenario <- "is_flooded_sea-2020-high-02-PRVI"
#cs.fips <- "7211377242"
for(pr.vi.scenario in pr.vi.inundation.scenarios.list[1:9])
{
  # Initialize objects to store results
  pr.vi.inundation.summary             <- list()
  pr.vi.inundated.named.infrastructure <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("PR"))) #empty sf with correct crs
  pr.vi.npl.inundated                  <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("PR"))) #empty sf with correct crs

  pr.vi.inundation.layer.summary.logfile <- file.path(paste(log.dir, "/", pr.vi.scenario, "_pr_vi_inundation_layer_summary.log",sep=""))
  unlink(pr.vi.inundation.layer.summary.logfile)
  # Loop in parallel through county subdivisions
  # Register parallel backend
  cl <- makeCluster(cores, outfile = pr.vi.inundation.layer.summary.logfile)
  registerDoParallel(cl)
  results <- foreach(cs.fips = cs.pr.vi.fips,
                     .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
                     .verbose  = TRUE
                     #.export   = configuration.variables
                     #.noexport = exclude.vars
                     ) %do%
  {
    tic()
    r <- calculate.frequency.parallel(cs.fips, pr.vi.scenario)
    print(paste("Scenario ", scenario.counter, "/", length(pr.vi.inundation.scenarios.list),"|",pr.vi.scenario  ,"|", cs.fips,"|",toc(quiet = TRUE)[4], sep=""))
    return(r)
  }

  # Combine the results
  for (res in results) {
    pr.vi.inundation.summary             <- rbind(pr.vi.inundation.summary,             res$inundation.summary)
    pr.vi.inundated.named.infrastructure <- rbind(pr.vi.inundated.named.infrastructure, res$inundated.named.infrastructure)
    pr.vi.npl.inundated                  <- rbind(pr.vi.npl.inundated,                  res$npl.inundated)
  }

  #write inundated summary to GPKG
  cs.pr.vi.summary.layer.name <- paste(inundation.summaries.dir, "/", pr.vi.scenario, "_summary.gpkg", sep="")
  print(paste("writing provenance inundated summary to GPKG: ", cs.pr.vi.summary.layer.name, sep=""))
  cs.pr.vi.summary.sf <- sp::merge(x = cs.pr.vi.for.join.sf, y = pr.vi.inundation.summary, by.x = "GEOID", by.y = "cs.geoid") %>%
    write_sf(obj = ., dsn=cs.pr.vi.summary.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated named infrastructure to GPKG
  cs.pr.vi.inundated.named.infrastructure.layer.name <- paste(inundated.named.infrastructure.dir, "/", pr.vi.scenario, "_inundated_named_infrastructure.gpkg", sep="")
  print(paste("writing inundated named infrastructure to GPKG: ", cs.pr.vi.inundated.named.infrastructure.layer.name, sep=""))
  write_sf(obj = pr.vi.inundated.named.infrastructure, dsn = cs.pr.vi.inundated.named.infrastructure.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated NPL sites to GPKG
  cs.pr.vi.npl.inundated.layer.name <- paste(npl.inundated.dir, "/", pr.vi.scenario, "_npl_inundated.gpkg", sep="")
  print(paste("writing inundated NPL sites to GPKG: ", cs.pr.vi.npl.inundated.layer.name, sep=""))
  write_sf(obj = pr.vi.npl.inundated, dsn = cs.pr.vi.npl.inundated.layer.name, driver = "GPKG", delete_layer = TRUE)

  # Close the parallel backend
  stopCluster(cl)

  print(paste("FINISHED ", pr.vi.scenario, sep=""))
  #increase the scenario counter
  scenario.counter <- scenario.counter + 1
}

# GU -------------------------------------------------------------------
scenario.counter <- 1
#gu.scenario <- gu.inundation.scenarios.list[1]
for(gu.scenario in gu.inundation.scenarios.list)
{
  # Initialize objects to store results
  gu.inundation.summary             <- list()
  gu.inundated.named.infrastructure <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("GU"))) #empty sf with correct crs
  gu.npl.inundated                  <- st_sf(data.frame(), geometry = st_sfc(),crs = (assign.crs("GU"))) #empty sf with correct crs

  gu.inundation.layer.summary.logfile <- file.path(paste(log.dir, "/", gu.scenario, "_gu_inundation_layer_summary.log",sep=""))
  unlink(gu.inundation.layer.summary.logfile)
  # Loop in parallel through county subdivisions
  # Register parallel backend
  cl <- makeCluster(cores, outfile = gu.inundation.layer.summary.logfile)
  registerDoParallel(cl)
  #cs.fips <- cs.gu.fips[1]
  results <- foreach(cs.fips   = cs.gu.fips,
                     .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
                     .verbose  = TRUE
                     #.export   = configuration.variables
                     #.noexport = exclude.vars
  ) %dopar%
    {
      tic()
      r <- calculate.frequency.parallel(cs.fips, gu.scenario)
      print(paste("Scenario ", scenario.counter, "/", length(gu.inundation.scenarios.list),"|",gu.scenario  ,"|", cs.fips,"|",toc(quiet = TRUE)[4], sep=""))
      return(r)
    }

  # Combine the results
  for (res in results) {
    gu.inundation.summary             <- rbind(gu.inundation.summary,             res$inundation.summary)
    gu.inundated.named.infrastructure <- rbind(gu.inundated.named.infrastructure, res$inundated.named.infrastructure)
    gu.npl.inundated                  <- rbind(gu.npl.inundated,                  res$npl.inundated)
  }

  #write provenance inundated summary to GPKG
  cs.gu.summary.layer.name <- paste(inundation.summaries.dir, "/", gu.scenario, "_summary.gpkg", sep="")
  print(paste("writing inundated summary to GPKG: ", cs.gu.summary.layer.name, sep=""))
  cs.gu.summary.sf <- sp::merge(x = cs.gu.for.join.sf, y = gu.inundation.summary, by.x = "GEOID", by.y = "cs.geoid") %>%
    write_sf(obj = ., dsn=cs.gu.summary.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated named infrastructure to GPKG
  cs.gu.inundated.named.infrastructure.layer.name <- paste(inundated.named.infrastructure.dir, "/", gu.scenario, "_inundated_named_infrastructure.gpkg", sep="")
  print(paste("writing inundated named infrastructure to GPKG: ", cs.gu.inundated.named.infrastructure.layer.name, sep=""))
  write_sf(obj = gu.inundated.named.infrastructure, dsn = cs.gu.inundated.named.infrastructure.layer.name, driver = "GPKG", delete_layer = TRUE)

  #write inundated NPL sites to GPKG
  cs.gu.npl.inundated.layer.name <- paste(npl.inundated.dir, "/", gu.scenario, "_npl_inundated.gpkg", sep="")
  print(paste("writing inundated NPL sites to GPKG: ", cs.gu.npl.inundated.layer.name, sep=""))
  write_sf(obj = gu.npl.inundated, dsn = cs.gu.npl.inundated.layer.name, driver = "GPKG", delete_layer = TRUE)
  # Close the parallel backend
  stopCluster(cl)

  print(paste("FINISHED ", gu.scenario, sep=""))
  #increase the scenario counter
  scenario.counter <- scenario.counter + 1
}


# GU old -------------------------------------------------------------------
# scenario.counter <- 1
# for(gu.scenario in gu.inundation.scenarios.list)
# {
#   # Initialize lists to store results
#   gu.inundation.summary             <- list()
#   gu.inundated.named.infrastructure <- list()
#   gu.npl.inundated                  <- list()
#
#   gu.inundation.layer.summary.logfile <- file.path(paste(log.dir, "/", gu.scenario, "_gu_inundation_layer_summary.log",sep=""))
#   unlink(gu.inundation.layer.summary.logfile)
#   # Loop in parallel through county subdivisions
#   # Register parallel backend
#   cl <- makeCluster(cores, outfile = gu.inundation.layer.summary.logfile)
#   registerDoParallel(cl)
#   results <- foreach(cs.fips   = cs.gu.fips,
#                      .packages = c("dplyr","sf", "logr","tidyr", "tictoc"),
#                      .verbose  = TRUE
#                      #.export   = configuration.variables
#                      #.noexport = exclude.vars
#   ) %dopar%
#     {
#       tic()
#       r <- calculate.frequency.parallel(cs.fips, gu.scenario)
#       print(paste("Scenario ", scenario.counter, "/", length(gu.inundation.scenarios.list),"|",gu.scenario  ,"|", cs.fips,"|",toc(quiet = TRUE)[4], sep=""))
#       return(r)
#     }
#
#   # Combine the results
#   for (res in results) {
#     gu.inundation.summary             <- rbind(gu.inundation.summary,             res$provenance.summary)
#     gu.inundated.named.infrastructure <- rbind(gu.inundated.named.infrastructure, res$inundated.named.infrastructure)
#     gu.npl.inundated                  <- rbind(gu.npl.inundated,                  res$npl.inundated)
#   }
#
#   #write provenance inundated summary to GPKG
#   cs.gu.summary.layer.name <- paste(inundation.summaries.dir, "/", gu.scenario, "_summary.gpkg", sep="")
#   print(paste("writing provenance inundated summary to GPKG: ", cs.gu.summary.layer.name, sep=""))
#   cs.gu.summary.sf <- sp::merge(x = cs.gu.sf, y = gu.inundation.summary, by.x = "GEOID", by.y = "cs.geoid") %>%
#     select(-abb) %>%
#     write_sf(obj = ., dsn=cs.gu.summary.layer.name, driver = "GPKG", delete_layer = TRUE)
#
#   #write inundated named infrastructure to CSV
#   cs.gu.inundated.named.infrastructure.layer.name <- paste(inundated.named.infrastructure.dir, "/", gu.scenario, "_inundated_named_infrastructure.csv", sep="")
#   print(paste("writing inundated named infrastructure to CSV: ", cs.gu.inundated.named.infrastructure.layer.name, sep=""))
#   write.csv(x = gu.inundated.named.infrastructure, file=cs.gu.inundated.named.infrastructure.layer.name, row.names = FALSE)
#
#   #write inundated NPL sites to CSV
#   cs.gu.npl.inundated.layer.name <- paste(npl.inundated.dir, "/", gu.scenario, "_npl_inundated.csv", sep="")
#   print(paste("writing inundated NPL sites to CS: ", cs.gu.npl.inundated.layer.name, sep=""))
#   write.csv(x = gu.npl.inundated, file=cs.gu.npl.inundated.layer.name, row.names = FALSE)
#
#   # Close the parallel backend
#   stopCluster(cl)
#
#   print(paste("FINISHED ", gu.scenario, sep=""))
#   #increase the scenario counter
#   scenario.counter <- scenario.counter + 1
# }

