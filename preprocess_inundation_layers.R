source("~/coastal_deadline/scripts/configuration.R")
source("~/coastal_deadline/scripts/verify_inundation_raster_integrity.R")

library(parallel)
library(doParallel)

start_time <- Sys.time()
cores <- 3
blank.after.log.flag <- FALSE

#1. Get coastal county subdivisions -------------------------------------
west.cs.conus.sf   <- read_sf(dsn=paste(census.output.dir,"county_subdivisions_conus.gpkg",sep="/")) %>% filter(abb %in% west.states)
east.cs.conus.sf   <- read_sf(dsn=paste(census.output.dir,"county_subdivisions_conus.gpkg",sep="/")) %>% filter(abb %in% east.states)
west.cs.conus.fips <- west.cs.conus.sf %>% st_drop_geometry %>% filter(abb %in% west.states) %>% select(GEOID) %>% pull()
east.cs.conus.fips <- east.cs.conus.sf %>% st_drop_geometry %>% filter(abb %in% east.states) %>% select(GEOID) %>% pull()

#County Subdivisions in PR, VI, GU
cs.coastal.pr.sf    <- read_sf(dsn=paste(census.output.dir,"county_subdivisions_pr.gpkg",sep="/"))
cs.coastal.vi.sf    <- read_sf(dsn=paste(census.output.dir,"county_subdivisions_vi.gpkg",sep="/"))
cs.coastal.pr.vi.sf <- rbind(cs.coastal.pr.sf, cs.coastal.vi.sf) #bind them to process together since they have the same CRS
cs.coastal.gu.sf    <- read_sf(dsn=paste(census.output.dir,"county_subdivisions_gu.gpkg",sep="/"))

cs.coastal.pr.fips    <- cs.coastal.pr.sf %>% st_drop_geometry() %>% select(GEOID) %>% pull()
cs.coastal.vi.fips    <- cs.coastal.vi.sf %>% st_drop_geometry() %>% select(GEOID) %>% pull()
cs.coastal.pr.vi.fips <- c(cs.coastal.vi.fips,cs.coastal.pr.fips)
cs.coastal.gu.fips    <- cs.coastal.gu.sf %>% st_drop_geometry() %>% select(GEOID) %>% pull()

print(paste("WEST: ", nrow(west.cs.conus.sf), " coastal subdivisions in ", length(unique(west.cs.conus.sf$abb))," states", sep=""))
print(paste("EAST: ", nrow(east.cs.conus.sf), " coastal subdivisions in ", length(unique(east.cs.conus.sf$abb))," states", sep=""))
print(paste("PR & VI: ", nrow(cs.coastal.pr.vi.sf), " coastal subdivisions", sep=""))
print(paste("GU: ", nrow(cs.coastal.gu.sf), " coastal subdivisions", sep=""))

#2. Clip layers to coastal county subdivisions in CONUS-----------------------------------------------------------------
#debug
#inundation.layer <- conus.inundation.layers.list[1]
# num_cores <- 4
# cl <- makeCluster(num_cores, outfile=preprocess.inundation.layers.log)
# registerDoParallel(cl)
#WEST
tic()
for(inundation.layer in unprocessed.inundation.rasters.WEST$fullname)
{
  i <- 1
  print(inundation.layer)

  scenario <- tools::file_path_sans_ext(basename(inundation.layer))

  #reinitialize dir for output
  inundation.layer.crop.file.basename <- file_path_sans_ext(basename(inundation.layer))
  current.output.dir = paste(cropped.output.dir, inundation.layer.crop.file.basename,sep="/")
  print(paste("cleaning output dir ", current.output.dir,sep=''))
  unlink(current.output.dir, recursive = TRUE)
  dir.create(current.output.dir, recursive = TRUE)

  #log file
  preprocess.east.inundation.layer.logfile <- paste(log.dir, "/", scenario, "_preprocess_inundation_layers.log", sep= "")
  unlink(preprocess.east.inundation.layer.logfile)

  # Register parallel backend
  cl <- makeCluster(cores, outfile = preprocess.east.inundation.layer.logfile)
  registerDoParallel(cl)
  result <- foreach(cs = west.cs.conus.fips,
          .packages = c("sf","terra","dplyr","tools", "crayon"),
          .verbose=TRUE,
          .export =  c(configuration.variables, "cs.sf","inundation.layer.crop.file.basename","inundation.layer")) %dopar%
          {
            #print(cs)
            clip.layer(.cs.fips = cs,
                       .cs.sf   = cs.sf,
                       .inundation.layer.name = inundation.layer,
                       .inundation.layer.crop.file.basename = inundation.layer.crop.file.basename)
          }
  print("FINISHED")
  stopCluster(cl)
}
toc()

#EAST
tic()
for(inundation.layer in unprocessed.inundation.rasters.EAST$fullname)
{
  i <- 1
  print(inundation.layer)

  scenario <- tools::file_path_sans_ext(basename(inundation.layer))

  #reinitialize dir for output
  inundation.layer.crop.file.basename <- file_path_sans_ext(basename(inundation.layer))
  current.output.dir = paste(cropped.output.dir, inundation.layer.crop.file.basename,sep="/")
  print(paste("cleaning output dir ", current.output.dir,sep=''))
  unlink(current.output.dir, recursive = TRUE)
  dir.create(current.output.dir, recursive = TRUE)

  #log file
  preprocess.east.inundation.layer.logfile <- paste(log.dir, "/", scenario, "_preprocess_inundation_layers.log", sep= "")
  unlink(preprocess.east.inundation.layer.logfile)

  # Register parallel backend
  cl <- makeCluster(cores, outfile = preprocess.east.inundation.layer.logfile)
  registerDoParallel(cl)
  result <- foreach(cs = east.cs.conus.fips,
                    .packages = c("sf","terra","dplyr","tools", "crayon"),
                    .verbose=TRUE,
                    .export =  c(configuration.variables, "cs.sf","inundation.layer.crop.file.basename","inundation.layer")) %dopar%
    {
      #print(cs)
      clip.layer(.cs.fips = cs,
                 .cs.sf   = cs.sf,
                 .inundation.layer.name = inundation.layer,
                 .inundation.layer.crop.file.basename = inundation.layer.crop.file.basename)
    }
  print("FINISHED")
  stopCluster(cl)
}
toc()

#PR-VI
tic()
for(inundation.layer in unprocessed.inundation.rasters.PR.VI$fullname)
{
  i <- 1
  print(inundation.layer)

  scenario <- tools::file_path_sans_ext(basename(inundation.layer))

  #reinitialize dir for output
  inundation.layer.crop.file.basename <- file_path_sans_ext(basename(inundation.layer))
  current.output.dir = paste(cropped.output.dir, inundation.layer.crop.file.basename,sep="/")
  print(paste("cleaning output dir ", current.output.dir,sep=''))
  unlink(current.output.dir, recursive = TRUE)
  dir.create(current.output.dir, recursive = TRUE)

  #log file
  preprocess.pr.vi.inundation.layer.logfile <- paste(log.dir, "/", scenario, "_preprocess_inundation_layers.log", sep= "")
  unlink(preprocess.pr.vi.inundation.layer.logfile)

  # Register parallel backend
  cl <- makeCluster(cores, outfile = preprocess.pr.vi.inundation.layer.logfile)
  registerDoParallel(cl)
  result <- foreach(cs = cs.pr.vi.fips,
                    .packages = c("sf","terra","dplyr","tools", "crayon"),
                    .verbose=TRUE,
                    .export =  c(configuration.variables, "cs.sf","inundation.layer.crop.file.basename","inundation.layer")) %dopar%
    {
      #print(cs)
      clip.layer(.cs.fips = cs,
                 .cs.sf   = cs.sf,
                 .inundation.layer.name = inundation.layer,
                 .inundation.layer.crop.file.basename = inundation.layer.crop.file.basename)
    }
  print("FINISHED")
  stopCluster(cl)
}
toc()

#GU
tic()
for(inundation.layer in unprocessed.inundation.rasters.GU$fullname)
{
  i <- 1
  print(inundation.layer)

  scenario <- tools::file_path_sans_ext(basename(inundation.layer))

  #reinitialize dir for output
  inundation.layer.crop.file.basename <- file_path_sans_ext(basename(inundation.layer))
  current.output.dir = paste(cropped.output.dir, inundation.layer.crop.file.basename,sep="/")
  print(paste("cleaning output dir ", current.output.dir,sep=''))
  unlink(current.output.dir, recursive = TRUE)
  dir.create(current.output.dir, recursive = TRUE)

  #log file
  preprocess.gu.inundation.layer.logfile <- paste(log.dir, "/", scenario, "_preprocess_inundation_layers.log", sep= "")
  unlink(preprocess.gu.inundation.layer.logfile)

  # Register parallel backend
  cl <- makeCluster(cores, outfile = preprocess.gu.inundation.layer.logfile)
  registerDoParallel(cl)
  result <- foreach(cs = cs.gu.fips,
                    .packages = c("sf","terra","dplyr","tools", "crayon"),
                    .verbose  = TRUE,
                    .export   = c(configuration.variables, "cs.sf","inundation.layer.crop.file.basename","inundation.layer")) %dopar%
  {
      #print(cs)
      clip.layer(.cs.fips = cs,
                 .cs.sf   = cs.sf,
                 .inundation.layer.name = inundation.layer,
                 .inundation.layer.crop.file.basename = inundation.layer.crop.file.basename)
  }
  print("FINISHED")
  stopCluster(cl)
}
toc()

#4. Clip layers to coastal county subdivisions in PR-VI-----------------------------------------------------------------
#PR-VI
# for(inundation.layer in unprocessed.inundation.rasters.PR.VI$fullname)
# {
#   i <- 1
#   print(inundation.layer)
#   inundation.layer.raster     <- rast(inundation.layer)
#   inundation.layer.raster.crs <- st_crs(inundation.layer.raster)
#
#   #reinitialize dir for output
#   inundation.layer.crop.file.basename <- file_path_sans_ext(basename(inundation.layer))
#   current.output.dir = paste(cropped.output.dir, inundation.layer.crop.file.basename,sep="/")
#   print(paste("cleaning output dir ", current.output.dir,sep=''))
#   unlink(current.output.dir, recursive = TRUE)
#   dir.create(current.output.dir, recursive = TRUE)
#   #export.variables <- c("inundation.layer.raster")
#   #cs <-  "7207129985"
#   foreach(cs = cs.pr.vi.fips, .packages = c("sf","terra","dplyr","tools", "crayon"), .verbose=TRUE) %do%
#     {
#       print(paste(i,"/",length(cs.pr.vi.fips), sep=""))
#       clip.layer(.cs.fips = cs, .cs.sf = cs.pr.vi.sf, .inundation.raster = inundation.layer.raster, .inundation.layer.crop.file.basename = inundation.layer.crop.file.basename)
#       i <- i + 1
#     }
# }
#
#
#
#
# #5. Clip layers to coastal county subdivisions in GU-----------------------------------------------------------------
# for(inundation.layer in unprocessed.inundation.rasters.GU$fullname) #this list is created in verify_inundation_raster_integrity.R
# {
#   i <- 1
#   print(inundation.layer)
#   inundation.layer.raster     <- rast(inundation.layer)
#   inundation.layer.raster.crs <- st_crs(inundation.layer.raster)
#
#   #reinitialize dir for output
#   inundation.layer.crop.file.basename <- file_path_sans_ext(basename(inundation.layer))
#   current.output.dir = paste(cropped.output.dir, inundation.layer.crop.file.basename,sep="/")
#   print(paste("cleaning output dir ", current.output.dir,sep=''))
#   unlink(current.output.dir, recursive = TRUE)
#   dir.create(current.output.dir, recursive = TRUE)
#   #export.variables <- c("inundation.layer.raster")
#   #cs <-  "7207129985"
#   foreach(cs = cs.coastal.gu.fips, .packages = c("sf","terra","dplyr","tools", "crayon"), .verbose=TRUE) %do%
#     {
#       print(paste(i,"/",length(cs.coastal.gu.fips), sep=""))
#       clip.layer(.cs.fips = cs, .cs.sf = cs.coastal.gu.sf, .inundation.raster = inundation.layer.raster, .inundation.layer.crop.file.basename = inundation.layer.crop.file.basename)
#       i <- i + 1
#     }
# }

end_time <- Sys.time()
print(end_time - start_time)

end_time <- Sys.time()
print(end_time - start_time)


