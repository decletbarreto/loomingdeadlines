library(terra)
library(dplyr)
library(tictoc)
library(foreach)
library(doParallel)

inundation.rasters.dir <- "~/coastal_deadline/data/inundation_layers"
cropped.rasters.dir    <- "~/coastal_deadline/data/output/inundation_layers_crop"
inundation.rasters.list <- list.files(inundation.rasters.dir, pattern="*.tif$", full.names = T, recursive = T)
inundation.rasters.list <- grep("is_flooded", inundation.rasters.list, value=TRUE)
# plot rasters ---------------------------------------------------------------------
# i <- 1
# results <- foreach(raster.name = inundation.rasters.list,
#                    .packages = c("terra"),
#                    .verbose  = TRUE
#
#                   ) %dopar%
#             {
#               print(paste(i, "/", length(inundation.rasters.list), "|", raster.name, sep=""))
#               inundation.rast <- rast(raster.name)
#               #minmax(inundation.rast)
#               pdf.dir <- "~/coastal_deadline/data/output/accuracy_checks/inundation_layer_maps/"
#               pdf.filename <- paste(pdf.dir,"/",  basename(raster.name), ".pdf", sep="")
#               pdf(pdf.filename)
#               plot(inundation.rast, main = basename(raster.name))
#               #text(5, 5, "Text to display", cex = 1.5, col = "red")
#               dev.off()
#               i <- i + 1
#             }


# build table of inundation rasters ----------------------------------------------------------------------
inundation.rasters.df <- data.frame()
for(r in inundation.rasters.list)
{
  scenario <- basename(dirname(r))
  filename <- tools::file_path_sans_ext(basename(r))
  fullname  <- r
  row <- data.frame(scenario = scenario, filename = filename, fullname = fullname)
  inundation.rasters.df <- rbind(inundation.rasters.df, row)
}

table(inundation.rasters.df$scenario) #there should be 4 of each

# build table of inundation raster cropped status ----------------------------------------------------------------------
cropped.raster.dirs.list <- list.dirs(cropped.rasters.dir, full.names = T, recursive = F)
cropped.rasters.dirs.df  <- data.frame()
for(r in cropped.raster.dirs.list)
{
  filename <- basename(r)
  n.files <- length(list.files(r, pattern = "*.gpkg$"))

  row <- data.frame(filename = filename, cropped = "y", number.of.gpkgs = n.files)
  cropped.rasters.dirs.df <- rbind(cropped.rasters.dirs.df, row)
}

#join raster manifest to cropped status
inundation.rasters.join.df <- inundation.rasters.df %>%
                              left_join(x = .,
                                        y = cropped.rasters.dirs.df,
                                        by = "filename") %>%
                              arrange(cropped)

unprocessed.inundation.rasters.GU    <- inundation.rasters.join.df %>% filter(is.na(cropped)) %>% filter(grepl("GU",filename))
unprocessed.inundation.rasters.PR.VI <- inundation.rasters.join.df %>% filter(is.na(cropped)) %>% filter(grepl("PRVI",filename))
unprocessed.inundation.rasters.EAST  <- inundation.rasters.join.df %>% filter(is.na(cropped)) %>% filter(grepl("EAST",filename))
unprocessed.inundation.rasters.WEST  <- inundation.rasters.join.df %>% filter(is.na(cropped))  %>% filter(grepl("WEST",filename))
