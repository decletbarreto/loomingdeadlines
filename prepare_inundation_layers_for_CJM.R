source("~/coastal_deadline/scripts/configuration.R")
# matthews county VA
# Wicomico county, MD
# Pamlico County NC
# Beaufort County NC
destination_dir <- "/home/rama/coastal_deadline/data/CJM"
scenario.cols <- c("2020-int-02","2020-int-12", "2020-int-26", "2020-int_low-02", "2020-int_low-12","2020-int_low-26","2020-high-02", "2020-high-12","2020-high-26",
                   "2030-int-02","2030-int-12", "2030-int-26", "2030-int_low-02", "2030-int_low-12","2030-int_low-26","2030-high-02", "2030-high-12","2030-high-26",
                   "2050-int-02","2050-int-12", "2050-int-26", "2050-int_low-02", "2050-int_low-12","2050-int_low-26","2050-high-02", "2050-high-12","2050-high-26",
                   "2100-int-02","2100-int-12", "2100-int-26", "2100-int_low-02", "2100-int_low-12","2100-int_lo-26","2100-high-02", "2100-high-12","2100-high-26")
#1. filter counties
county.fips <- c(51115, 24045,37137,37013)
#cjm.counties <- coastal.counties.sf %>% filter(countyfips %in% county.fips)

#2. get EAST inundation layers
EAST.inundation.layers.crop.list <- list.files(path=paste(output.dir, "/inundation_layers_crop", sep=""), pattern=".*EAST.*\\.gpkg$", full.names = TRUE, recursive = TRUE)
EAST.inundation.layers.crop.county.list <-  regmatches(basename(EAST.inundation.layers.crop.list), regexpr("(?<=EAST_).{5}", basename(EAST.inundation.layers.crop.list), perl = TRUE))
listing.df <- data.frame(f = EAST.inundation.layers.crop.list, county =EAST.inundation.layers.crop.county.list)
cjm.files <- listing.df %>%
             filter(county %in% county.fips) %>%
             filter(!str_detect(f, "26-EAST")) %>%
             filter(!str_detect(f, "int_low")) %>%
             select(f)

#merge together files in same scenario
for(scenario in scenario.cols[1])
{
  curr.cjm <- cjm.files %>%  filter(str_detect(f, scenario)) %>% select(f) %>% pull()
  # Read each GeoPackage file into an sf object
  sf_objects <- lapply(curr.cjm, read_sf)

  # Bind the sf objects together
  combined_sf <- do.call(rbind, sf_objects)

  write.sf(combined_sf)

}


# for(file in cjm.files)
# {
#   print(file)
#   file.copy(file, paste0(destination_dir, "/", basename(file)), overwrite =TRUE)
# }






