#Loiza inundated infrastructure analysis
source("~/coastal_deadline/scripts/configuration.R")
library(doParallel)
library(parallel)
library(foreach)

ucc.df <- read.csv("/home/rama/coastal_deadline/data/CJM/United Church of Christ.csv")
ucc.sf <- st_as_sf(ucc.df, coords = c("X", "Y"), crs = 4326) %>% write_sf(., dsn="/home/rama/coastal_deadline/data/CJM/UCC.gpkg", driver="GPKG", delete_layer = TRUE)
