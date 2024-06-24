source("~/coastal_deadline/scripts/configuration.R")

fgdb <- paste(data.dir,"/EPA/brownfields/FRS_INTERESTS.gdb", sep="")

l <- st_layers(fgdb)
qry <- "SELECT * FROM ACRES"
brownfields.sf <- read_sf(dsn= fgdb, query = qry)


