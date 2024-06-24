source("~/coastal_deadline/scripts/configuration.R")

library(foreign)
library(data.table)
data.dir                 <- "~/coastal_deadline/data"

frs.data.dir       <- paste(data.dir,"/EPA/national_combined",sep="")
frs.facilities.csv <- (paste(frs.data.dir, "NATIONAL_FACILITY_FILE.CSV",sep="/"))
frs.facilities.df  <- fread(frs.facilities.csv)
colnames(frs.facilities.df)

naics.df <- fread(paste(data.dir,"/EPA/national_combined/NATIONAL_NAICS_FILE.CSV",sep=""))
head(naics.df)

sewage.codes <- naics.df %>%
                filter(CODE_DESCRIPTION %in% c("SEWAGE TREATMENT FACILITIES.","SEWAGE TREATMENT FACILITIES","WATER, SEWAGE AND OTHER SYSTEMS")) %>%
                select(CODE_DESCRIPTION, NAICS_CODE) %>%
                distinct(CODE_DESCRIPTION, .keep_all = TRUE)

#get registry IDs of WWTPs by NAICS.
wwtp.df <- naics.df %>%
                 filter(CODE_DESCRIPTION %in% c("SEWAGE TREATMENT FACILITIES.","SEWAGE TREATMENT FACILITIES","WATER, SEWAGE AND OTHER SYSTEMS")) %>%
                 select(REGISTRY_ID, CODE_DESCRIPTION, NAICS_CODE) %>%
                 distinct(REGISTRY_ID, .keep_all = TRUE)
nrow(wwtp.df)
wwtp.facilities.sf <- frs.facilities.df %>%
                      filter(REGISTRY_ID %in% wwtp.df$REGISTRY_ID &
                             FIPS_CODE %in% coastal.counties.fips)
nrow(wwtp.facilities.sf) #in coastal counties

wwtp.facilities.valid.sf <- wwtp.facilities.sf %>%
                            filter(!is.na(LATITUDE83) &
                                   !is.na(LONGITUDE83)) %>%
                            select(REGISTRY_ID,PRIMARY_NAME,LOCATION_ADDRESS,SUPPLEMENTAL_LOCATION,CITY_NAME,COUNTY_NAME,STATE_CODE,POSTAL_CODE,LATITUDE83,LONGITUDE83)  %>%
                            st_as_sf(x=.,coords=c("LONGITUDE83","LATITUDE83"), crs=st_crs(4326))
write_sf(wwtp.facilities.valid.sf, dsn=paste(data.dir,"wwtp/wwtp.gpkg",sep="/"), driver="GPKG", delete_layer = TRUE)

