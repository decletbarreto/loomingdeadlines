#analysis of SES in CTs with at least one piece of inundated infra in scenario
source("~/coastal_deadline/scripts/configuration.R")

ses.percent.cols <- c("DM_B", "DM_AI", "DM_A", "DM_HI", "DM_W", "DM_H","DM_O", "UF_PFS","P100_PFS", "LLEF_PFS")
cjest.cols <- c("GEOID10", "SN_C","TPF", "SF", ses.percent.cols , "geom")

#CEJST data
qry <- paste("SELECT ", paste(cjest.cols, collapse = ","), " FROM cejst_nad83", sep="")
cejst.dsn <- "/media/rama/datastore1/UCS_projects/climate_shop/2024/ReAmp_request/data/cejst_nad83.gpkg"
cejst.sf <- read_sf(dsn=cejst.dsn, query = qry) %>% st_transform(., crs=st_crs(4326)) %>%
  mutate(across(all_of(ses.percent.cols), ~ round(. * 100, digits = 2)))


inundated.sf <- read_sf(paste(output.dir, "/agol/inundated_critical_infrastructure", sep="")) %>% st_transform(., crs=st_crs(4326))

S2050.hi.12.sf <- inundated.ci.sf %>% filter(`2050-in-12` == 1)

CTs.in.S2050.hi.12.sf <- st_join(x = cejst.sf,
                                 y = S2050.hi.12.sf,
                                 join = st_contains,
                                 left=FALSE
                                ) %>% distinct(., GEOID10, .keep_all = TRUE) %>%
                        select(all_of(cjest.cols))
