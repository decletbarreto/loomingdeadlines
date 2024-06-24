source("~/coastal_deadline/scripts/configuration.R")
library(readxl)
library(modeest)

sf_use_s2(TRUE)
simplify.tolerance <- 100

cs.gu.for.join.sf    <- cs.gu.sf    %>% select(GEOID)
cs.pr.vi.for.join.sf <- cs.pr.vi.sf %>% select(GEOID)
cs.conus.for.join.sf <- cs.conus.sf %>% select(GEOID)

npl.sf <- read_sf("~/coastal_deadline/data/EPA/NPL_Boundaries.gdb", layer="SITE_BOUNDARIES_SF") %>%
          mutate(STATE_CODE = if_else(is.na(STATE_CODE), substr(EPA_ID,1,2), STATE_CODE)) %>% #fill in NA STATE_CODE with first 2 characters of EPA_ID
          st_cast(., "MULTIPOLYGON")

npl.unique.epa.id.df <- npl.sf %>% st_drop_geometry() %>% distinct(EPA_ID, .keep_all = TRUE)
npl.unique.epa.id.df2 <- npl.sf %>% st_drop_geometry() %>% distinct(EPA_ID, .keep_all = TRUE) %>%
                         filter(STATE_CODE %in% c(east.states.in.analysis, west.states.in.analysis,"PR","VI","GU"))

table(npl.sf$STATE_CODE, useNA = 'always') # there should now be zero NAs
npl.conus.sf  <- npl.sf %>% filter(STATE_CODE %in% c(east.states.in.analysis, west.states.in.analysis))
npl.gu.sf     <- npl.sf %>% filter(STATE_CODE == "GU")
npl.pr.vi.sf  <- npl.sf %>% filter(STATE_CODE %in% c("PR", "VI"))

#fix invalid geometries
#CONUS
npl.conus.valid.idx   <- npl.conus.sf %>% st_is_valid()
npl.conus.valid.sf    <- npl.conus.sf[npl.conus.valid.idx,]
npl.conus.invalid.sf  <- npl.conus.sf[!npl.conus.valid.idx,]
npl.conus.made.valid  <- st_make_valid(npl.conus.invalid.sf)
npl.conus.fixed.sf    <- rbind(npl.conus.valid.sf,npl.conus.made.valid)
nrow(npl.conus.fixed.sf)
npl.conus.fixed.sf <- npl.conus.fixed.sf %>%
                      st_transform(., st_crs(cs.conus.sf)) %>%
                      st_simplify(., dTolerance = simplify.tolerance) %>%
                      st_intersection(., cs.conus.sf) %>%
                      mutate(a = st_area(.)) %>%
                      group_by(EPA_ID) %>%
                      summarize(max_area = max(a)) %>%
                      #join county subdivision ID
                      st_join(x = .,
                              y = cs.conus.for.join.sf,
                              join = st_within,
                              left = FALSE) %>%
                      st_transform(., 4326)


#GU
npl.gu.valid.idx   <- npl.gu.sf %>% st_is_valid()
npl.gu.valid.sf    <- npl.gu.sf[npl.gu.valid.idx,]
npl.gu.invalid.sf  <- npl.gu.sf[!npl.gu.valid.idx,]
npl.gu.made.valid  <- st_make_valid(npl.gu.invalid.sf)
npl.gu.fixed.sf    <- rbind(npl.gu.valid.sf,npl.gu.made.valid)
nrow(npl.gu.fixed.sf)
npl.gu.fixed.sf    <- npl.gu.fixed.sf %>%
                      st_transform(., st_crs(cs.gu.sf)) %>%
                      st_simplify(., dTolerance = simplify.tolerance) %>%
                      st_intersection(., cs.gu.sf) %>%
                      mutate(a = st_area(.)) %>%
                      group_by(EPA_ID) %>%
                      summarize(max_area = max(a)) %>%
                      mutate(inside_leveed_area = 'n') %>%
                      #join county subdivision ID
                      st_join(x = .,
                              y = cs.gu.for.join.sf,
                              join = st_within,
                              left = FALSE) %>%
                      st_transform(., 4326)

#PR-VI
npl.pr.vi.valid.idx   <- npl.pr.vi.sf %>% st_is_valid()
npl.pr.vi.valid.sf    <- npl.pr.vi.sf[npl.pr.vi.valid.idx,]
npl.pr.vi.invalid.sf  <- npl.pr.vi.sf[!npl.pr.vi.valid.idx,]
npl.pr.vi.made.valid  <- st_make_valid(npl.pr.vi.invalid.sf)
npl.pr.vi.fixed.sf    <- rbind(npl.pr.vi.valid.sf,npl.pr.vi.made.valid)
npl.pr.vi.fixed.sf    <- npl.pr.vi.fixed.sf %>%
                         st_transform(., st_crs(cs.pr.vi.sf)) %>%
                         st_simplify(., dTolerance = simplify.tolerance) %>%
                         st_intersection(., cs.pr.vi.sf) %>%
                         mutate(a = st_area(.)) %>%
                         group_by(EPA_ID) %>%
                         summarize(max_area = max(a)) %>%
                         mutate(inside_leveed_area = 'n') %>%
                         #join county subdivision ID
                          st_join(x = .,
                                  y = cs.pr.vi.for.join.sf,
                                  join = st_within,
                                  left = FALSE) %>%
                          st_transform(., 4326)

#add flag for NPL sites located within or outside leveed areas. only for CONUS since PR, VI, GU don't have leveed areas
state.regions.without.leveed.areas.sf <- regions.without.leveed.areas.sf %>%
                                         mutate(inside_leveed_area = "n") %>%
                                         select(inside_leveed_area)
npl.conus.leveed.areas.sf <- npl.conus.fixed.sf %>%
                             st_transform(x = ., st_crs(state.regions.without.leveed.areas.sf)) %>%
                             st_join(x    = .,
                                     y    = state.regions.without.leveed.areas.sf,
                                     join = st_intersects,
                                     left = TRUE) %>%
                             mutate(inside_leveed_area = if_else(is.na(inside_leveed_area),"y",inside_leveed_area)) %>%
                             st_transform(., 4326)

npl.intersect.cs.all <- rbind(npl.conus.leveed.areas.sf,npl.gu.fixed.sf,npl.pr.vi.fixed.sf) %>%
                        #join NPL atttibute data back
                        merge(x = .,
                              y = npl.unique.epa.id.df,
                              by = "EPA_ID") %>%
                        rename(county.subdivision.geoid = GEOID) %>%
                        select(all_of(c(npl.cols.list, "county.subdivision.geoid"))) %>%
                        write_sf(., dsn=paste(data.dir,"/EPA/SuperfundSiteBoundaries.gpkg", sep=""), driver="GPKG", delete_layer = TRUE)
total.in.analysis.areas <- nrow(npl.conus.sf) + nrow(npl.gu.sf) + nrow(npl.pr.vi.sf)
total.fixed <- nrow(npl.intersect.cs.all)
total.fixed/1159


