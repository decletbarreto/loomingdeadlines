source("~/coastal_deadline/scripts/configuration.R")
inundated.infra.sf             <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep=""))
infra.sf <- read_sf(paste(output.dir, "/agol/all_critical_infrastructure/all_critical_infrastructure.shp",sep=""))
ah.by.state <- infra.sf %>% st_drop_geometry() %>% filter(type %in% c("PH", "AFF")) %>% group_by(state) %>% summarise(freq = n())
housing.types = c("Affordable Housing", "Public Housing")
ah.2050.hi.2x.df <- inundated.infra.sf %>%
                    st_drop_geometry() %>%
                    filter(type %in% housing.types &
                           `2050-hi-02` == 1) %>%
                    group_by(state) %>%
                    summarise(inundated.freq.2050.hi.2x = n())

ah.2030.hi.2x.df <- inundated.infra.sf %>%
  st_drop_geometry() %>%
  filter(type %in% housing.types &
           `2030-hi-02` == 1) %>%
  group_by(state) %>%
  summarise(inundated.freq.2030.hi.2x = n())

ah.2020.hi.2x.df <- inundated.infra.sf %>%
  st_drop_geometry() %>%
  filter(type %in% housing.types &
           `2020-hi-02` == 1) %>%
  group_by(state) %>%
  summarise(inundated.freq.2020.hi.2x = n())

ah.by.state2 <- left_join(x = ah.by.state,
                          y = ah.2020.hi.2x.df,
                          by = "state") %>%
                left_join(x = .,
                          y = ah.2030.hi.2x.df,
                          by = "state") %>%
                left_join(x = .,
                          y = ah.2050.hi.2x.df,
                          by = "state")






