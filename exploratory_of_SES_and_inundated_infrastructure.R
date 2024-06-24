# exploratory analysis of SES and inundated infrastructure
source("~/coastal_deadline/scripts/configuration.R")
svi.df <- read_sf("/media/rama/datastore3/CDC/SVI2020_US_county.gdb", layer= "SVI2020_US_county") %>%
          st_drop_geometry() %>%
          select(FIPS, RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4,RPL_THEMES)

inundated.sf <- read_sf(paste(inundation.summaries.dir,"/EAST_is_flooded_sea-2050-high-02-EAST_summary.gpkg", sep= "")) %>%
                mutate(inundated = if_else(per.all>0, 1,0))

#aggregate total and inundated infrastructure by state
total.by.state.df <- inundated.sf %>%
            st_drop_geometry() %>%
            select(.,county.fips,all.t,all.i) %>%
            group_by(county.fips) %>%
            summarise(all.i = sum(all.i),
                      all.t = sum(all.t)) %>%
            mutate(per.i = if_else (all.i==0, 0,round((all.i/all.t) * 100, 2))) %>%
            sp::merge(x = .,
                      y = svi.df,
                      by.x = "county.fips",
                      by.y = "FIPS") %>%
            arrange(per.i) %>%
            select(-all.i, -all.t)
plot(total.by.state.df$per.i, total.by.state.df$RPL_THEME1)
plot(total.by.state.df$per.i, total.by.state.df$RPL_THEME2)
plot(total.by.state.df$per.i, total.by.state.df$RPL_THEME3)
plot(total.by.state.df$per.i, total.by.state.df$RPL_THEME4)
plot(total.by.state.df$per.i, total.by.state.df$RPL_THEMES)


##%>%
            # pivot_longer(data = .,
            #              cols = c(per.i,RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4, RPL_THEMES))
total.by.state.df %>% filter(per.i==0) %>%nrow()
View(total.by.state.df)



table(inundated.sf$inundated)
