#prepare draft figures for min-report
dir <- "/home/rama/coastal_deadline/data/data_for_report_graphics"
library(ggplot2)

year.projection.frequency.list2 <- c("2020-in-02","2020-in-12", "2020-in-26", "2020-il-02", "2020-il-12","2020-il-26","2020-hi-02", "2020-hi-12","2020-hi-26",
                   "2030-in-02","2030-in-12", "2030-in-26", "2030-il-02", "2030-il-12","2030-il-26","2030-hi-02", "2030-hi-12","2030-hi-26",
                   "2050-in-02","2050-in-12", "2050-in-26", "2050-il-02", "2050-il-12","2050-il-26","2050-hi-02", "2050-hi-12","2050-hi-26",
                   "2100-in-02","2100-in-12", "2100-in-26", "2100-il-02", "2100-il-12","2100-il-26","2100-hi-02", "2100-hi-12","2100-hi-26")

year.projection.frequency.list <- c("2020-int_low-02", "2020-int_low-12", "2020-int_low-26", "2020-int-02", "2020-int-12", "2020-int-26", "2020-high-02", "2020-high-12", "2020-high-26",
                                    "2030-int_low-02", "2030-int_low-12", "2030-int_low-26", "2030-int-02", "2030-int-12", "2030-int-26", "2030-high-02", "2030-high-12", "2030-high-26",
                                    "2050-int_low-02", "2050-int_low-12", "2050-int_low-26", "2050-int-02", "2050-int-12", "2050-int-26", "2050-high-02", "2050-high-12", "2050-high-26",
                                    "2100-int_low-02", "2100-int_low-12", "2100-int_low-26", "2100-int-02", "2100-int-12", "2100-int-26", "2100-high-02", "2100-high-12", "2100-high-26")

cejst.sf <- read_sf("/media/rama/datastore3/CJEST/usa.shp")
sn_c.labels <- c("not disadvantaged", "disadvantaged")
npl.dir <- paste(output.dir, "/npl_inundated",sep="")

#2030
col     <- "2030-hi-02"
col.npl <- "2030-high-02"
# "2050-high-02"))
data.df <- read.csv(paste(dir,"/frequency_of_inundated_infra_by_state_by_disadvantaged_status_", col, ".csv", sep="")) %>%
           mutate(key = paste(state, disadvantaged, sep="|"))
sum(data.df$frequency)
#npl inundated infra
inundated.npl.files <- list.files(npl.dir, pattern = paste0("^is_flooded_sea.*", col.npl, ".*"), full.names = TRUE)
inundated.npl.sf    <- lapply(inundated.npl.files, function(file) {sf::st_read(file) %>% st_transform(., crs=st_crs("EPSG:4326"))}) %>%
                        do.call(rbind, .) %>%
                        st_join(x    = .,
                                y    = cejst.sf,
                                join = st_within)

inundated.npl.summary.df <- inundated.npl.sf %>%
                            st_drop_geometry() %>%
                            rename(SF.npl = SF, SN_C.npl = SN_C) %>%
                            group_by(SF.npl, SN_C.npl) %>%
                            summarise(freq.npl = n()) %>%
                            ungroup() %>%
                            mutate(key = paste(SF.npl, SN_C.npl, sep="|"))
data.join.2030 <- left_join(x = data.df,
                       y = inundated.npl.summary.df,
                       by = "key") %>%
                       mutate(freq.npl = if_else(is.na(freq.npl), 0, freq.npl)) %>%
                       mutate(frequency = (frequency + freq.npl)) %>%
                       select(state, disadvantaged, frequency) %>%
                       mutate(year = 2030)

#2050
col     <- "2050-hi-02"
col.npl <- "2050-high-02"
data.df <- read.csv(paste(dir,"/frequency_of_inundated_infra_by_state_by_disadvantaged_status_", col, ".csv", sep="")) %>%
  mutate(key = paste(state, disadvantaged, sep="|"))
sum(data.df$frequency)

#npl inundated infra
inundated.npl.files <- list.files(npl.dir, pattern = paste0("^is_flooded_sea.*", col.npl, ".*"), full.names = TRUE)
inundated.npl.sf    <- lapply(inundated.npl.files, function(file) {sf::st_read(file) %>% st_transform(., crs=st_crs("EPSG:4326"))}) %>%
  do.call(rbind, .) %>%
  st_join(x    = .,
          y    = cejst.sf,
          join = st_within)

inundated.npl.summary.df <- inundated.npl.sf %>%
  st_drop_geometry() %>%
  rename(SF.npl = SF, SN_C.npl = SN_C) %>%
  group_by(SF.npl, SN_C.npl) %>%
  summarise(freq.npl = n()) %>%
  ungroup() %>%
  mutate(key = paste(SF.npl, SN_C.npl, sep="|"))
data.join.2050 <- left_join(x = data.df,
                            y = inundated.npl.summary.df,
                            by = "key") %>%
  mutate(freq.npl = if_else(is.na(freq.npl), 0, freq.npl)) %>%
  mutate(frequency = (frequency + freq.npl)) %>%
  select(state, disadvantaged, frequency) %>%
  mutate(year = 2050)

data.join.2050$state <- as.factor(data.join.2050$state)
data.join.2050$state <- factor(data.join.2050$state, levels = rev(levels(data.join.2050$state)))

#data.join.df <- rbind(data.join.2030, data.join.2050)
# Plot stacked bar chart
ggplot(data.join.2050, aes(x = frequency, y = reorder(state, -frequency), fill = as.factor(disadvantaged))) +
  xlim(c(0,400)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of inundated assets", y = " ", fill = "Disadvantaged status", subtitle = "2050, High scenario, twice a year") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.grid  = element_blank(),
        plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                    labels = sn_c.labels) +
  ggtitle("Number of inundated assets by disadvantaged status")

data.join.2050 %>% write.csv(., paste("/home/rama/coastal_deadline/data/data_for_report_graphics/figure2_data.csv",sep=""),row.names = F)

f <- paste(output.dir,"/inundated_infrastructure_frequency_by_state/inundated_infrastructure_frequency_by_state_", col, ".pdf", sep="")


# # Create the plot
# ggplot(data, aes(x = state, y = frequency, fill = as.factor(disadvantaged))) +
#   geom_bar(stat = "identity", position = "stack") +
#   facet_grid(. ~ year, scales = "free_x") +
#   labs(title = "Frequency by State and Disadvantaged",
#        x = "State",
#        y = "Frequency",
#        fill = "Disadvantaged") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



#34.8% percent of communities in coastal counties are considered disadvantaged, but they are home to 57.5 percent of the infrastructure at risk in the near term.  2050 hi 2x
coastal.counties.geoids <- coastal.counties.sf %>% st_drop_geometry() %>% select(countyfips) %>% pull()
cejst.sf2 <- cejst.sf %>%  mutate(county.geoid = substr(GEOID10,1,5)) %>% filter(county.geoid %in% coastal.counties.geoids) %>% write_sf(., dsn=paste(tmp.dir,"cejst_coastal.gpkg",sep=""), driver= "GPKG", delete_layer = T)

cejst.sf2 %>% st_drop_geometry() %>% group_by(SN_C) %>% summarize(freq = n())
8361/(15682 + 8361)

#1966 inundated infra assets in 2050 hi 02.
#1131 of those are in DACs
#1131/1966 *100 = 57.5


#12989 inundated infra assets in 2100 hi 02.
# of those are in DACs
#7552/12989 *100 = 58.1%


#all infra 152315
#infra in DACs
79877/152315
