source("~/coastal_deadline/scripts/configuration.R")
library(ggplot2)
library(viridis)
library(plotly)

scenario.cols <- c("2020-in-02","2020-in-12", "2020-in-26", "2020-il-02", "2020-il-12","2020-il-26","2020-hi-02", "2020-hi-12","2020-hi-26",
                   "2030-in-02","2030-in-12", "2030-in-26", "2030-il-02", "2030-il-12","2030-il-26","2030-hi-02", "2030-hi-12","2030-hi-26",
                   "2050-in-02","2050-in-12", "2050-in-26", "2050-il-02", "2050-il-12","2050-il-26","2050-hi-02", "2050-hi-12","2050-hi-26",
                   "2100-in-02","2100-in-12", "2100-in-26", "2100-il-02", "2100-il-12","2100-il-26","2100-hi-02", "2100-hi-12","2100-hi-26")

inundated.infra.sf <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep=""))
cejst.sf <- read_sf("/media/rama/datastore3/CJEST/usa.shp")

ct2010.ej.df <- read.csv("/media/rama/datastore3/Census/ct2010_ej_variables.csv") %>%
                mutate(GEOID=stringr::str_pad(GEOID, width=11, side="left", pad="0"),
                       PER.WHITE   = round(((white/total)    * 100),2),
                       PER.BLACK   = round(((black/total)    * 100),2),
                       PER.NATIVE  = round(((aian/total)     * 100),2),
                       PER.ASIAN   = round(((asian/total)    * 100),2),
                       PER.HIPI    = round(((nativehi/total) * 100),2),
                       PER.HISPANIC= round(((hispanic/total) * 100),2))

infra.sj <- inundated.infra.sf %>%
            st_join(x = .,
                    y = cejst.sf,
                    join = st_within)
#frequency of inundated infra by state by CJEST status ----
for(col in scenario.cols)
{
  curr.df       <- infra.sj %>%
                   st_drop_geometry() %>%
                   filter(!!sym(col) ==1) %>% select(c("GEOID10", !!col))
  print(paste(col, "|", nrow(curr.df),sep=""))

  sn_c.labels <- c("0" = "not disadvantaged", "1" = "disadvantaged")
  frequency1.df <- curr.df  %>% st_drop_geometry() %>% group_by(GEOID10) %>% summarize(n = n())
  frequency1.cejst.df <- frequency1.df %>%
                         left_join(x  = .,
                                   y  = st_drop_geometry(cejst.sf),
                                   by = "GEOID10")  %>%
                         select(GEOID10, SF, n, SN_C) %>%
                         filter(!is.na(SF)) %>%
                         mutate(SN_C = as.factor(SN_C))

   # Aggregate data by state and SN_C, and count frequency of n
   agg_data <- frequency1.cejst.df %>%
               group_by(SF, SN_C) %>%
               summarise(freq = sum(n)) %>%
               ungroup() %>%
               arrange(SF, SN_C)  # Arrange data for better plotting order

    # Plot stacked bar chart
    ggplot(agg_data, aes(x = as.factor(SF), y = freq, fill = as.factor(SN_C))) +
      #ylim(y.lim) +
      geom_bar(stat = "identity") +
      labs(x = "State", y = "Number of inundated assets", fill = "Disadvantaged status", subtitle = col) +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))  +
      scale_fill_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                        labels = sn_c.labels) +
      ggtitle("Number of inundated assets by disadvantaged status")
    f <- paste(output.dir,"/inundated_infrastructure_frequency_by_state/inundated_infrastructure_frequency_by_state_", col, ".pdf", sep="")
    ggsave(f)

    #save data for Cynthia
    agg_data %>% rename(state = SF,disadvantaged = SN_C, frequency = freq) %>% write.csv(., paste("/home/rama/coastal_deadline/data/data_for_cynthia", "/frequency_of_inundated_infra_by_state_by_disadvantaged_status_", col,".csv", sep=""), row.names = FALSE)
}

#2020 and 2030 by CEJST status
f2020.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2020-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2020=sum(`2020-hi-02`))
f2020.hi.2x.wide <- pivot_wider(f2020.hi.2x, names_from = SN_C, values_from=n.2020) %>% rename(nd.2020 = `0`, d.2020 = `1`)

f2030.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-02`))
f2030.hi.2x.wide <- pivot_wider(f2030.hi.2x, names_from = SN_C, values_from=n.2030) %>%  rename(nd.2030 = `0`, d.2030 = `1`)

hi.2x.merged <- left_join(x = f2020.hi.2x.wide, y = f2030.hi.2x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2030 - nd.2020) / nd.2020)) * 100,
         d.per.change = (((d.2030  - d.2020) /   d.2020)) * 100)


f2020.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2020-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2020=sum(`2020-hi-26`))
f2020.hi.26x.wide <- pivot_wider(f2020.hi.26x, names_from = SN_C, values_from=n.2020) %>% rename(nd.2020 = `0`, d.2020 = `1`)

f2030.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-26`))
f2030.hi.26x.wide <- pivot_wider(f2030.hi.26x, names_from = SN_C, values_from=n.2030) %>%  rename(nd.2030 = `0`, d.2030 = `1`)

hi.26x.merged <- left_join(x = f2020.hi.26x.wide, y = f2030.hi.26x.wide, by="type") %>%
                 mutate(nd.per.change = (((nd.2030 - nd.2020) / nd.2020)) * 100,
                         d.per.change = (((d.2030  - d.2020) /   d.2020)) * 100)





#2030 and 2050 by CEJST status
f2030.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-02`))
f2030.hi.2x.wide <- pivot_wider(f2030.hi.2x, names_from = SN_C, values_from=n.2030) %>% rename(nd.2030 = `0`, d.2030 = `1`)

f2050.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2050-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2050=sum(`2050-hi-02`))
f2050.hi.2x.wide <- pivot_wider(f2050.hi.2x, names_from = SN_C, values_from=n.2050) %>%  rename(nd.2050 = `0`, d.2050 = `1`)

hi.2x.merged <- left_join(x = f2030.hi.2x.wide, y = f2050.hi.2x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2050 - nd.2030) / nd.2030)) * 100,
         d.per.change = (((d.2050  - d.2030) /   d.2030)) * 100)


f2030.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-26`))
f2030.hi.26x.wide <- pivot_wider(f2030.hi.26x, names_from = SN_C, values_from=n.2030) %>% rename(nd.2030 = `0`, d.2030 = `1`)

f2050.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2050-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2050=sum(`2050-hi-26`))
f2050.hi.26x.wide <- pivot_wider(f2050.hi.26x, names_from = SN_C, values_from=n.2050) %>%  rename(nd.2050 = `0`, d.2050 = `1`)

hi.26x.merged <- left_join(x = f2030.hi.26x.wide, y = f2050.hi.26x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2050 - nd.2030) / nd.2030)) * 100,
         d.per.change = (((d.2050  - d.2030) /   d.2030)) * 100)




#population by DAC status for 2050-hi-02
states.df2 <- states.df %>% mutate(state.name = recode(state.name,
                   "District_Of_Columbia" = "District of Columbia",
                   "New_Hampshire"        = "New Hampshire",
                   "New_Jersey"           = "New Jersey",
                   "New_York"             = "New York",
                   "North_Carolina"       = "North Carolina",
                   "South_Carolina"       = "South Carolina",
                   "Puerto_Rico"          = "Puerto Rico",
                   "Rhode_Island"         = "Rhode Island",
                   "Virgin Islands"       = "United States Virgin Islands"))
col <- "2050-in-02"
curr.df <- infra.sj %>%
           st_drop_geometry() %>%
           filter(!!sym(col) ==1) %>% select(c("GEOID10", !!col))
           print(paste(col, "|", nrow(curr.df),sep=""))

sn_c.labels <- c("0" = "not disadvantaged", "1" = "disadvantaged")
frequency1.df <- curr.df  %>% st_drop_geometry() %>% group_by(GEOID10) %>% summarize(n = n())
frequency1.cejst.df <- frequency1.df %>%
                       left_join(x  = .,
                                 y  = st_drop_geometry(cejst.sf),
                                 by = "GEOID10")  %>%
                       left_join(x = .,
                                 y = ct2010.ej.df,
                                 by = c("GEOID10" = "GEOID")) %>%
                       select(GEOID10, SF, n, SN_C, PER.WHITE, PER.BLACK, PER.NATIVE, PER.ASIAN, PER.HIPI, PER.HISPANIC,total) %>%
                       filter(!is.na(SF)) %>%
                       mutate(SN_C = as.factor(SN_C))
inundated.infra.by.state <- frequency1.cejst.df %>% group_by(SF) %>% summarise(inundated.infra = sum(n))

cejst.state.dac.summary <- frequency1.cejst.df %>%
                           group_by(SF, SN_C) %>%
                           summarize(inundated.freq = sum(n),
                                     tracts.freq = n(),
                                     PER.WHITE   = round(mean(PER.WHITE,    na.rm=T),2),
                                     PER.BLACK   = round(mean(PER.BLACK,    na.rm=T),2),
                                     PER.NATIVE  = round(mean(PER.NATIVE,   na.rm=T),2),
                                     PER.ASIAN   = round(mean(PER.ASIAN,    na.rm=T),2),
                                     PER.HIPI    = round(mean(PER.HIPI,     na.rm=T),2),
                                     PER.HISPANIC= round(mean(PER.HISPANIC, na.rm=T),2),
                                     total.pop   = sum(total)) %>%
                            mutate(SF = recode(SF,
                                               "District_Of_Columbia" = "District of Columbia",
                                               "New_Hampshire"        = "New Hampshire",
                                               "New_Jersey"           = "New Jersey",
                                               "New_York"             = "New York",
                                               "North_Carolina"       = "North Carolina",
                                               "South_Carolina"       = "South Carolina",
                                               "Puerto_Rico"          = "Puerto Rico",
                                               "Rhode_Island"         = "Rhode Island",
                                               "Virgin Islands"       = "United States Virgin Islands")) %>%
                            left_join(x  =.,
                                      y  = inundated.infra.by.state,
                                      by = "SF") %>%
                            mutate(per.inundated = round((inundated.freq/inundated.infra) *100,2)) %>%
                            mutate(SN_C = recode(SN_C,
                                                 "0" = "not disadvantaged",
                                                 "1" = "disadvantaged")) %>%
                            left_join(x  = .,
                                      y  = states.df2,
                                      by = c("SF" ="state.name")) %>%
                            select(SF, SN_C, total.pop, per.inundated,PER.BLACK,PER.HISPANIC,PER.WHITE,PER.ASIAN,PER.NATIVE,PER.HIPI,tracts.freq,inundated.freq,inundated.infra,state.abbreviation) %>%
                            rename(state = SF,
                                   `CJEST status` = SN_C,
                                   `Inundated assets (group)`   = inundated.freq,
                                   `Number of Census Tracts`    = tracts.freq,
                                   `Percent White (mean)`       = PER.WHITE,
                                   `Percent Black (mean)`       = PER.BLACK,
                                   `Percent Native (mean)`      = PER.NATIVE,
                                   `Percent Asian (mean)`       = PER.ASIAN,
                                   `Percent Hawaiian (mean)`    = PER.HIPI,
                                   `Percent Hispanic (mean)`    = PER.HISPANIC,
                                   `Inundated assets (state)`   = inundated.infra,
                                   `Percent inundated assets`   = per.inundated,
                                   `Population (group)`         = total.pop)
View(cejst.state.dac.summary)

cejst.state.dac.summary.long1 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Percent Hispanic (mean)`,  names_prefix = 'Percent Hispanic in ')
cejst.state.dac.summary.long2 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Percent inundated assets`, names_prefix = 'Percent inundated assets in ')
cejst.state.dac.summary.long3 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Inundated assets (group)`, names_prefix = 'Inundated assets in ')
cejst.state.dac.summary.long  <- left_join(x = cejst.state.dac.summary.long1,
                                          y = cejst.state.dac.summary.long2,
                                          by = "state") %>%
                                left_join(x = .,
                                          y = cejst.state.dac.summary.long3,
                                          by = "state")
# percent hispanic -----
p<- ggplot(cejst.state.dac.summary.long,
           aes(x    = `Percent Hispanic in not disadvantaged`,
               y    = `Percent Hispanic in disadvantaged`,
               size = `Percent inundated assets in disadvantaged`,
               color= state,
               text = paste("State: ", state,
                            "<br>Per. Hispanic (not DACs): ", `Percent Hispanic in not disadvantaged`,
                            "<br>Per. Hispanic (DACs): ", `Percent Hispanic in disadvantaged`,
                            "<br>Per. inundated assets in DACs: ", `Percent inundated assets in disadvantaged`,
                            "<br>Inundated assets (state): ", `Inundated assets in disadvantaged`))) +
  ylim(c(0,100)) +
  xlim(c(0,100)) +
  xlab("Mean Percent Hispanic in not-disadvantaged Census Tracts") +
  ylab("Mean Percent Hispanic in disadvantaged Census Tracts") +
  geom_hline(yintercept = 50, linetype="solid", color = "grey") +
  geom_vline(xintercept = 50, linetype="solid", color = "grey") +
  geom_point(alpha=0.7) +
  annotate(geom = "text", x = 75, y = 75, label = "I",   color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  annotate(geom = "text", x = 25, y = 75, label = "II",  color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  annotate(geom = "text", x = 25, y = 25, label = "III", color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  annotate(geom = "text", x = 75, y = 25, label = "IV",  color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  scale_size(range = c(1, 10),
             breaks = c(0,33,66,100),
             name="Percent inundated assets in disadvantaged",
             guide = guide_legend((title = "Percent inundated assets in disadvantaged"))) +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="right", plot.background = element_rect(fill = "white")) +
    labs(title ="Demographics and inundated infrastructure: Percent Hispanic (2050 High/2x)")

pp <- ggplotly(p,tooltip = "text")%>%
  layout(plot_bgcolor = "white")
pp

#percent hispanic -----
cejst.state.dac.summary.long1 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Percent Black (mean)`, names_prefix = 'Percent Black in ')
cejst.state.dac.summary.long2 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Percent inundated assets`, names_prefix = 'Percent inundated assets in ')
cejst.state.dac.summary.long3 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Inundated assets (group)`, names_prefix = 'Inundated assets in ')
cejst.state.dac.summary.long  <- left_join(x = cejst.state.dac.summary.long1,
                                           y = cejst.state.dac.summary.long2,
                                           by = "state") %>%
  left_join(x = .,
            y = cejst.state.dac.summary.long3,
            by = "state")
# Classic ggplot
p<- ggplot(cejst.state.dac.summary.long,
           aes(x    = `Percent Black in not disadvantaged`,
               y    = `Percent Black in disadvantaged`,
               size = `Percent inundated assets in disadvantaged`,
               color= state,
               text = paste("State: ", state,
                            "<br>Per. Black (not DACs): ", `Percent Black in not disadvantaged`,
                            "<br>Per. Black (DACs): ", `Percent Black in disadvantaged`,
                            "<br>Per. inundated assets in DACs: ", `Percent inundated assets in disadvantaged`,
                            "<br>Inundated assets (state): ", `Inundated assets in disadvantaged`))) +
  ylim(c(0,100)) +
  xlim(c(0,100)) +
  xlab("Mean Percent Black in not-disadvantaged Census Tracts") +
  ylab("Mean Percent Black in disadvantaged Census Tracts") +
  geom_hline(yintercept = 50, linetype="solid", color = "grey") +
  geom_vline(xintercept = 50, linetype="solid", color = "grey") +
  geom_point(alpha=0.7) +
  annotate(geom = "text", x = 75, y = 75, label = "I",   color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  annotate(geom = "text", x = 25, y = 75, label = "II",  color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  annotate(geom = "text", x = 25, y = 25, label = "III", color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  annotate(geom = "text", x = 75, y = 25, label = "IV",  color = "black", size = 5, vjust = -1, hjust = 1) + # Adding text annotation
  scale_size(range = c(1, 10),
             breaks = c(0,33,66,100),
             name="Percent inundated assets in disadvantaged",
             guide = guide_legend((title = "Percent inundated assets in disadvantaged"))) +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="right", plot.background = element_rect(fill = "white")) +
  labs(title ="Demographics and inundated infrastructure: Percent Black (2050 High/2x)")

pp <- ggplotly(p,tooltip = "text")%>%
  layout(plot_bgcolor = "white")
pp





#-------
#cejst.state.dac.summary %>% write.csv(., "/home/rama/coastal_deadline/data/tmp/cejst_state_summary_2050-hi-02.csv",row.names = F)
#sum(cejst.state.dac.summary$inundated.freq)
ggplot(cejst.state.dac.summary, aes(x = `Percent inundated assets`, y = `Percent Black (mean)` )) +
   geom_point() +
   geom_text(aes(label = state.abbreviation,vjust = -0.5, hjust = 0.5)) +
   scale_color_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                    labels = sn_c.labels) +
  labs(x = "Percent of all inundated infrastructure in Disadvantaged Census Tracts", y = "Mean Percent Black in Disadvantaged Census Tracts")

ggplot(abbreviation, aes(x = per.inundated, y = PER.HISPANIC)) +
  geom_point() +
  geom_text(aes(label = state.abbreviation,vjust = -0.5, hjust = 0.5)) +
  scale_color_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                     labels = sn_c.labels) +
  labs(x = "Percent of all inundated infrastructure in Disadvantaged Census Tracts", y = "Mean Percent Hispanic in Disadvantaged Census Tracts")

ggplot(cejst.state.dac.summary, aes(x = per.inundated, y = PER.NATIVE)) +
  geom_point() +
  geom_text(aes(label = state.abbreviation,vjust = -0.5, hjust = 0.5)) +
  scale_color_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                     labels = sn_c.labels) +
  labs(x = "Percent of all inundated infrastructure in Disadvantaged Census Tracts", y = "Mean Percent Native in Disadvantaged Census Tracts")

ggplot(cejst.state.dac.summary, aes(x = per.inundated, y = PER.ASIAN)) +
  geom_point() +
  geom_text(aes(label = state.abbreviation,vjust = -0.5, hjust = 0.5)) +
  scale_color_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                     labels = sn_c.labels) +
  labs(x = "Percent of all inundated infrastructure in Disadvantaged Census Tracts", y = "Mean Percent Asian in Disadvantaged Census Tracts")

ggplot(cejst.state.dac.summary, aes(x = per.inundated, y = PER.WHITE)) +
  geom_point() +
  geom_text(aes(label = state.abbreviation,vjust = -0.5, hjust = 0.5)) +
  scale_color_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
                     labels = sn_c.labels) +
  labs(x = "Percent of all inundated infrastructure in Disadvantaged Census Tracts", y = "Mean Percent White in Disadvantaged Census Tracts")



#
#     labs(x = "State", y = "Number of inundated assets", fill = "Disadvantaged status", subtitle = col) +
#     theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))  +
#     scale_fill_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
#                       labels = sn_c.labels) +
#     ggtitle("Number of inundated assets by disadvantaged status")
#   f <- paste(output.dir,"/inundated_infrastructure_frequency_by_state/inundated_infrastructure_frequency_by_state_", col, ".pdf", sep="")
#   ggsave(f)
#
#   #save data for Cynthia
#   agg_data %>% rename(state = SF,disadvantaged = SN_C, frequency = freq) %>% write.csv(., paste("/home/rama/coastal_deadline/data/data_for_cynthia", "/frequency_of_inundated_infra_by_state_by_disadvantaged_status_", col,".csv", sep=""), row.names = FALSE)
#





#frequency of inundated infra by type by CJEST status ----
for(col in c("2050-in-02"))
{
  curr.df       <- infra.sj %>%
    st_drop_geometry() %>%
    filter(!!sym(col) ==1) %>% select(c("GEOID10", type, !!col))
  print(paste(col, "|", nrow(curr.df),sep=""))

  sn_c.labels <- c("0" = "not disadvantaged", "1" = "disadvantaged")
  frequency1.df <- curr.df  %>% st_drop_geometry() %>% group_by(GEOID10, type) %>% summarize(n = n())
  frequency1.cejst.df <- frequency1.df %>%
    left_join(x  = .,
              y  = st_drop_geometry(cejst.sf),
              by = "GEOID10")  %>%
    select(GEOID10, type, n, SN_C) %>%
    filter(!is.na(type)) %>%
    mutate(SN_C = as.factor(SN_C))

  # Aggregate data by state and SN_C, and count frequency of n
  agg_data <- frequency1.cejst.df %>%
    group_by(type, SN_C) %>%
    summarise(freq = sum(n)) %>%
    ungroup() %>%
    arrange(type, SN_C)  # Arrange data for better plotting order
agg_data.wide <- agg_data %>%  tidyr::pivot_wider(id_cols = type, names_from = SN_C, values_from = freq) %>%
                mutate(per.DAC = `1` /( `0`+ `1`))
#105+61 0
#367+64 1
#PH and AFF 2050-in-02
nDAC <- 105+61
DAC  <- 367+64
DAC / (nDAC + DAC)

  # Plot stacked bar chart
  # ggplot(agg_data, aes(x = as.factor(SF), y = freq, fill = as.factor(SN_C))) +
  #   #ylim(y.lim) +
  #   geom_bar(stat = "identity") +
  #   labs(x = "State", y = "Number of inundated assets", fill = "Disadvantaged status", subtitle = col) +
  #   theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))  +
  #   scale_fill_manual(values = c("0" = "#009fdf", "1" = "#d7117d"),
  #                     labels = sn_c.labels) +
  #   ggtitle("Number of inundated assets by disadvantaged status")
  # f <- paste(output.dir,"/inundated_infrastructure_frequency_by_state/inundated_infrastructure_frequency_by_state_", col, ".pdf", sep="")
  # ggsave(f)
  #
  # #save data for Cynthia
  # agg_data %>% rename(state = SF,disadvantaged = SN_C, frequency = freq) %>% write.csv(., paste("/home/rama/coastal_deadline/data/data_for_cynthia", "/frequency_of_inundated_infra_by_state_by_disadvantaged_status_", col,".csv", sep=""), row.names = FALSE)
}

#2020 and 2030 by CEJST status
f2020.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2020-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2020=sum(`2020-hi-02`))
f2020.hi.2x.wide <- pivot_wider(f2020.hi.2x, names_from = SN_C, values_from=n.2020) %>% rename(nd.2020 = `0`, d.2020 = `1`)

f2030.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-02`))
f2030.hi.2x.wide <- pivot_wider(f2030.hi.2x, names_from = SN_C, values_from=n.2030) %>%  rename(nd.2030 = `0`, d.2030 = `1`)

hi.2x.merged <- left_join(x = f2020.hi.2x.wide, y = f2030.hi.2x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2030 - nd.2020) / nd.2020)) * 100,
         d.per.change = (((d.2030  - d.2020) /   d.2020)) * 100)


f2020.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2020-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2020=sum(`2020-hi-26`))
f2020.hi.26x.wide <- pivot_wider(f2020.hi.26x, names_from = SN_C, values_from=n.2020) %>% rename(nd.2020 = `0`, d.2020 = `1`)

f2030.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-26`))
f2030.hi.26x.wide <- pivot_wider(f2030.hi.26x, names_from = SN_C, values_from=n.2030) %>%  rename(nd.2030 = `0`, d.2030 = `1`)

hi.26x.merged <- left_join(x = f2020.hi.26x.wide, y = f2030.hi.26x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2030 - nd.2020) / nd.2020)) * 100,
         d.per.change = (((d.2030  - d.2020) /   d.2020)) * 100)





#2030 and 2050 by CEJST status
f2030.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-02`))
f2030.hi.2x.wide <- pivot_wider(f2030.hi.2x, names_from = SN_C, values_from=n.2030) %>% rename(nd.2030 = `0`, d.2030 = `1`)

f2050.hi.2x <- infra.sj %>% st_drop_geometry() %>% filter(`2050-hi-02` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2050=sum(`2050-hi-02`))
f2050.hi.2x.wide <- pivot_wider(f2050.hi.2x, names_from = SN_C, values_from=n.2050) %>%  rename(nd.2050 = `0`, d.2050 = `1`)

hi.2x.merged <- left_join(x = f2030.hi.2x.wide, y = f2050.hi.2x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2050 - nd.2030) / nd.2030)) * 100,
         d.per.change = (((d.2050  - d.2030) /   d.2030)) * 100)


f2030.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2030-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2030=sum(`2030-hi-26`))
f2030.hi.26x.wide <- pivot_wider(f2030.hi.26x, names_from = SN_C, values_from=n.2030) %>% rename(nd.2030 = `0`, d.2030 = `1`)

f2050.hi.26x <- infra.sj %>% st_drop_geometry() %>% filter(`2050-hi-26` ==1, !is.na(SN_C)) %>% group_by(type, SN_C) %>% summarize(n.2050=sum(`2050-hi-26`))
f2050.hi.26x.wide <- pivot_wider(f2050.hi.26x, names_from = SN_C, values_from=n.2050) %>%  rename(nd.2050 = `0`, d.2050 = `1`)

hi.26x.merged <- left_join(x = f2030.hi.26x.wide, y = f2050.hi.26x.wide, by="type") %>%
  mutate(nd.per.change = (((nd.2050 - nd.2030) / nd.2030)) * 100,
         d.per.change = (((d.2050  - d.2030) /   d.2030)) * 100)




#population by DAC status for 2050-hi-02
states.df2 <- states.df %>% mutate(state.name = recode(state.name,
                                                       "District_Of_Columbia" = "District of Columbia",
                                                       "New_Hampshire"        = "New Hampshire",
                                                       "New_Jersey"           = "New Jersey",
                                                       "New_York"             = "New York",
                                                       "North_Carolina"       = "North Carolina",
                                                       "South_Carolina"       = "South Carolina",
                                                       "Puerto_Rico"          = "Puerto Rico",
                                                       "Rhode_Island"         = "Rhode Island",
                                                       "Virgin Islands"       = "United States Virgin Islands"))
col <- "2050-in-02"
curr.df <- infra.sj %>%
  st_drop_geometry() %>%
  filter(!!sym(col) ==1) %>% select(c("GEOID10", !!col))
print(paste(col, "|", nrow(curr.df),sep=""))

sn_c.labels <- c("0" = "not disadvantaged", "1" = "disadvantaged")
frequency1.df <- curr.df  %>% st_drop_geometry() %>% group_by(GEOID10) %>% summarize(n = n())
frequency1.cejst.df <- frequency1.df %>%
  left_join(x  = .,
            y  = st_drop_geometry(cejst.sf),
            by = "GEOID10")  %>%
  left_join(x = .,
            y = ct2010.ej.df,
            by = c("GEOID10" = "GEOID")) %>%
  select(GEOID10, SF, n, SN_C, PER.WHITE, PER.BLACK, PER.NATIVE, PER.ASIAN, PER.HIPI, PER.HISPANIC,total) %>%
  filter(!is.na(SF)) %>%
  mutate(SN_C = as.factor(SN_C))
inundated.infra.by.state <- frequency1.cejst.df %>% group_by(SF) %>% summarise(inundated.infra = sum(n))

cejst.state.dac.summary <- frequency1.cejst.df %>%
  group_by(SF, SN_C) %>%
  summarize(inundated.freq = sum(n),
            tracts.freq = n(),
            PER.WHITE   = round(mean(PER.WHITE,    na.rm=T),2),
            PER.BLACK   = round(mean(PER.BLACK,    na.rm=T),2),
            PER.NATIVE  = round(mean(PER.NATIVE,   na.rm=T),2),
            PER.ASIAN   = round(mean(PER.ASIAN,    na.rm=T),2),
            PER.HIPI    = round(mean(PER.HIPI,     na.rm=T),2),
            PER.HISPANIC= round(mean(PER.HISPANIC, na.rm=T),2),
            total.pop   = sum(total)) %>%
  mutate(SF = recode(SF,
                     "District_Of_Columbia" = "District of Columbia",
                     "New_Hampshire"        = "New Hampshire",
                     "New_Jersey"           = "New Jersey",
                     "New_York"             = "New York",
                     "North_Carolina"       = "North Carolina",
                     "South_Carolina"       = "South Carolina",
                     "Puerto_Rico"          = "Puerto Rico",
                     "Rhode_Island"         = "Rhode Island",
                     "Virgin Islands"       = "United States Virgin Islands")) %>%
  left_join(x  =.,
            y  = inundated.infra.by.state,
            by = "SF") %>%
  mutate(per.inundated = round((inundated.freq/inundated.infra) *100,2)) %>%
  mutate(SN_C = recode(SN_C,
                       "0" = "not disadvantaged",
                       "1" = "disadvantaged")) %>%
  left_join(x  = .,
            y  = states.df2,
            by = c("SF" ="state.name")) %>%
  select(SF, SN_C, total.pop, per.inundated,PER.BLACK,PER.HISPANIC,PER.WHITE,PER.ASIAN,PER.NATIVE,PER.HIPI,tracts.freq,inundated.freq,inundated.infra,state.abbreviation) %>%
  rename(state = SF,
         `CJEST status` = SN_C,
         `Inundated assets (group)`   = inundated.freq,
         `Number of Census Tracts`    = tracts.freq,
         `Percent White (mean)`       = PER.WHITE,
         `Percent Black (mean)`       = PER.BLACK,
         `Percent Native (mean)`      = PER.NATIVE,
         `Percent Asian (mean)`       = PER.ASIAN,
         `Percent Hawaiian (mean)`    = PER.HIPI,
         `Percent Hispanic (mean)`    = PER.HISPANIC,
         `Inundated assets (state)`   = inundated.infra,
         `Percent inundated assets`   = per.inundated,
         `Population (group)`         = total.pop)
View(cejst.state.dac.summary)

cejst.state.dac.summary.long1 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Percent Hispanic (mean)`,  names_prefix = 'Percent Hispanic in ')
cejst.state.dac.summary.long2 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Percent inundated assets`, names_prefix = 'Percent inundated assets in ')
cejst.state.dac.summary.long3 <- pivot_wider(cejst.state.dac.summary, id_cols = state, names_from = `CJEST status`, values_from = `Inundated assets (group)`, names_prefix = 'Inundated assets in ')
cejst.state.dac.summary.long  <- left_join(x = cejst.state.dac.summary.long1,
                                           y = cejst.state.dac.summary.long2,
                                           by = "state") %>%
  left_join(x = .,
            y = cejst.state.dac.summary.long3,
            by = "state")
