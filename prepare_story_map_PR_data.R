source("~/coastal_deadline/scripts/configuration.R")
library(ggplot2)
pr.sf <- read_sf("/media/rama/datastore3/Census/borinquen/borinquen.gdb", layer="CT2018_PR")
pr.combine.sf <- pr.sf %>% st_union(., by_feature = FALSE)
write_sf(pr.combine.sf, dsn="/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/PR_outline", driver="ESRI Shapefile")
pr2.sf <- vect("/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/PR_outline")

files <- c("/home/rama/coastal_deadline/data/inundation_layers/2020-high-02/is_flooded_sea-2020-high-02-PRVI.tif",
           "/home/rama/coastal_deadline/data/inundation_layers/2030-high-02/is_flooded_sea-2030-high-02-PRVI.tif",
           "/home/rama/coastal_deadline/data/inundation_layers/2050-high-02/is_flooded_sea-2050-high-02-PRVI.tif",
           "/home/rama/coastal_deadline/data/inundation_layers/2100-high-02/is_flooded_sea-2100-high-02-PRVI.tif")
counter <- 1
vals <- c(2020,2030,2050,2100)

#crop to PR perimeter
for(f in files)
{
  r.raster <- rast(f)
  pr2.sf <- pr2.sf %>% project(., r.raster)

  m <- c(0,1,vals[counter])
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  r.raster.recode <- classify(r.raster, rclmat,include.lowest=TRUE)
  r.filename <- paste("/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/crop_",basename(f),sep="")

  f.crop   <- crop(r.raster.recode, pr2.sf, mask=TRUE) %>% writeRaster(., r.filename, overwrite=TRUE)

  counter <- counter + 1
}

files2 <- c("/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/crop_is_flooded_sea-2020-high-02-PRVI.tif",
            "/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/crop_is_flooded_sea-2030-high-02-PRVI.tif",
            "/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/crop_is_flooded_sea-2050-high-02-PRVI.tif",
            "/home/rama/coastal_deadline/data/output/agol/PR_StoryMap/crop_is_flooded_sea-2100-high-02-PRVI.tif")

counter <- 1
all.merged.r <- list()
#merge crops
for(f in files2)
{
  print(f)
  r.raster <- rast(f)
  m <- c(0,1,vals[counter])
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  r.raster.recode <- classify(r.raster, rclmat,include.lowest=TRUE)
    #all.merged.r <- terra::merge(all.merged.r,r.raster.recode, first = F)
  v <- vect(r.raster.recode)
  writeVector(v, paste(output.dir,"/agol/PR_StoryMap/",basename(f),sep=""), filetype = "GPKG", overwrite=TRUE)



  counter <- counter + 1
}


#2030 and 2050 by recoded infra
inundated.infra.pr.2030.hi.02.df <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep="")) %>%
                         st_drop_geometry() %>%
                         filter(state        == "PR" &
                                `2030-hi-02` == 1) %>%
                         select(type,city,name) %>%
                        mutate(year = 2030)
inundated.infra.pr.2050.hi.02.df <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep="")) %>%
                        st_drop_geometry() %>%
                        filter(state        == "PR" &
                                 `2050-hi-02` == 1) %>%
                        select(type,city,name) %>%
                        mutate(year = 2050)
inundated.infra.pr.2100.hi.02.df <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep="")) %>%
                                    st_drop_geometry() %>%
                                    filter(state        == "PR" &
                                             `2100-hi-02` == 1) %>%
                                    select(type,city,name) %>%
                                    mutate(year = 2100)

inundated.infra.pr.hi.02.df      <- rbind(inundated.infra.pr.2030.hi.02.df, inundated.infra.pr.2050.hi.02.df,inundated.infra.pr.2100.hi.02.df)
inundated.infra.pr.hi.02.summary <- inundated.infra.pr.hi.02.df %>% group_by(type,city,year) %>%
                                    summarise(freqq = n()) %>%
                                    mutate(year = factor(year, levels = c("2030", "2050", "2100"))) %>%
                                    mutate(city = if_else(city %in% c("Cata","Catano"), "Cataño",city)) %>%
                                    mutate(city = if_else(city == "Loiza", "Loíza",city)) %>%
                                    mutate(city = if_else(city == "Mayaguez", "Mayagüez",city))

inundated.infra.pr.hi.02.summary.wide <- pivot_wider(data = inundated.infra.pr.hi.02.summary, id_cols = type,names_from = year, values_from = freqq)

custom_order <- c("Public Housing", "K-12 School", "Brownfields", "Electrical Substation", "Wastewater Treatment Plant",
                  "Law Enforcement", "Post Office", "Toxics Release Inventory Site",
                  "College, University, Technical or Trade School", "City Hall", "Power Plant", "Fire Station")
inundated.infra.pr.hi.02.summary$type <- factor(inundated.infra.pr.hi.02.summary$type, levels = custom_order)

ggplot(inundated.infra.pr.hi.02.summary, aes(x = freqq, y = reorder(type, -freqq), fill = year)) +
  #ggplot(inundated.infra.pr.hi.02.summary, aes(x = freq, y = reorder(type, match(type, subset_2100$type)), fill = year)) +
  #xlim(c(0,400)) +
  geom_bar(stat = "identity", position="stack") +
  labs(x = "Number of inundated assets", y = " ", fill = "", subtitle = "PR, High scenario, twice a year") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        panel.grid  = element_blank(),
        plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("2030" = "#009fdf", "2050" = "#d7117d", "2100" = "red"),
                    labels = c("2030" = "2030", "2050" = "2050", "2100" = "2100")) +  # Adjust legend labels


  #                   labels = sn_c.labels) +
  ggtitle("Number of inundated assets by disadvantaged status")

#cual city hall?
inundated.infra.pr.hi.02.summary %>% filter(type == "City Hall")

#vivienda pública solamente ----
# Subset data for the year 2100
ph.filtered <- inundated.infra.pr.hi.02.summary %>% filter(type == "Public Housing")
ph.filtered_2100 <- ph.filtered %>%
  filter(year == 2100) %>%
  arrange(desc(freq))
ph.filtered$year <- factor(ph.filtered$year, levels = c("2100", "2050", "2030"))
city.order1 <- c("Cataño", "San Juan", "Vieques","Mayagüez","Guaynabo","Carolina", "Naguabo", "Luquillo","Aguadilla","Loíza")
city.order2 <- (seq(1, length(city.order1)))
city.order.df <- as.data.frame(cbind(city=city.order1, city.order = city.order2))
ph.filtered <- inundated.infra.pr.hi.02.summary %>%
  filter(type=="Public Housing") %>%
  left_join(x = .,
            y = city.order.df,
            by = "city")
ph.filtered$year <- factor(ph.filtered$year, levels = unique(ph.filtered$year))
ph.filtered$city <- factor(ph.filtered$city, levels = city.order1)
label.eng <- "Number of inundated public housing buildings"
label.spa <- "Número de edificios de vivienda pública bajo inundación"
#ggplot(ph.filtered, aes(x = freq, y = reorder(city, match(city, ph.filtered_2100$city)), fill = year)) +
  ggplot(ph.filtered, aes(x = freq, y = reorder(city, city.order), fill = year)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = label.spa, y = " ", fill = "", subtitle = "High scenario, twice a year") +
  theme_minimal() +
  theme(axis.text.x = element_text(family = "Gotham Medium", size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(family = "Gotham Medium", size = 10),
        panel.grid  = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.title.x    = element_text(family = "Gotham Medium", size = 10),
        legend.text     = element_text(family = "Gotham Medium", size = 10),
        plot.subtitle   = element_text(family = "Gotham Medium", size = 10)) +
  scale_fill_manual(values = c("2030" = "#e7e7e9", "2050" = "#a8a9ad", "2100" = "#58585a"),
                    labels = c("2030" = "2030", "2050" = "2050", "2100" = "2100"))

