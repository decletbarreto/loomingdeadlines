source("~/coastal_deadline/scripts/configuration.R")

nukes <- openxlsx::read.xlsx("/home/rama/coastal_deadline/data/energy_infrastructure/december_generator2023.xlsx")  %>%
          filter(X16 == "Nuclear") %>% select(X3) %>% distinct(X3) %>% pull()
inundated.infra.df <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep="")) %>%
                      st_drop_geometry() %>%
                      filter(type == "Power Plant") %>%
                      mutate(plant.id = as.character(substr(id,4,length(id)))) %>%
                      write.csv(., paste(output.dir, "/coastal_deadline_inundated_power_plants.csv",sep=""), row.names = F)


                      # select(id, name, plant.id)
table(inundated.infra.df)
#View( openxlsx::read.xlsx("/home/rama/coastal_deadline/data/energy_infrastructure/december_generator2023.xlsx"))

