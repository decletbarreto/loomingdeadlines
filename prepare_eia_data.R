source("~/coastal_deadline/scripts/configuration.R")
library(readxl)
library(modeest)

cols <- c("Entity.ID","Entity.Name","Plant.ID","Plant.Name","Google.Map","Bing.Map","Plant.State", "County", "Balancing.Authority.Code" ,
          "Sector" ,"Generator.ID","Unit.Code", "Nameplate.Capacity.MW","Net.Summer.Capacity.MW" ,"Net.Winter.CapacityMW" ,"Technology",
          "Energy.Source.Code","Prime.Mover.Code","Operating.Month","Operating.Year","Planned.Retirement.Month","Planned.Retirement.Year",
          "Status","Nameplate.Energy.Capacity.MWh","DC.Net.Capacity.MW", "Planned.Derate.Year","Planned.Derate.Month", "Planned.Derate.of Summer.Capacity.MW","Planned.Uprate.Year","Planned.Uprate.Month",	"Planned.Uprate.of.Summer.Capacity.MW",	"Latitude","Longitude")

#NOTES: Capacity from facilities with a total generator nameplate capacity less than 1 MW are excluded from this report.
#This exclusion may represent a significant portion of capacity for some technologies such as solar photovoltaic generation.
#Links to Google Map and Bing Map were set up to connect to these sites at the time of publication of this inventory.
#The links may not properly function if these sites update their protocol for accessing maps.  Sources: U.S. Energy Information
#Administration, Form EIA-860, 'Annual Electric Generator Report' and Form EIA-860M, 'Monthly Update to the Annual Electric Generator Report.'

#old method with Form 860 data which does not have data for PR, GU, AS, VI
# generators.sf <-read.csv(paste(data.dir,"/energy_infrastructure/EIA/Generator_Y2022.csv", sep="")) %>%
#   mutate(Utility.ID = as.integer(Utility.ID))
# aggregated.generators.sf <- generators.sf %>%
#   group_by(Plant.Code) %>%
#   summarize(Technology = find_mode(Technology))

#https://www.eia.gov/electricity/data/eia860m/
generators.us.sf <-read_xlsx(paste(data.dir,"/energy_infrastructure/december_generator2023.xlsx", sep=""),
                                   sheet ="Operating", col_names = cols, skip=3)
generators.pr.sf <-read_xlsx(paste(data.dir,"/energy_infrastructure/december_generator2023.xlsx", sep=""),
                             sheet ="Operating_PR", col_names = cols, skip=3)

nrow(generators.us.sf) #25887
nrow(generators.pr.sf) #214

plants.us.sf <- generators.us.sf %>%
                mutate(Plant.ID = as.integer(Plant.ID)) %>%
                group_by(Plant.ID) %>%
                #filter(Sector %in% energy.sectors.list) %>%
                filter(Nameplate.Capacity.MW == max(Nameplate.Capacity.MW)) %>% #units in plants with the same capacity will be duplicated
                distinct(Plant.ID, Plant.Name, Nameplate.Capacity.MW, .keep_all = TRUE) %>%  #eliminate duplicates
                select(Plant.ID, Plant.Name, Nameplate.Capacity.MW,Plant.State, Sector, Latitude,Longitude) %>%
                arrange(Plant.State, Plant.Name)

plants.pr.sf <- generators.pr.sf %>%
                mutate(Plant.ID = as.integer(Plant.ID)) %>%
                group_by(Plant.ID) %>%
                #filter(Sector %in% energy.sectors.list) %>%
                filter(Nameplate.Capacity.MW == max(Nameplate.Capacity.MW)) %>% #units in plants with the same capacity will be duplicated
                distinct(Plant.ID, Plant.Name, Nameplate.Capacity.MW, .keep_all = TRUE) %>%  #eliminate duplicates
                select(Plant.ID, Plant.Name, Nameplate.Capacity.MW,Plant.State, Sector,Latitude,Longitude) %>%
                arrange(Plant.State, Plant.Name)

plant.data.2022.cols <- c ("Utility.ID", "Utility.Name","Plant.Code","Plant.Name", "Street.Address", "City", "State","Zip", "County",
                           "Latitude", "Longitude",	"NERC.Region",	"Balancing.Authority.Code",	"Balancing.Authority.Name", "Name.of.Water.Source",
                           "Primary.Purpose.NAICS.Code",	"Regulatory.Status","Sector",	"Sector.Name", "FERC.Cogeneration.Status",
                           "FERC.Cogeneration.Docket.Number",	"FERC.Small.Power.Producer.Status",	"FERC.Small.Power.Producer.Docket.Number",
                           "FERC.Exempt.Wholesale.Generator.Status",	"FERC.Exempt.Wholesale.Generator.Docket.Number",	"Ash.Impoundment",
                           "Ash.Impoundment.Lined", "Ash.Impoundment.Status",	"Transmission.or.Distribution.System.Owner",
                           "Transmission.or.Distribution.System.Owner.ID",	"Transmission.or.Distribution.System.Owner.State",
                           "Grid Voltage.kV","Grid Voltage.2.kV","Grid.Voltage.3.kV","Energy.Storage","Natural.Gas.LDC.Name",
                           "Natural.Gas.Pipeline.Name.1",	"Natural.Gas.Pipeline.Name.2","Natural.Gas.Pipeline.Name.3",	"Pipeline.Notes",
                           "Natural.Gas.Storage","Liquefied.Natural.Gas.Storage")

plant.data.2022.df <- read_xlsx(paste(data.dir, "/energy_infrastructure/EIA_2022/2___Plant_Y2022.xlsx",sep=""),
                                sheet     = "Plant",
                                skip      = 2,
                                col_names = plant.data.2022.cols) %>%
                      filter(Sector.Name %in% energy.sectors.list) %>%
                      select(Plant.Code, Street.Address, City, State, Zip,Sector.Name)

#create sf object and write
plants.final.sf <- rbind(plants.us.sf, plants.pr.sf) %>%
                   filter(!is.na(Latitude))  %>%
                   filter(!is.na(Longitude)) %>%
                   filter(!(Latitude ==""))  %>%
                   filter(!(Longitude =="")) %>%
                   st_as_sf(x      = .,
                            coords = c("Longitude", "Latitude"),
                            crs    = 4326) %>%
                   sp::merge(x    =  .,
                             y    =  plant.data.2022.df,
                             by.x = "Plant.ID",
                             by.y = "Plant.Code",
                             all.x= T) %>% # left join to keep all plants since there are no data for PR in plant.data.2022.df
                   write_sf(obj = ., dsn=paste(data.dir,"/energy_infrastructure/power_plants.gpkg",sep=""), driver="GPKG", delete_layer = TRUE)

nrow(plants.us.sf) # unique plants with nameplate capacity assigned base on max capacity of generating units within plant in energy sector
nrow(plants.pr.sf) # unique plants with nameplate capacity assigned base on max capacity of generating units within plant in energy sector


