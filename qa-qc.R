library(sf)
library(openxlsx)
library(dplyr)
library(tidyr)

ci.sf <- read_sf("/home/rama/coastal_deadline/data/output/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp")
npl1.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-02-EAST_npl_inundated.gpkg") %>% sf::st_transform(4326)
npl2.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-02-WEST_npl_inundated.gpkg") %>% sf::st_transform(4326)
npl3.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-02-PRVI_npl_inundated.gpkg") %>% sf::st_transform(4326)
npl4.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-02-GU_npl_inundated.gpkg")   %>% sf::st_transform(4326)
npl.sf <- rbind(npl1.sf, npl2.sf, npl3.sf, npl4.sf)
npl.state <- table(npl.sf$STATE_CODE)

n1 <- ci.sf %>% filter(`2050-in-02`==1)
table(n1$state)

n1 <- ci.sf %>% filter(`2050-in-12`==1)
table(n1$state)
npl1.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-12-EAST_npl_inundated.gpkg") %>% sf::st_transform(4326)
npl2.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-12-WEST_npl_inundated.gpkg") %>% sf::st_transform(4326)
npl3.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-12-PRVI_npl_inundated.gpkg") %>% sf::st_transform(4326)
npl4.sf <- read_sf("/home/rama/coastal_deadline/data/output/npl_inundated/is_flooded_sea-2050-int-12-GU_npl_inundated.gpkg")   %>% sf::st_transform(4326)
npl.sf <- rbind(npl1.sf, npl2.sf, npl3.sf, npl4.sf)
npl.state <- table(npl.sf$STATE_CODE)


#second verification of all CI
ci2.df <- openxlsx::read.xlsx("/home/rama/coastal_deadline/data/output/county_subdivision_summaries_merged/2020-high-02_county_subdivision_summary.xlsx") %>%
          select(contains("(T)")) %>%
          select(-"All.Infrastructure.(T)")
colnames(ci2.df)
ci3.df <- ci2.df %>%  pivot_longer(cols = everything(),
                                            names_to = "variable",
                                            values_to = "value") %>%
                      group_by(variable) %>%
                      summarise(n=sum(value))
View(ci3.df)
