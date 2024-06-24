source("~/coastal_deadline/scripts/configuration.R")
library(fmsb)
library(openxlsx)
library(tidyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(logr)


cores <- 12

coastal.counties.df.for.join <- coastal.counties.sf %>% st_drop_geometry() %>% select(countyfips, countyname)
cs.df.for.join <- st_drop_geometry(cs.sf) %>%
  select(GEOID,NAMELSAD) %>%
  mutate(COUNTY.GEOID = substr(GEOID, 1,5)) %>%
  left_join(x = .,
            y = coastal.counties.df.for.join,
            by = c("COUNTY.GEOID" ="countyfips"))

svi.conus.df <- read_sf("/media/rama/datastore3/CDC/SVI2020_US_county.gdb", layer = "SVI2020_US_county")  %>%
                st_drop_geometry() %>%
                select(all_of(c("FIPS","RPL_THEMES","RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", "E_TOTPOP")))
svi.pr.df    <- read_sf("/media/rama/datastore3/CDC/SVI_PuertoRico", layer = "SVI2020_PUERTORICO_county") %>%
                st_drop_geometry() %>%
                select(all_of(c("FIPS","RPL_THEMES","RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4")))
svi.vi.df    <- read_sf("/media/rama/datastore3/USVI SVI", layer = "Overall Vulnerability")  %>% st_drop_geometry()

svi.df <- svi.conus.df #join all three later when schema is unified

round.factor <- 2
#summaries.list <- list.files(path = paste(output.dir,"/inundation_summaries", sep= ""), pattern = "*.gpkg$", full.names = TRUE)
inundated.columns <- c("AFF.i","AMB.i","BRO.i", "COL.i","COR.i", "DPW.i", "FIR.i", "HOS.i","K12.i", "LAW.i", "PH.i","POS.i", "POW.i","SUB.i", "TRI.i", "VA.i","WT.i", "HQ.i", "PRI.i","RNGR.i", "SSC.i","SCAP.i","GOV.i", "SUP.i")
totals.columns    <- c("AFF.t","AMB.t","BRO.t", "COL.t","COR.t", "DPW.t", "FIR.t", "HOS.t","K12.t", "LAW.t", "PH.t","POS.t", "POW.t","SUB.t", "TRI.t", "VA.t","WT.t", "HQ.t", "PRI.t","RNGR.t", "SSC.t","SCAP.t","GOV.t", "SUP.t")
year.projection.frequency.list <- c("2020-in-02", "2020-in-12", "2020-in-26", "2020-int-02", "2020-int-12", "2020-int-26", "2020-high-02", "2020-high-12", "2020-high-26",
                                    "2030-in-02", "2030-in-12", "2030-in-26", "2030-int-02", "2030-int-12", "2030-int-26", "2030-high-02", "2030-high-12", "2030-high-26",
                                    "2050-in-02", "2050-in-12", "2050-in-26", "2050-int-02", "2050-int-12", "2050-int-26", "2050-high-02", "2050-high-12", "2050-high-26",
                                    "2100-in-02", "2100-in-12", "2100-in-26", "2100-int-02", "2100-int-12", "2100-int-26", "2100-high-02", "2100-high-12", "2100-high-26")

year.projection.frequency.list2 <- c("2020-il-02", "2020-il-12", "2020-il-26", "2020-in-02", "2020-in-12", "2020-in-26", "2020-hi-02", "2020-hi-12", "2020-hi-26",
                                     "2030-il-02", "2030-il-12", "2030-il-26", "2030-in-02", "2030-in-12", "2030-in-26", "2030-hi-02", "2030-hi-12", "2030-hi-26",
                                     "2050-il-02", "2050-il-12", "2050-il-26", "2050-in-02", "2050-in-12", "2050-in-26", "2050-hi-02", "2050-hi-12", "2050-hi-26",
                                     "2100-il-02", "2100-il-12", "2100-il-26", "2100-in-02", "2100-in-12", "2100-in-26", "2100-hi-02", "2100-hi-12", "2100-hi-26")

year.projection.list<- c("2020-high", "2020-int_low", "2020-int",
                         "2030-high", "2030-int_low", "2030-int",
                         "2050-high", "2050-int_low", "2050-int",
                         "2100-high", "2100-int_low", "2100-int")

coastal.counties.df <- coastal.counties.sf %>% select(countyfips, countyname)

#county subdivision-level summary by scenario -----
#View(all.scenarios.summaries)
#bind all summary files together
all.total.number.of.structures.at.risk <- list()
for(year.projection in year.projection.frequency.list)
{
  files <- list.files(paste(output.dir,"/inundation_summaries", sep=""), pattern="\\.gpkg$", full.names = TRUE)
  files <- grep(year.projection, files, value = TRUE)
  #files <- grep("GU", files, value=TRUE, invert = T)
  #files <- grep("PR", files, value=TRUE, invert = T)
  all.df <- list()
  #file <- files[3]
  for(file in files)
  {
    print(file)
    curr.df <- read_sf(file) %>% st_drop_geometry()
    all.df  <- rbind(all.df, curr.df)
  }
  coastal.counties.for.join.sf <- coastal.counties.sf %>%
                                  select(countyfips,countyname)
  all.df <- all.df %>%
            rename(cs.geoid = GEOID) %>%
            mutate(county.geoid = substr(cs.geoid,1,5)) %>%
            left_join(x = .,
                      y = st_drop_geometry(coastal.counties.for.join.sf),
                      by = c("county.geoid" = "countyfips")
                      ) %>%
            select("cs.geoid", "cs.name", "state", "countyname", everything())

  #https://ucsusa-my.sharepoint.com/:w:/r/personal/kdahl_ucsusa_org/_layouts/15/Doc.aspx?sourcedoc=%7BEFAD253A-596F-4009-8AFD-111498B0CCBE%7D&file=data_party_goals.docx&fromShare=true&action=default&mobileredirect=true
  #1.Characterize overall risk at the national scale
  #  Total number of structures at risk with each year, scenario, and flood frequency

  curr.total.number.of.structures.at.risk <- all.df %>%
                                             #select(-tidyselect::starts_with("per.i")) %>%
                                             summarise_if(is.numeric, sum) %>%
                                             mutate(scenario = year.projection,
                                                    per.i.K12    = round((K12.i/K12.t)        * 100, round.factor),
                                                    per.i.COL    = round((COL.i/COL.t)        * 100, round.factor),
                                                    per.i.DPW    = round((DPW.i/DPW.t)        * 100, round.factor),
                                                    per.i.HQ     = round((HQ.i/HQ.t)          * 100, round.factor),
                                                    per.i.VA     = round((VA.i/VA.t)          * 100, round.factor),
                                                    per.i.PH     = round((PH.i/PH.t)          * 100, round.factor),
                                                    per.i.WT     = round((WT.i/WT.t)          * 100, round.factor),
                                                    per.i.TRI    = round((TRI.i/TRI.t)        * 100, round.factor),
                                                    per.i.SUB    = round((SUB.i/SUB.t)        * 100, round.factor),
                                                    per.i.POW    = round((POW.i/POW.t)        * 100, round.factor),
                                                    per.i.AFF    = round((AFF.i/AFF.t)        * 100, round.factor),
                                                    per.i.BRO    = round((BRO.i/BRO.t)        * 100, round.factor),
                                                    per.i.AMB    = round((AMB.i/AMB.t)        * 100, round.factor),
                                                    per.i.COR    = round((COR.i/COR.t)        * 100, round.factor),
                                                    per.i.FIR    = round((FIR.i/FIR.t)        * 100, round.factor),
                                                    per.i.HOS    = round((HOS.i/HOS.t)        * 100, round.factor),
                                                    per.i.LAW    = round((LAW.i/LAW.t)        * 100, round.factor),
                                                    per.i.POS    = round((POS.i/POS.t)        * 100, round.factor),
                                                    per.i.PRI    = round((PRI.i/PRI.t)        * 100, round.factor),
                                                    per.i.SCAP   = round((SCAP.i/SCAP.t)      * 100, round.factor),
                                                    per.i.SSC    = round((SSC.i/SSC.t)        * 100, round.factor),
                                                    per.i.RNGR   = round((RNGR.i/RNGR.t)      * 100, round.factor),
                                                    per.i.SUP    = round((SUP.i/SUP.t)        * 100, round.factor),
                                                    per.i.GOV    = round((GOV.i/GOV.t)        * 100, round.factor),
                                                    per.i.all    = round((all.i/all.t)        * 100, round.factor))

  all.total.number.of.structures.at.risk  <- rbind(all.total.number.of.structures.at.risk, curr.total.number.of.structures.at.risk)

  #write CS-level summaries to spreadsheet

  all.df <- all.df %>%
    select(-county.geoid) %>%
    rename(
      `Brownfields (I)`                     = BRO.i,
      `Pub. Housing (I)`                    = PH.i,
      `Power Plants (I)`                    = POW.i,
      `Elec. Substations (I)`               = SUB.i,
      `TRI Sites (I)`                       = TRI.i,
      `VA Hospitals (I)`                    = VA.i,
      `WW Treatment Plants (I)`             = WT.i,
      `K-12 Institutions (I)`               = K12.i,
      `Higher Ed. Institutions (I)`         = COL.i,
      `Amb. Services (I)`                   = AMB.i,
      `Fire Stations (I)`                   = FIR.i,
      `Law Enforcement (I)`                 = LAW.i,
      `Prisons (I)`                         = PRI.i,
      `Post Offices (I)`                    = POS.i,
      `Hospitals (I)`                       = HOS.i,
      `State Capitol (I)`                   = SCAP.i,
      `State Supreme Court (I)`             = SSC.i,
      `Courthouses (I)`                     = COR.i,
      `Nat'l Park HQ (I)`                   = HQ.i,
      `Ranger Station (I)`                  = RNGR.i,
      `City Halls (I)`                      = DPW.i,
      `Superfund Sites (I)`                 = SUP.i,
      `Aff. Housing (I)`                    = AFF.i,
      `Federal Gov´t/Military (I)`          = GOV.i,
      `All Infrastructure (I)`              = all.i,

      `Brownfields (T)`                     = BRO.t,
      `Pub. Housing (T)`                    = PH.t,
      `Power Plants (T)`                    = POW.t,
      `Elec. Substations (T)`               = SUB.t,
      `TRI Sites (T)`                       = TRI.t,
      `VA Hospitals (T)`                    = VA.t,
      `WW Treatment Plants (T)`             = WT.t,
      `K-12 Institutions (T)`               = K12.t,
      `Higher Ed. Institutions (T)`         = COL.t,
      `Amb. Services (T)`                   = AMB.t,
      `Fire Stations (T)`                   = FIR.t,
      `Law Enforcement (T)`                 = LAW.t,
      `Prisons (T)`                         = PRI.t,
      `Post Offices (T)`                    = POS.t,
      `Hospitals (T)`                       = HOS.t,
      `State Capitol (T)`                   = SCAP.t,
      `State Supreme Court (T)`             = SSC.t,
      `Courthouses (T)`                     = COR.t,
      `Nat'l Park HQ (T)`                   = HQ.t,
      `Ranger Station (T)`                  = RNGR.t,
      `City Halls (T)`                      = DPW.t,
      `Superfund Sites (T)`                 = SUP.t,
      `Aff. Housing (T)`                    = AFF.t,
      `Federal Gov´t/Military (T)`          = GOV.t,
      `All Infrastructure (T)`              = all.t,

      `Brownfields (P)`                     = per.i.BRO,
      `Pub. Housing (P)`                    = per.i.PH,
      `Power Plants (P)`                    = per.i.POW,
      `Elec. Substations (P)`               = per.i.SUB,
      `TRI Sites (P)`                       = per.i.TRI,
      `VA Hospitals (P)`                    = per.i.VA,
      `WW Treatment Plants PI)`             = per.i.WT,
      `K-12 Institutions (P)`               = per.i.K12,
      `Higher Ed. Institutions (P)`         = per.i.COL,
      `Amb. Services (P)`                   = per.i.AMB,
      `Fire Stations (P)`                   = per.i.FIR,
      `Law Enforcement (P)`                 = per.i.LAW,
      `Prisons (P)`                         = per.i.PRI,
      `Post Offices (P)`                    = per.i.POS,
      `Hospitals (P)`                       = per.i.HOS,
      `State Capitol (P)`                   = per.i.SCAP,
      `State Supreme Court (P)`             = per.i.SSC,
      `Courthouses (P)`                     = per.i.COR,
      `Nat'l Park HQ (P)`                   = per.i.HQ,
      `Ranger Station (P)`                  = per.i.RNGR,
      `City Halls (P)`                      = per.i.DPW,
      `Superfund Sites (P)`                 = per.i.SUP,
      `Aff. Housing (P)`                    = per.i.AFF,
      `Federal Gov´t/Military (P)`          = per.i.GOV,
      `All Infrastructure (P)`              = per.i.all,

      `County Subdivision GEOID`            = cs.geoid,
      `County Subdivision Name`             = cs.name,
      `State`                               = state,
      `County Name`                         = countyname
    )

  xl.workbook<- paste(output.dir, "/county_subdivision_summaries_merged/", year.projection, "_county_subdivision_summary.xlsx",sep="")
  header_style <- createStyle(halign = "center", textDecoration = "bold")
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, year.projection)
  writeData(wb, year.projection, all.df, headerStyle = header_style)
  freezePane(wb, year.projection, firstRow = TRUE)
  #setColWidths(wb, year.projection, cols = 1:ncol(all.df), widths = "auto")
  #setColWidths(wb, "national_summary_by_scenario", cols = 1:ncol(all.df), widths = c(30,3,20, "auto"))
  openxlsx::saveWorkbook(wb, file = xl.workbook, overwrite = TRUE)
}

## write RQ #1 ----
xl.workbook<- paste(output.dir, "/national_summary_by_scenario.xlsx",sep="")
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- openxlsx::createWorkbook()
addWorksheet(wb, "national_summary_by_scenario")
writeData(wb, "national_summary_by_scenario", all.total.number.of.structures.at.risk, headerStyle = header_style)
freezePane(wb, "national_summary_by_scenario", firstRow = TRUE)
setColWidths(wb, "national_summary_by_scenario", cols = 1:ncol(all.df), widths = "auto")
writeData(wb, "national_summary_by_scenario", all.total.number.of.structures.at.risk, headerStyle = header_style)
openxlsx::saveWorkbook(wb, file = xl.workbook, overwrite = TRUE)


create.state.level.summary <- function(.state.abb = NA){
  round.factor <- 2
# summary ---------------------------------------------------
  #.state.abb <- "PR" #debug
  summary.package.region <- assign.summary.package.region(.state.abb)
  summaries.list         <- list.files(path = paste(output.dir,"/inundation_summaries", sep= ""), pattern = "*.gpkg$", full.names = TRUE)
  summaries.list         <- summaries.list[summaries.list %>% grepl(summary.package.region,.)]
  #current.summary.file = summaries.list[1] #debug

  npl.summary <- list.files(path = paste(output.dir,"/npl_inundated", sep= ""), pattern = "*.gpkg$", full.names = TRUE)
  npl.summary <- npl.summary[npl.summary %>% grepl(summary.package.region,.)]

  all.scenarios.summary <- data.frame()
  for(current.summary.file in summaries.list)
  {
    print(scenario)
    scenario <- substr(basename(current.summary.file), 16, regexpr(summary.package.region, basename(current.summary.file)) - 2)
    summary.sf        <- read_sf(current.summary.file)
    npl.summary.sf    <- read_sf(paste(output.dir,"/npl_inundated/is_flooded_sea-", scenario, "-",summary.package.region, "_npl_inundated.gpkg",sep=""))


    curr.scenarios.summary <- summary.sf %>%
                               st_drop_geometry()                %>%
                               filter(state == .state.abb)       %>%
                               group_by(state)                   %>%
                               select(all_of(c(inundated.columns, totals.columns))) %>%
                               summarise(across(everything(), sum, na.rm = TRUE))   %>%
                               select(-state) #drop to avoid duplicate in rbind at the end

    curr.scenarios.summary <- curr.scenarios.summary %>%
                              mutate(scenario = scenario,
                                     state = .state.abb) %>%
                              select(state, scenario, everything())
    #NPL
    if(nrow(npl.summary.sf) > 0)
    {
      npl.summary <- npl.summary.sf %>%
                     st_drop_geometry()                 %>%
                     filter(STATE_CODE == .state.abb) %>%
                     group_by("STATE_CODE")             %>%
                     summarise(freq = n())
      curr.scenarios.summary <- curr.scenarios.summary %>% mutate(SUP.i = nrow(npl.summary))
    }else
    {
      print("no Superfund sites found.")
    }

    all.scenarios.summary <- rbind(all.scenarios.summary, curr.scenarios.summary)

  }
    # calculate inundation percentages ----------------------------------------
    all.scenarios.summary <- all.scenarios.summary %>%
      mutate(per.i.BRO    = round((BRO.i/BRO.t)        * 100, round.factor),
             per.i.PH     = round((PH.i/PH.t)          * 100, round.factor),
             per.i.POW    = round((POW.i/POW.t)        * 100, round.factor),
             per.i.SUB    = round((SUB.i/SUB.t)        * 100, round.factor),
             per.i.TRI    = round((TRI.i/TRI.t)        * 100, round.factor),
             per.i.VA     = round((VA.i/VA.t)          * 100, round.factor),
             per.i.WT     = round((WT.i/WT.t)          * 100, round.factor),
             per.i.AFF    = round((AFF.i/AFF.t)        * 100, round.factor),
             per.i.K12    = round((K12.i/K12.t)        * 100, round.factor),
             per.i.COL    = round((COL.i/COL.t)        * 100, round.factor),
             per.i.AMB    = round((AMB.i/AMB.t)        * 100, round.factor),
             per.i.FIR    = round((FIR.i/FIR.t)        * 100, round.factor),
             per.i.LAW    = round((LAW.i/LAW.t)        * 100, round.factor),
             per.i.PRI    = round((PRI.i/PRI.t)        * 100, round.factor),
             per.i.POS    = round((POS.i/POS.t)        * 100, round.factor),
             per.i.HOS    = round((HOS.i/HOS.t)        * 100, round.factor),
             per.i.SCAP   = round((SCAP.i/SCAP.t)      * 100, round.factor),
             per.i.SSC    = round((SSC.i/SSC.t)        * 100, round.factor),
             per.i.COR    = round((COR.i/COR.t)        * 100, round.factor),
             per.i.HQ     = round((HQ.i/HQ.t)          * 100, round.factor),
             per.i.RNGR   = round((RNGR.i/RNGR.t)      * 100, round.factor),
             per.i.DPW    = round((DPW.i/DPW.t)        * 100, round.factor),
             per.i.SUP    = round((SUP.i/SUP.t)        * 100, round.factor),
             per.i.GOV    = round((GOV.i/GOV.t)        * 100, round.factor),

             all.t        = rowSums(select(., BRO.t, PH.t, POW.t, SUB.t, TRI.t, VA.t, WT.t, AFF.t, K12.t, COL.t, AMB.t, FIR.t, LAW.t, PRI.t, POS.t, HOS.t, SCAP.t, SSC.t, COR.t, HQ.t, RNGR.t, DPW.t, SUP.t, GOV.t)),
             all.i        = rowSums(select(., BRO.i, PH.i, POW.i, SUB.i, TRI.i, VA.i, WT.i, AFF.i, K12.i, COL.i, AMB.i, FIR.i, LAW.i, PRI.i, POS.i, HOS.i, SCAP.i, SSC.i, COR.i, HQ.i, RNGR.i, DPW.i, SUP.i, GOV.i)),
             per.i.all    = round((all.i/all.t)        * 100, round.factor)) %>%
      mutate_all(~replace(., is.nan(.),0)) %>%
      select(all_of(c("state",
                    "scenario",
                    "all.i", "all.t",  "per.i.all",
                    "AFF.i", "AFF.t", "per.i.AFF",
                    "AMB.i", "AMB.t",  "per.i.AMB",
                    "BRO.i", "BRO.t",  "per.i.BRO",
                    "COL.i", "COL.t",  "per.i.COL",
                    "COR.i", "COR.t",  "per.i.COR",
                    "DPW.i", "DPW.t",  "per.i.DPW",
                    "FIR.i", "FIR.t",  "per.i.FIR",
                    "HOS.i", "HOS.t",  "per.i.HOS",
                    "K12.i", "K12.t",  "per.i.K12",
                    "LAW.i", "LAW.t",  "per.i.LAW",
                    "PH.i",  "PH.t",   "per.i.PH",
                    "POS.i", "POS.t",  "per.i.POS",
                    "POW.i", "POW.t",  "per.i.POW",
                    "SUB.i", "SUB.t",  "per.i.SUB",
                    "TRI.i", "TRI.t",  "per.i.TRI",
                    "VA.i",  "VA.t",   "per.i.VA",
                    "WT.i",  "WT.t",   "per.i.WT",
                    "HQ.i",  "HQ.t",   "per.i.HQ",
                    "PRI.i", "PRI.t",  "per.i.PRI",
                    "RNGR.i","RNGR.t", "per.i.RNGR",
                    "SSC.i", "SSC.t",  "per.i.SSC",
                    "SCAP.i","SCAP.t", "per.i.SCAP",
                    "GOV.i", "GOV.t",  "per.i.GOV",
                    "SUP.i", "SUP.t",  "per.i.SUP")))
  return(all.scenarios.summary)
}
create.county.level.summary <- function(.county.geoid = NA){

  options(dplyr.quiet = TRUE)
  round.factor <- 2
  # summary ---------------------------------------------------
  #.county.geoid <- "0604591580" #debug
  .state.fips <- substr(.county.geoid, 1,2)
  state.abb <- states.df  %>% filter(state.fips == .state.fips) %>% select(state.abbreviation) %>% pull() %>% as.character()

  summary.package.region <- assign.summary.package.region(state.abb)
  summaries.list         <- list.files(path = paste(output.dir,"/inundation_summaries", sep= ""), pattern = "*.gpkg$", full.names = TRUE)
  summaries.list         <- summaries.list[summaries.list %>% grepl(summary.package.region,.)]
  #current.summary.file = summaries.list[19] #debug

  all.scenarios.summary <- data.frame()
  for(current.summary.file in summaries.list)
  {
    scenario <- substr(basename(current.summary.file), 16, regexpr(summary.package.region, basename(current.summary.file)) - 2)
    summary.sf        <- read_sf(current.summary.file)
    curr.scenarios.summary <- summary.sf %>%
      st_drop_geometry()                %>%
      mutate(county.geoid = substr(GEOID,1,5)) %>%
      filter(county.geoid == .county.geoid)       %>%
      group_by(county.geoid)                   %>%
      select(all_of(c(inundated.columns, totals.columns))) %>%
      summarise(across(everything(), sum, na.rm = TRUE))
    curr.scenarios.summary <- curr.scenarios.summary %>%
      mutate(scenario = scenario) %>%
      select(scenario, everything())
    all.scenarios.summary <- rbind(all.scenarios.summary, curr.scenarios.summary)
  }
  # calculate inundation percentages ----------------------------------------
  all.scenarios.summary <- all.scenarios.summary %>%
    mutate(per.i.BRO    = round((BRO.i/BRO.t)        * 100, round.factor),
           per.i.PH     = round((PH.i/PH.t)          * 100, round.factor),
           per.i.POW    = round((POW.i/POW.t)        * 100, round.factor),
           per.i.SUB    = round((SUB.i/SUB.t)        * 100, round.factor),
           per.i.TRI    = round((TRI.i/TRI.t)        * 100, round.factor),
           per.i.VA     = round((VA.i/VA.t)          * 100, round.factor),
           per.i.WT     = round((WT.i/WT.t)          * 100, round.factor),
           per.i.AFF    = round((AFF.i/AFF.t)        * 100, round.factor),
           per.i.K12    = round((K12.i/K12.t)        * 100, round.factor),
           per.i.COL    = round((COL.i/COL.t)        * 100, round.factor),
           per.i.AMB    = round((AMB.i/AMB.t)        * 100, round.factor),
           per.i.FIR    = round((FIR.i/FIR.t)        * 100, round.factor),
           per.i.LAW    = round((LAW.i/LAW.t)        * 100, round.factor),
           per.i.PRI    = round((PRI.i/PRI.t)        * 100, round.factor),
           per.i.POS    = round((POS.i/POS.t)        * 100, round.factor),
           per.i.HOS    = round((HOS.i/HOS.t)        * 100, round.factor),
           per.i.SCAP   = round((SCAP.i/SCAP.t)      * 100, round.factor),
           per.i.SSC    = round((SSC.i/SSC.t)        * 100, round.factor),
           per.i.COR    = round((COR.i/COR.t)        * 100, round.factor),
           per.i.HQ     = round((HQ.i/HQ.t)          * 100, round.factor),
           per.i.RNGR   = round((RNGR.i/RNGR.t)      * 100, round.factor),
           per.i.DPW    = round((DPW.i/DPW.t)        * 100, round.factor),
           per.i.SUP    = round((SUP.i/SUP.t)        * 100, round.factor),
           per.i.GOV    = round((GOV.i/GOV.t)        * 100, round.factor),

           all.t        = rowSums(select(., BRO.t, PH.t, POW.t, SUB.t, TRI.t, VA.t, WT.t, AFF.t, K12.t, COL.t, AMB.t, FIR.t, LAW.t, PRI.t, POS.t, HOS.t, SCAP.t, SSC.t, COR.t, HQ.t, RNGR.t, DPW.t, SUP.t, GOV.t)),
           all.i        = rowSums(select(., BRO.i, PH.i, POW.i, SUB.i, TRI.i, VA.i, WT.i, AFF.i, K12.t, COL.i, AMB.i, FIR.i, LAW.i, PRI.i, POS.i, HOS.i, SCAP.i, SSC.i, COR.i, HQ.i, RNGR.i, DPW.i, SUP.i, GOV.i)),
           per.i.all    = round((all.i/all.t)        * 100, round.factor)) %>%
    mutate_all(~replace(., is.nan(.),0)) %>%
    select(all_of(c("county.geoid",
                    "scenario",
                    "all.i", "all.t",  "per.i.all",
                    "AFF.i", "AFF.t", "per.i.AFF",
                    "AMB.i", "AMB.t",  "per.i.AMB",
                    "BRO.i", "BRO.t",  "per.i.BRO",
                    "COL.i", "COL.t",  "per.i.COL",
                    "COR.i", "COR.t",  "per.i.COR",
                    "DPW.i", "DPW.t",  "per.i.DPW",
                    "FIR.i", "FIR.t",  "per.i.FIR",
                    "HOS.i", "HOS.t",  "per.i.HOS",
                    "K12.i", "K12.t",  "per.i.K12",
                    "LAW.i", "LAW.t",  "per.i.LAW",
                    "PH.i",  "PH.t",   "per.i.PH",
                    "POS.i", "POS.t",  "per.i.POS",
                    "POW.i", "POW.t",  "per.i.POW",
                    "SUB.i", "SUB.t",  "per.i.SUB",
                    "TRI.i", "TRI.t",  "per.i.TRI",
                    "VA.i",  "VA.t",   "per.i.VA",
                    "WT.i",  "WT.t",   "per.i.WT",
                    "HQ.i",  "HQ.t",   "per.i.HQ",
                    "PRI.i", "PRI.t",  "per.i.PRI",
                    "RNGR.i","RNGR.t", "per.i.RNGR",
                    "SSC.i", "SSC.t",  "per.i.SSC",
                    "SCAP.i","SCAP.t", "per.i.SCAP",
                    "GOV.i", "GOV.t",  "per.i.GOV",
                    "SUP.i", "SUP.t",  "per.i.SUP")))
  options(dplyr.quiet = FALSE)
  return(all.scenarios.summary)
}

#state-level summaries  ---------
#state <- "PR"
all.scenarios.summaries <- list()
for(state in c(states.in.analysis))
#for(state in c("AL"))
{
  print(state)
  state.summary  <- create.state.level.summary(state)
  all.scenarios.summaries <- rbind(all.scenarios.summaries, state.summary)
}

 all.scenarios.summaries.renamed <-all.scenarios.summaries %>%
  rename(
  `Brownfields (I)`                     = BRO.i,
  `Pub. Housing (I)`                    = PH.i,
  `Power Plants (I)`                    = POW.i,
  `Elec. Substations (I)`               = SUB.i,
  `TRI Sites (I)`                       = TRI.i,
  `VA Hospitals (I)`                    = VA.i,
  `WW Treatment Plants (I)`             = WT.i,
  `K-12 Institutions (I)`               = K12.i,
  `Higher Ed. Institutions (I)`         = COL.i,
  `Amb. Services (I)`                   = AMB.i,
  `Fire Stations (I)`                   = FIR.i,
  `Law Enforcement (I)`                 = LAW.i,
  `Prisons (I)`                         = PRI.i,
  `Post Offices (I)`                    = POS.i,
  `Hospitals (I)`                       = HOS.i,
  `State Capitol (I)`                   = SCAP.i,
  `State Supreme Court (I)`             = SSC.i,
  `Courthouses (I)`                     = COR.i,
  `Nat'l Park HQ (I)`                   = HQ.i,
  `Ranger Station (I)`                  = RNGR.i,
  `City Halls (I)`                      = DPW.i,
  `Superfund Sites (I)`                 = SUP.i,
  `Aff. Housing (I)`                    = AFF.i,
  `All Infrastructure (I)`              = all.i,

  `Brownfields (T)`                     = BRO.t,
  `Pub. Housing (T)`                    = PH.t,
  `Power Plants (T)`                    = POW.t,
  `Elec. Substations (T)`               = SUB.t,
  `TRI Sites (T)`                       = TRI.t,
  `VA Hospitals (T)`                    = VA.t,
  `WW Treatment Plants (T)`             = WT.t,
  `K-12 Institutions (T)`               = K12.t,
  `Higher Ed. Institutions (T)`         = COL.t,
  `Amb. Services (T)`                   = AMB.t,
  `Fire Stations (T)`                   = FIR.t,
  `Law Enforcement (T)`                 = LAW.t,
  `Prisons (T)`                         = PRI.t,
  `Post Offices (T)`                    = POS.t,
  `Hospitals (T)`                       = HOS.t,
  `State Capitol (T)`                   = SCAP.t,
  `State Supreme Court (T)`             = SSC.t,
  `Courthouses (T)`                     = COR.t,
  `Nat'l Park HQ (T)`                   = HQ.t,
  `Ranger Station (T)`                  = RNGR.t,
  `City Halls (T)`                      = DPW.t,
  `Superfund Sites (T)`                 = SUP.t,
  `Aff. Housing (T)`                    = AFF.t,
  `All Infrastructure (T)`              = all.t,

  `Brownfields (P)`                     = per.i.BRO,
  `Pub. Housing (P)`                    = per.i.PH,
  `Power Plants (P)`                    = per.i.POW,
  `Elec. Substations (P)`               = per.i.SUB,
  `TRI Sites (P)`                       = per.i.TRI,
  `VA Hospitals (P)`                    = per.i.VA,
  `WW Treatment Plants (P)`             = per.i.WT,
  `K-12 Institutions (P)`               = per.i.K12,
  `Higher Ed. Institutions (P)`         = per.i.COL,
  `Amb. Services (P)`                   = per.i.AMB,
  `Fire Stations (P)`                   = per.i.FIR,
  `Law Enforcement (P)`                 = per.i.LAW,
  `Prisons (P)`                         = per.i.PRI,
  `Post Offices (P)`                    = per.i.POS,
  `Hospitals (P)`                       = per.i.HOS,
  `State Capitol (P)`                   = per.i.SCAP,
  `State Supreme Court (P)`             = per.i.SSC,
  `Courthouses (P)`                     = per.i.COR,
  `Nat'l Park HQ (P)`                   = per.i.HQ,
  `Ranger Station (P)`                  = per.i.RNGR,
  `City Halls (P)`                      = per.i.DPW,
  `Superfund Sites (P)`                 = per.i.SUP,
  `Aff. Housing (P)`                    = per.i.AFF,
  `All Infrastructure (P)`              = per.i.all
  )

xl.workbook<- paste(output.dir, "state_summary_all_scenarios_years_frequencies.xlsx",sep="/")
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- openxlsx::createWorkbook()
addWorksheet(wb, "state_summaries")
writeData(wb, "state_summaries", all.scenarios.summaries.renamed, headerStyle = header_style)
freezePane(wb,  "state_summaries", firstRow = TRUE)
setColWidths(wb, "state_summaries", cols = 1:ncol(all.scenarios.summaries.renamed), widths = 27)
openxlsx::saveWorkbook(wb, file = xl.workbook, overwrite = TRUE)

## barplots of inundated infrastructure by state -----
inundation.summaries.list <- list.files(paste(output.dir,"inundation_summaries",sep="/"), pattern =  "*.gpkg$", full.names = TRUE)
combined.df <- list()
for(inundation.summary in inundation.summaries.list)
{
  #gpkg.files <- grep(year.projection, inundation.summaries.list, value = TRUE)
  curr.df <- read_sf(inundation.summary)
  base.filename <- file_path_sans_ext(basename(inundation.summary))
  base.string <- substr(base.filename, 16,nchar(base.filename))
  year.string <- substr(base.string,1,4)
  scenario.base.string <- substr(base.string, 6,nchar(base.string))
  scenario.index.end <- gregexpr("[0-9]", scenario.base.string)[[1]][1] #grab index of first occurrence of number
  scenario.string    <-substr(scenario.base.string, 1,  scenario.index.end - 2)

  frequency.string <- substr(base.string, scenario.index.end + 5 , scenario.index.end + 6)

  print(paste(year.string, scenario.string, frequency.string,sep="|"))
  curr.df <- curr.df %>%
            st_drop_geometry() %>%
            mutate(year = year.string,
                   scenario = scenario.string,
                   inundation.frequency = frequency.string)

  combined.df <- rbind(combined.df, curr.df)
  }

table(combined.df$inundation.frequency)
table(combined.df$scenario)
table(combined.df$year)
years.list <- c(2020, 2030, 2050, 2100)
scenarios.list <- c("high", "int", "int_low")
# curr.scenario <- "high"
# curr.state <- states.in.analysis[1]
# curr.year <- 2020
for(curr.state in states.in.analysis)
{
  for (curr.year in years.list)
  {
    for (curr.scenario in scenarios.list)
    {
    combined.df2 <- combined.df %>%
                    filter(state    == curr.state &
                           year     == curr.year &
                           scenario == curr.scenario) %>%
                    group_by(inundation.frequency) %>%
                    select(all_of(c("all.i", inundated.columns))) %>%
                    summarise_if(is.numeric, sum)
    combined.long.df <- combined.df2 %>%
                        select(all_of(c("all.i", "inundation.frequency", inundated.columns))) %>%
                        pivot_longer(.,
                                     cols       = all_of(c("all.i", inundated.columns)),
                                     cols_vary  = "slowest",
                                     names_to   = "infrastucture.type",
                                     values_to  = "inundated.frequency")

    #rename columns -----
    combined.long.df <- combined.long.df %>%
      mutate(infrastucture.type = recode(infrastucture.type,
      BRO.i  = "Brownfields (I)",
      PH.i  = "Pub. Housing (I)",
      POW.i  = "Power Plants (I)",
      SUB.i  = "Elec. Substations (I)",
      TRI.i  = "TRI Sites (I)",
      VA.i   = "VA Hospitals (I)",
      WT.i   = "WW Treatment Plants (I)",
      K12.i  = "K-12 Institutions (I)",
      COL.i  = "Higher Ed. Institutions (I)",
      AMB.i  = "Amb. Services (I)",
      FIR.i  = "Fire Stations (I)",
      LAW.i  = "Law Enforcement (I)",
      PRI.i  = "Prisons (I)",
      POS.i  = "Post Offices (I)",
      HOS.i  = "Hospitals (I)",
      SCAP.i = "State Capitol (I)",
      SSC.i  = "State Supreme Court (I)",
      COR.i  = "Courthouses (I)",
      HQ.i   = "Nat'l Park HQ (I)",
      RNGR.i = "Ranger Station (I)",
      DPW.i  = "City Halls (I)",
      SUP.i  = "Superfund Sites (I)",
      AFF.i  = "Aff. Housing (I)",
      GOV.i  =  "Federal Gov´t/Military (I)",
      all.i  = "All Infrastructure (I)"))

## plot -----
    ggplot(combined.long.df, aes(x = inundation.frequency, y = inundated.frequency, fill= infrastucture.type)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~infrastucture.type) +
      labs(title = paste(curr.state, "|", curr.year, "-", curr.scenario),
           x = "Annual frequency of inundation",
           y = "Frequency of inundated infrastructure") +
      theme(strip.text = element_text(size = 8))
    png.output.dir <- paste(output.dir, "/charts/", curr.state, sep="")
    if (!file.exists(png.output.dir)) {
      dir.create(png.output.dir)
    }
    png.name <- paste(png.output.dir, "/", curr.state, "_", curr.year, "-", curr.scenario, ".png", sep = "")
    print(paste("saving ", png.name, sep =" "))
    ggsave(png.name)
    }
  }
}

#county-level summaries  ---------
all.scenarios.county.summaries <- list()
#counter <- 1
cl <- makeCluster(cores, outfile = paste(tmp.dir, "prepare_results.log", sep="/"))
registerDoParallel(cl)
configuration.variables <- c("create.county.level.summary", "states.df","assign.summary.package.region", "output.dir","inundated.columns","totals.columns", "round.factor")
fips.list <- coastal.counties.df$countyfips
results <- foreach(county.fips = fips.list,
        .packages = c("dplyr","sf"),
        .verbose  = TRUE,
        .export   = configuration.variables
        # .noexport = exclude.vars
) %dopar%
{
  print(county.fips)
  county.summary                 <- create.county.level.summary(county.fips)
  #all.scenarios.county.summaries <- rbind(all.scenarios.county.summaries, county.summary)
  #counter <- counter + 1
}
stopCluster(cl)

combined.county.summaries <- bind_rows(results) %>%
                             left_join(x = .,
                                       y = coastal.counties.df.for.join,
                                       by = c("county.geoid" = "countyfips")) %>%
                             left_join(x = .,
                                       y = svi.df,
                                       by = c("county.geoid" = "FIPS"))

all.scenarios.county.summaries.renamed <-combined.county.summaries %>%
  rename(
    `Brownfields (I)`                     = BRO.i,
    `Pub. Housing (I)`                    = PH.i,
    `Power Plants (I)`                    = POW.i,
    `Elec. Substations (I)`               = SUB.i,
    `TRI Sites (I)`                       = TRI.i,
    `VA Hospitals (I)`                    = VA.i,
    `WW Treatment Plants (I)`             = WT.i,
    `K-12 Institutions (I)`               = K12.i,
    `Higher Ed. Institutions (I)`         = COL.i,
    `Amb. Services (I)`                   = AMB.i,
    `Fire Stations (I)`                   = FIR.i,
    `Law Enforcement (I)`                 = LAW.i,
    `Prisons (I)`                         = PRI.i,
    `Post Offices (I)`                    = POS.i,
    `Hospitals (I)`                       = HOS.i,
    `State Capitol (I)`                   = SCAP.i,
    `State Supreme Court (I)`             = SSC.i,
    `Courthouses (I)`                     = COR.i,
    `Nat'l Park HQ (I)`                   = HQ.i,
    `Ranger Station (I)`                  = RNGR.i,
    `City Halls (I)`                      = DPW.i,
    `Superfund Sites (I)`                 = SUP.i,
    `Aff. Housing (I)`                    = AFF.i,
    `All Infrastructure (I)`              = all.i,

    `Brownfields (T)`                     = BRO.t,
    `Pub. Housing (T)`                    = PH.t,
    `Power Plants (T)`                    = POW.t,
    `Elec. Substations (T)`               = SUB.t,
    `TRI Sites (T)`                       = TRI.t,
    `VA Hospitals (T)`                    = VA.t,
    `WW Treatment Plants (T)`             = WT.t,
    `K-12 Institutions (T)`               = K12.t,
    `Higher Ed. Institutions (T)`         = COL.t,
    `Amb. Services (T)`                   = AMB.t,
    `Fire Stations (T)`                   = FIR.t,
    `Law Enforcement (T)`                 = LAW.t,
    `Prisons (T)`                         = PRI.t,
    `Post Offices (T)`                    = POS.t,
    `Hospitals (T)`                       = HOS.t,
    `State Capitol (T)`                   = SCAP.t,
    `State Supreme Court (T)`             = SSC.t,
    `Courthouses (T)`                     = COR.t,
    `Nat'l Park HQ (T)`                   = HQ.t,
    `Ranger Station (T)`                  = RNGR.t,
    `City Halls (T)`                      = DPW.t,
    `Superfund Sites (T)`                 = SUP.t,
    `Aff. Housing (T)`                    = AFF.t,
    `All Infrastructure (T)`              = all.t,

    `Brownfields (P)`                     = per.i.BRO,
    `Pub. Housing (P)`                    = per.i.PH,
    `Power Plants (P)`                    = per.i.POW,
    `Elec. Substations (P)`               = per.i.SUB,
    `TRI Sites (P)`                       = per.i.TRI,
    `VA Hospitals (P)`                    = per.i.VA,
    `WW Treatment Plants (P)`             = per.i.WT,
    `K-12 Institutions (P)`               = per.i.K12,
    `Higher Ed. Institutions (P)`         = per.i.COL,
    `Amb. Services (P)`                   = per.i.AMB,
    `Fire Stations (P)`                   = per.i.FIR,
    `Law Enforcement (P)`                 = per.i.LAW,
    `Prisons (P)`                         = per.i.PRI,
    `Post Offices (P)`                    = per.i.POS,
    `Hospitals (P)`                       = per.i.HOS,
    `State Capitol (P)`                   = per.i.SCAP,
    `State Supreme Court (P)`             = per.i.SSC,
    `Courthouses (P)`                     = per.i.COR,
    `Nat'l Park HQ (P)`                   = per.i.HQ,
    `Ranger Station (P)`                  = per.i.RNGR,
    `City Halls (P)`                      = per.i.DPW,
    `Superfund Sites (P)`                 = per.i.SUP,
    `Aff. Housing (P)`                    = per.i.AFF,
    `All Infrastructure (P)`              = per.i.all
  )

xl.workbook<- paste(output.dir, "county_summary_all_scenarios_years_frequencies.xlsx",sep="/")
header_style <- createStyle(halign = "center", textDecoration = "bold")
wb <- openxlsx::createWorkbook()
addWorksheet(wb, "county_summaries")
writeData(wb, "county_summaries", combined.county.summaries, headerStyle = header_style)
freezePane(wb,  "county_summaries", firstRow = TRUE)
setColWidths(wb, "county_summaries", cols = 1:ncol(combined.county.summaries), widths = 27)
openxlsx::saveWorkbook(wb, file = xl.workbook, overwrite = TRUE)

#SES analysis
combined.county.summaries <- combined.county.summaries %>%
                             mutate(all.iper.capita = all.i / E_TOTPOP)



#table of all and inundated infra ----
#all infra
infra.sf <- read_sf(paste(output.dir, "/agol/all_critical_infrastructure/all_critical_infrastructure.shp",sep=""))
#infra.by.type.df <-infra.sf %>% st_drop_geometry()

infra.by.type.df <-infra.sf %>% st_drop_geometry() %>%
  mutate(type = case_when(type == "BRO" ~ "Brownfields",
                          type == "PH"  ~ "Public Housing",
                          type == "POW" ~"Power Plant",
                          type == "SUB" ~ "Electrical Substation",
                          type == "TRI" ~ "Toxics Release Inventory Site",
                          type =="VA"   ~ "Veterans' Administration Hospital",
                          type =="WT"   ~ "Wastewater Treatment Plant",
                          type =="K12"  ~ "K-12 School",
                          type =="COL"  ~ "College, University, Technical or Trade School",
                          type =="AMB"  ~ "Ambulance Service",
                          type =="FIR"  ~ "Fire Station",
                          type =="LAW"  ~ "Law Enforcement",
                          type =="PRI"  ~ "Prison or Jail",
                          type =="POS"  ~ "Post Office",
                          type =="HOS"  ~ "Hospital",
                          type =="SCAP" ~ "State Capitol",
                          type =="SSC"  ~ "State Supreme Court",
                          type =="COR"  ~ "Courthouse",
                          type =="HQ"   ~ "Nat'l Park Headquarters",
                          type =="RNGR" ~ "Nat'l Park Ranger Station",
                          type =="DPW"  ~ "City Hall",
                          type == "AFF" ~ "Affordable Housing",
                          type == "GOV" ~ "gov/mil")) %>%
  group_by(type) %>%
  summarize (n = n()) %>%
  filter(type != "gov/mil") %>% #eliminate this category
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(n = format(n, big.mark = ","),) %>%
  rename("Category"  = type,
         "Number of Assets" = n)

#inundated infra
inundated.infra.sf             <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep=""))
all.inundated.infra.df         <- inundated.infra.sf %>% st_drop_geometry() %>% filter(`2020-il-02` ==1) %>% select(c(type, `2020-il-02`))
all.inundated.infra.by.type.df <- all.inundated.infra.df %>% group_by(type) %>% summarize (`2020-il-02` = n())

for(col in year.projection.frequency.list2[2:36])
{
  curr.inundated.infra.df <- inundated.infra.sf %>% st_drop_geometry() %>% filter(!!sym(col) ==1) %>% select(c(type, !!col))
  curr.inundated.infra.by.type.df <- curr.inundated.infra.df %>% group_by(type) %>% summarize ({{col}} := n())
  all.inundated.infra.by.type.df <- left_join(x = all.inundated.infra.by.type.df,
                                              y = curr.inundated.infra.by.type.df,
                                              by = "type")

}
#replace NAs with 0
all.inundated.infra.by.type.df <- all.inundated.infra.by.type.df %>%
                                  mutate_all(~replace(., is.na(.), 0)) %>%
                                  mutate_if(is.numeric, ~ format(., big.mark = ","))



#write to spreadsheet
excel.filename   <- paste(output.dir, "coastal-deadline-technical-appendix-tables.xlsx",sep="/")
wb <- openxlsx::createWorkbook(title="frequencies")

#il scenario -----
il.scenario <- "Intermediate-Low Scenario"
il.scenario.cols <- c( "2020-il-02", "2020-il-12", "2020-il-26", "2030-il-02", "2030-il-12", "2030-il-26", "2050-il-02", "2050-il-12", "2050-il-26", "2100-il-02", "2100-il-12", "2100-il-26")
addWorksheet(wb, il.scenario)
setColWidths(wb, sheet = il.scenario, cols = c(1:13), widths = c(40,rep(17,12)))
mergeCells(wb, il.scenario, rows = 1, cols = c(2:4))
mergeCells(wb, il.scenario, rows = 1, cols = c(5:7))
mergeCells(wb, il.scenario, rows = 1, cols = c(8:10))
mergeCells(wb, il.scenario, rows = 1, cols = c(11:13))
writeData(wb, il.scenario, as.data.frame(t(unlist("2020"))), startCol = 2, colNames = FALSE)
writeData(wb, il.scenario, as.data.frame(t(unlist("2030"))), startCol = 5, colNames = FALSE)
writeData(wb, il.scenario, as.data.frame(t(unlist("2050"))), startCol = 8, colNames = FALSE)
writeData(wb, il.scenario, as.data.frame(t(unlist("2100"))), startCol = 11, colNames = FALSE)

column.names <- c("Category", rep(c("Inundated 2x/year", "Inundated 12x/year", "Inundated 26x/year"),4))
writeData(wb = wb, sheet = il.scenario, x = as.data.frame(t(unlist(column.names))), colNames = FALSE, startRow = 2)
writeData(wb = wb,
               sheet = il.scenario,
               x = all.inundated.infra.by.type.df[,c("type", il.scenario.cols)],
               rowNames = FALSE,
               colNames = FALSE,
               startRow = 3)
openxlsx::saveWorkbook(wb, file = excel.filename, overwrite=TRUE)



