source("~/coastal_deadline/scripts/configuration.R")
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library("RColorBrewer")
library(htmltools)
library(oceanis)
library(rlang)
library(leaflet.extras)

year.projection.frequency.list <- c("2020-high-02", "2020-high-12", "2020-high-26", "2020-int_low-02", "2020-int_low-12", "2020-int_low-26", "2020-int-02", "2020-int-12", "2020-int-26",
                                    "2030-high-02", "2030-high-12", "2030-high-26", "2030-int_low-02", "2030-int_low-12", "2030-int_low-26", "2030-int-02", "2030-int-12", "2030-int-26",
                                    "2050-high-02", "2050-high-12", "2050-high-26", "2050-int_low-02", "2050-int_low-12", "2050-int_low-26", "2050-int-02", "2050-int-12", "2050-int-26",
                                    "2100-high-02", "2100-high-12", "2100-high-26", "2100-int_low-02", "2100-int_low-12", "2100-int_low-26", "2100-int-02", "2100-int-12", "2100-int-26")

EAST.inundation.summaries.list <- list.files(path=paste(output.dir, "/inundation_summaries", sep=""), pattern=".*EAST.*\\.gpkg$", full.names = TRUE)
WEST.inundation.summaries.list <- list.files(path=paste(output.dir, "/inundation_summaries", sep=""), pattern=".*WEST.*\\.gpkg$", full.names = TRUE)
GU.inundation.summaries.list   <- list.files(path=paste(output.dir, "/inundation_summaries", sep=""), pattern=".*GU.*\\.gpkg$", full.names = TRUE)
PRVI.inundation.summaries.list <- list.files(path=paste(output.dir, "/inundation_summaries", sep=""), pattern=".*PR.*\\.gpkg$", full.names = TRUE)

cs.conus.sf <- rbind(cs.west.sf, cs.east.sf) %>% st_simplify(.,preserveTopology = FALSE, dTolerance = 1000) %>% st_transform(st_crs(4326))

inundated.ci.sf <- read_sf(paste(output.dir, "/agol/inundated_critical_infrastructure",sep="")) %>%
                   mutate(type = recode(type,
                                   BRO  = "Brownfield",
                                   PH  = "Pub. Housing",
                                   POW = "Power Plant",
                                   SUB  = "Elec. Substation",
                                   TRI  = "TRI Site",
                                   VA   = "VA Hospital",
                                   WT   = "WW Treatment Plant",
                                   K12  = "K-12 Institution",
                                   COL  = "Higher Ed. Institutions",
                                   AMB  = "Amb. Service",
                                   FIR  = "Fire Station)",
                                   LAW  = "Law Enforcement)",
                                   PRI  = "Prison/Jail",
                                   POS  = "Post Office",
                                   HOS  = "Hospital",
                                   SCAP = "State Capitol",
                                   SSC  = "State Supreme Court",
                                   COR  = "Courthouse",
                                   HQ   = "Nat'l Park HQ",
                                   RNGR = "Ranger Station",
                                   DPW  = "City Hall",
                                   SUP  = "Superfund Site",
                                   AFF  = "Aff. Housing",
                                   GOV  =  "Federal Gov´t/Military"))




tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))
all.index.html <- list()

per.i.all.colors <-colorBin(c(NA, "#ffbfbf","#ff8080", "#ff4040", "#ff0000"), domain = c(0,0.001, 25,50,75,100), bins = c(0,0.001, 25,50,75,100))
#colorQuantile("YlOrRd", domain = sf_data$variable_to_color, n = num_breaks)


for(ypf in year.projection.frequency.list)
{
  print(ypf)
  east.gpkg <-  grep(ypf, EAST.inundation.summaries.list, value = TRUE)
  west.gpkg <-  grep(ypf, WEST.inundation.summaries.list, value = TRUE)
  inundated.west.sf <- read_sf(west.gpkg)
  inundated.east.sf <- read_sf(east.gpkg)
  inundated.sf      <- rbind(inundated.east.sf, inundated.west.sf) %>% st_simplify(.,preserveTopology = FALSE, dTolerance = 100) %>% st_transform(st_crs(4326))

  ypf2 <- gsub("high", "hi", ypf)
  ypf2 <- gsub("int_low", "il", ypf2)
  ypf2 <- gsub("int", "in", ypf2)
  curr.inundated.ci.sf <- inundated.ci.sf %>% select(all_of(c("name", "address", "city", "state", "type", ypf2))) %>% filter(!!sym(ypf2) ==1)

  circle_svg <- '<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20">
               <circle cx="10" cy="10" r="8" fill="blue" />
               </svg>'

  title <- tags$div(
    tag.map.title, HTML(ypf)
  )


  #conus map
  m <- leaflet() %>%
       addTiles()  %>%
    #addPolygons(data = cs.conus.sf, fill = FALSE, fillOpacity = 0.4, stroke = TRUE, weight = 1, color = "black") %>%
    addPolygons(data = inundated.sf, fillColor = ~per.i.all.colors(per.i.all),  # Symbolize by the values in symbol_field
                color = "black",  # Outline color
                weight = 0.4,     # Outline weight
                opacity = 1,      # Outline opacity
                fillOpacity = 0.7,  # Fill opacity
                popup = ~paste("<b>", cs.name,"</b>","<br>",
                               "<b> Scenario: ", ypf, "</b>","<br>",
                               "All Infrastructure (I): ", all.i, "<br>",
                               "All Infrastructure (T): ", all.t, "<br>",
                               "All Infrastructure (P): ", per.i.all, "<br>"
                               )
                )
    #inundated infra
    m <- addMarkers(m,
                    data = st_as_sf(curr.inundated.ci.sf, coords = c("lon", "lat"), crs = 4326),
                    clusterOptions = markerClusterOptions(),
                    popup =  ~paste("<b>", type ,"</b>","<br>",
                                    "<b>", name,"</b>","<br>",
                                    address," ", city, " ", state, "<br>"
                    ))
    m <- addControl(m, title, position = "topleft", className="map-title")

    m <-   addLegend(m, pal = per.i.all.colors, values = per.i.all.colors,
                     title = paste("<small>All Infrastructure (Percent Inundated) - ",ypf, "</small>",sep=""),
                     labels = c("0","1-25","25-50","50-75", "75-100"),
                     position = 'bottomleft')

  leaflet.map.html <- paste(output.dir, "/leaflet/" , ypf,"_inundation_summary.html", sep="")

  saveWidget(m, leaflet.map.html)
}
all.index.html <- list()
for(ypf in year.projection.frequency.list)
{
  index.html <- paste(ypf,"_inundation_summary.html", sep="")
  all.index.html <- c(all.index.html, index.html)
}
all.index.html <- unlist(all.index.html)

# List of file names
file_names <- c(
  "2020-high-02_inundation_summary.html",
  "2020-high-12_inundation_summary.html",
  "2020-high-26_inundation_summary.html",
  "2020-int_low-02_inundation_summary.html",
  "2020-int_low-12_inundation_summary.html",
  "2020-int_low-26_inundation_summary.html",
  "2020-int-02_inundation_summary.html",
  "2020-int-12_inundation_summary.html",
  "2020-int-26_inundation_summary.html",
  "2030-high-02_inundation_summary.html",
  "2030-high-12_inundation_summary.html",
  "2030-high-26_inundation_summary.html",
  "2030-int_low-02_inundation_summary.html",
  "2030-int_low-12_inundation_summary.html",
  "2030-int_low-26_inundation_summary.html",
  "2030-int-02_inundation_summary.html",
  "2030-int-12_inundation_summary.html",
  "2030-int-26_inundation_summary.html",
  "2050-high-02_inundation_summary.html",
  "2050-high-12_inundation_summary.html",
  "2050-high-26_inundation_summary.html",
  "2050-int_low-02_inundation_summary.html",
  "2050-int_low-12_inundation_summary.html",
  "2050-int_low-26_inundation_summary.html",
  "2050-int-02_inundation_summary.html",
  "2050-int-12_inundation_summary.html",
  "2050-int-26_inundation_summary.html",
  "2100-high-02_inundation_summary.html",
  "2100-high-12_inundation_summary.html",
  "2100-high-26_inundation_summary.html",
  "2100-int_low-02_inundation_summary.html",
  "2100-int_low-12_inundation_summary.html",
  "2100-int_low-26_inundation_summary.html",
  "2100-int-02_inundation_summary.html",
  "2100-int-12_inundation_summary.html",
  "2100-int-26_inundation_summary.html"
)

# Create HTML content
html_content <- paste("<ul>",
                      sapply(file_names, function(file_name) {
                        paste("<li><a href='", file_name, "'>", file_name, "</a></li>", sep = "")
                      }),
                      "</ul>",
                      sep = "")

# Write HTML content to a file
writeLines(html_content,  paste(output.dir, "/leaflet/index.html", sep=""))




