source("~/coastal_deadline/scripts/configuration.R")
library(ggradar)
library(ggplot2)

scenario.cols <- c("2020-in-02","2020-in-12", "2020-in-26", "2020-il-02", "2020-il-12","2020-il-26","2020-hi-02", "2020-hi-12","2020-hi-26",
                  "2030-in-02","2030-in-12", "2030-in-26", "2030-il-02", "2030-il-12","2030-il-26","2030-hi-02", "2030-hi-12","2030-hi-26",
                  "2050-in-02","2050-in-12", "2050-in-26", "2050-il-02", "2050-il-12","2050-il-26","2050-hi-02", "2050-hi-12","2050-hi-26",
                  "2100-in-02","2100-in-12", "2100-in-26", "2100-il-02", "2100-il-12","2100-il-26","2100-hi-02", "2100-hi-12","2100-hi-26")

inundated.infra.df <- read_sf(paste(output.dir,"/agol/inundated_critical_infrastructure/inundated_critical_infrastructure.shp",sep="")) %>% st_drop_geometry()

#radar graph of 2020 and 2030, hi, 26x ----
curr.df <- inundated.infra.df %>% select(c(type,c("2020-hi-26", "2030-hi-26")))
type.summary <- curr.df %>%
  group_by(type) %>%
  summarize("n2020-hi-26" = sum(`2020-hi-26`),
            "n2030-hi-26" = sum(`2030-hi-26`))
type.summary.wide.2020 <- type.summary %>% select(c(type,`n2020-hi-26`)) %>% pivot_wider(names_from = type, values_from = `n2020-hi-26`) %>% mutate(scenario = "2020") %>% select(c(scenario, everything()))
type.summary.wide.2030 <- type.summary %>% select(c(type,`n2030-hi-26`)) %>% pivot_wider(names_from = type, values_from = `n2030-hi-26`) %>% mutate(scenario = "2030") %>% select(c(scenario, everything()))
radar.graph.data.df <- rbind(type.summary.wide.2020,type.summary.wide.2030)
freq.range <- radar.graph.data.df %>% select(-scenario) %>% range()
ggradar(radar.graph.data.df,
        axis.label.size = 4,
        grid.min = 0, grid.mid = 72, grid.max = 144,
        values.radar = c(freq.range[1], median(freq.range), freq.range[2]),
        group.point.size = 3,
        group.line.width = 1,
        group.colours = c("#009fdf", "#d7117d"),
        background.circle.colour = "#ffffff",
        gridline.min.linetype = "solid",
        gridline.mid.linetype = "solid",
        gridline.max.linetype = "solid",
        gridline.min.colour = "grey",
        gridline.mid.colour = "grey",
        gridline.max.colour = "grey",
        plot.title = "High scenario, 26x") +
        theme(axis.text = element_text(hjust = 0.5))



#radar graph of 2030 and 2050, hi, 2x ----
curr.df <- inundated.infra.df %>% select(c(type,c("2030-hi-02", "2050-hi-02")))
type.summary <- curr.df %>%
  group_by(type) %>%
  summarize("n2030-hi-02" = sum(`2030-hi-02`),
            "n2050-hi-02" = sum(`2050-hi-02`))
type.summary.wide.2030 <- type.summary %>% select(c(type,`n2030-hi-02`)) %>% pivot_wider(names_from = type, values_from = `n2030-hi-02`) %>% mutate(scenario = "2030") %>% select(c(scenario, everything()))
type.summary.wide.2050 <- type.summary %>% select(c(type,`n2050-hi-02`)) %>% pivot_wider(names_from = type, values_from = `n2050-hi-02`) %>% mutate(scenario = "2050") %>% select(c(scenario, everything()))
radar.graph.data.df    <- rbind(type.summary.wide.2030,type.summary.wide.2050)
freq.range <- radar.graph.data.df %>% select(-scenario) %>% range()
circle.offset <- 75
line.color <- "grey"
range <- seq(0,700,length.out = 5)
ggradar(radar.graph.data.df,
        axis.label.size = 4,
        grid.min = range[1], grid.mid = 350, grid.max = range[5],
        #values.radar = c(0,350,700),
        values.radar = NA,
        group.point.size = 3,
        group.line.width = 1,
        group.colours = c("#009fdf", "#d7117d"),
        background.circle.colour = "#ffffff",
        gridline.min.linetype = "solid",
        gridline.mid.linetype = "solid",
        gridline.max.linetype = "solid",
        gridline.min.colour = line.color,
        gridline.mid.colour = line.color,
        gridline.max.colour = line.color,
        plot.title = "High scenario, 2x") +
  annotate("path",
           x = (circle.offset + range[2]) * cos(seq(0, 2*pi, length.out = 100)),
           y = (circle.offset + range[2]) * sin(seq(0, 2*pi, length.out = 100)),
           color = line.color) +
  annotate("path",
           x = (circle.offset + range[4]) * cos(seq(0, 2*pi, length.out = 100)),
           y = (circle.offset + range[4]) * sin(seq(0, 2*pi, length.out = 100)),
           color = line.color) +
  theme(axis.text = element_text(hjust = 0.5)) +
  annotate("text", x = circle.offset + range, y = 0, label = range, color = "black", size = 5)

write.csv(radar.graph.data.df, paste("/home/rama/coastal_deadline/data/data_for_cynthia/inundated_infra_by_type_national_2030-2050_hi_2x.csv",sep="/"),row.names = FALSE)


#% change in PH 2020-2030
old_value <- radar.graph.data.df[1,"Public Housing"]
new_value <- radar.graph.data.df[2,"Public Housing"]

# Calculate percent change for each pair of values
percent_change <- ((new_value - old_value) / old_value) * 100


#radar graph of 2050 and 2100, il, 2x ----
curr.df <- inundated.infra.df %>% select(c(type,c("2050-il-02", "2100-il-02")))
type.summary <- curr.df %>%
  group_by(type) %>%
  summarize("n2050-il-02" = sum(`2050-il-02`),
            "n2100-il-02" = sum(`2100-il-02`))
type.summary.wide.2050 <- type.summary %>% select(c(type,`n2050-il-02`)) %>% pivot_wider(names_from = type, values_from = `n2050-il-02`) %>% mutate(scenario = "2050") %>% select(c(scenario, everything()))
type.summary.wide.2100 <- type.summary %>% select(c(type,`n2100-il-02`)) %>% pivot_wider(names_from = type, values_from = `n2100-il-02`) %>% mutate(scenario = "2100") %>% select(c(scenario, everything()))
radar.graph.data.df <- rbind(type.summary.wide.2050,type.summary.wide.2100)
freq.range <- radar.graph.data.df %>% select(-scenario) %>% range()
circle.offset <- 75
line.color <- "grey"
radar.graph.data.df %>%  select_if(is.numeric) %>% max()

range <- seq(0,1100,length.out = 5)
ggradar(radar.graph.data.df,
        axis.label.size = 4,
        grid.min = range[1], grid.mid = 350, grid.max = range[5],
        #values.radar = c(0,350,700),
        values.radar = NA,
        group.point.size = 3,
        group.line.width = 1,
        group.colours = c("#009fdf", "#d7117d"),
        background.circle.colour = "#ffffff",
        gridline.min.linetype = "solid",
        gridline.mid.linetype = "solid",
        gridline.max.linetype = "solid",
        gridline.min.colour = line.color,
        gridline.mid.colour = line.color,
        gridline.max.colour = line.color,
        plot.title = "Intermediate-Low scenario, 2x") +
  annotate("path",
           x = (circle.offset + range[2]) * cos(seq(0, 2*pi, length.out = 100)),
           y = (circle.offset + range[2]) * sin(seq(0, 2*pi, length.out = 100)),
           color = line.color) +
  annotate("path",
           x = (circle.offset + range[4]) * cos(seq(0, 2*pi, length.out = 100)),
           y = (circle.offset + range[4]) * sin(seq(0, 2*pi, length.out = 100)),
           color = line.color) +
  theme(axis.text = element_text(hjust = 0.5)) +
  annotate("text", x = circle.offset + range, y = 0, label = range, color = "black", size = 5)




#% change in PH 2020-2030
old_value <- radar.graph.data.df[1,"Public Housing"]
new_value <- radar.graph.data.df[2,"Public Housing"]

# Calculate percent change for each pair of values
percent_change <- ((new_value - old_value) / old_value) * 100


#radar graph of 2050 and 2100, hi, 12x ----
curr.df <- inundated.infra.df %>% select(c(type,c("2050-hi-12", "2100-hi-12")))
type.summary <- curr.df %>%
  group_by(type) %>%
  summarize("n2050-hi-12" = sum(`2050-hi-12`),
            "n2100-hi-12" = sum(`2100-hi-12`))
type.summary.wide.2050 <- type.summary %>% select(c(type,`n2050-hi-12`)) %>% pivot_wider(names_from = type, values_from = `n2050-hi-12`) %>% mutate(scenario = "2050") %>% select(c(scenario, everything()))
type.summary.wide.2100 <- type.summary %>% select(c(type,`n2100-hi-12`)) %>% pivot_wider(names_from = type, values_from = `n2100-hi-12`) %>% mutate(scenario = "2100") %>% select(c(scenario, everything()))
radar.graph.data.df <- rbind(type.summary.wide.2050,type.summary.wide.2100)
freq.range <- radar.graph.data.df %>% select(-scenario) %>% range()
circle.offset <- 75
line.color <- "grey"
radar.graph.data.df %>%  select_if(is.numeric) %>% max()

range <- seq(0,3000,length.out = 5)
ggradar(radar.graph.data.df,
        axis.label.size = 4,
        grid.min = range[1], grid.mid = 350, grid.max = range[5],
        #values.radar = c(0,350,700),
        values.radar = NA,
        group.point.size = 3,
        group.line.width = 1,
        group.colours = c("#009fdf", "#d7117d"),
        background.circle.colour = "#ffffff",
        gridline.min.linetype = "solid",
        gridline.mid.linetype = "solid",
        gridline.max.linetype = "solid",
        gridline.min.colour = line.color,
        gridline.mid.colour = line.color,
        gridline.max.colour = line.color,
        plot.title = "High scenario, 12x") +
  annotate("path",
           x = (circle.offset + range[2]) * cos(seq(0, 2*pi, length.out = 100)),
           y = (circle.offset + range[2]) * sin(seq(0, 2*pi, length.out = 100)),
           color = line.color) +
  annotate("path",
           x = (circle.offset + range[4]) * cos(seq(0, 2*pi, length.out = 100)),
           y = (circle.offset + range[4]) * sin(seq(0, 2*pi, length.out = 100)),
           color = line.color) +
  theme(axis.text = element_text(hjust = 0.5)) +
  annotate("text", x = circle.offset + range, y = 0, label = range, color = "black", size = 5)




#% change in PH 2020-2030
old_value <- radar.graph.data.df[1,"Public Housing"]
new_value <- radar.graph.data.df[2,"Public Housing"]

# Calculate percent change for each pair of values
percent_change <- ((new_value - old_value) / old_value) * 100

