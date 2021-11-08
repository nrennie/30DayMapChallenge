library(tidyverse)
library(osmdata)
library(sf)
library(cowplot)
library(ggmap)
library(patchwork)

#### GLASGOW ####
#choose area
bbx <- getbb("Glasgow, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_glasgow <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="GLASGOW") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_glasgow

#### EDINBURGH ####
#choose area
bbx <- getbb("Edinburgh, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_edinburgh <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="EDINBURGH") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_edinburgh

#### DUNDEE ####
#choose area
bbx <- getbb("Dundee, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_dundee <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="DUNDEE") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_dundee

#### INVERNESS ####
#choose area
bbx <- getbb("Inverness, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_inverness <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="INVERNESS") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_inverness

#### ABERDEEN ####
#choose area
bbx <- getbb("Aberdeen, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_aberdeen <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="ABERDEEN") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_aberdeen

#### STIRLING ####
#choose area
bbx <- getbb("Stirling, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_stirling <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="STIRLING") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_stirling

#### PERTH ####
#choose area
bbx <- getbb("Perth, UK")
#water
water_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("water")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
river_osm <- opq(bbx) %>%
  add_osm_feature(key = "waterway", value = c("river", "riverbank", "canal", "stream", "weir")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_perth <- ggplot() +
  geom_sf(data = water_osm$osm_polygons,col = "#003166", fill = "#003166") +
  geom_sf(data = water_osm$osm_lines,col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_polygons, col = "#003166", fill = "#003166") +
  geom_sf(data = river_osm$osm_lines, col = "#003166", fill = "#003166") +
  labs(subtitle="PERTH") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#003166", family="serif", size=14, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_perth

#### FINAL EDITS ####
#join plots
p <- p_glasgow + p_edinburgh + p_dundee + p_perth + 
  p_inverness + p_aberdeen + p_stirling + plot_spacer() + plot_layout(nrow=2, ncol=4) &
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"))
p

#add title
q <- ggdraw(p) + 
  draw_label(x=0.96, y=0.03, hjust=1, 
             "N. Rennie | Data: OpenStreetMap", color = "#003166", size = 12, fontfamily="serif") +
  draw_label(x=0.85, y=0.3, hjust=0.5, 
             "SCOTLAND'S\nRIVER\nCITIES", color = "#003166", size = 28, fontface="bold", fontfamily="serif") +
  theme(panel.background = element_rect(fill = "gray95", colour = "gray95"))
q

#save image
ggsave(q, filename="2021/viz/map_08.jpg")
