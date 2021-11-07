library(tidyverse)
library(osmdata)
library(sf)
library(cowplot)
library(ggmap)
library(patchwork)

#### GLASGOW ####
#choose area
bbx <- getbb("Glasgow, UK")
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_glasgow <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22", fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="GLASGOW") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
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
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_edinburgh <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22",fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="EDINBURGH") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p_edinburgh

#### DUNDEE ####
#choose area
bbx <- getbb("Dundee, UK")
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_dundee <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22", fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="DUNDEE") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
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
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_inverness <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22", fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="INVERNESS") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
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
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_aberdeen <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22", fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="ABERDEEN") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
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
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_stirling <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22", fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="STIRLING") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
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
#parks
parks_osm <- opq(bbx) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "common", "garden", "golfcourse", "stadium")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
grass_osm <- opq(bbx) %>%
  add_osm_feature(key = "natural", value = c("grassland")) %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
#Make plot
p_perth <- ggplot() +
  geom_sf(data = parks_osm$osm_polygons,col = "#228B22", fill = "#228B22") +
  geom_sf(data = grass_osm$osm_polygons, col = "#228B22", fill = "#228B22") +
  labs(subtitle="PERTH") +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        plot.subtitle = element_text(colour="#228B22", family="serif", size=14, hjust=0.5),
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
p <- p_glasgow + p_edinburgh + p_dundee + p_aberdeen + 
  p_inverness + p_perth + p_stirling + plot_spacer() + plot_layout(nrow=2, ncol=4) &
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"))
p

#add title
q <- ggdraw(p) + 
  draw_label(x=0.96, y=0.03, hjust=1, 
             "N. Rennie | Data: OpenStreetMap", color = "#228B22", size = 12, fontfamily="serif") +
  draw_label(x=0.85, y=0.3, hjust=0.5, 
             "SCOTLAND'S\nGREEN\nCITIES", color = "#228B22", size = 28, fontface="bold", fontfamily="serif") +
  theme(panel.background = element_rect(fill = "gray95", colour = "gray95"))
q

#save image
ggsave(q, filename="2021/viz/map_07.jpg")
