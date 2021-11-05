library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(cowplot)

#background map
bg_map <- get_map(getbb("Glasgow"), maptype = "toner-background")

#query OSM data
q1 <- opq("Glasgow") %>%
  add_osm_feature("amenity", "restaurant") 
restaurants <- osmdata_sf(q1)
q2 <- opq("Glasgow") %>%
  add_osm_feature("amenity", "cafe") 
cafes <- osmdata_sf(q2)
q3 <- opq("Glasgow") %>%
  add_osm_feature("amenity", "bar") 
bars <- osmdata_sf(q3)
q4 <- opq("Glasgow") %>%
  add_osm_feature("amenity", "pub") 
pubs <- osmdata_sf(q4)

#final map
p1 <- ggmap(bg_map)+
  geom_sf(data = restaurants$osm_points, inherit.aes = FALSE, aes(fill="Restaurant"), alpha = .5, size = 2, shape = 21) +
  geom_sf(data = cafes$osm_points, inherit.aes = FALSE, aes(fill="Cafe"), alpha = .5, size = 2, shape = 21) +
  geom_sf(data = bars$osm_points, inherit.aes = FALSE, aes(fill="Bar"), alpha = .5, size = 2, shape = 21) +
  geom_sf(data = pubs$osm_points, inherit.aes = FALSE, aes(fill="Pub"), alpha = .5, size = 2, shape = 21) +
  scale_fill_manual("", values=c("Restaurant"="#FFA45E", "Cafe"="#EC4176", "Bar"="#543884", "Pub"="#9A77CF")) +
  labs(x = "", y = "") +
  theme(plot.background = element_rect(fill = "#575548", colour="#575548"),
        panel.background = element_rect(fill = "#575548", colour="#575548"),
        plot.title = element_text(colour = "white", size=28, hjust = 0, family="sans", face="bold"),
        plot.subtitle = element_text(colour = "white", size=12, hjust = 0, family="sans"),
        plot.caption = element_text(colour = "white", size=12, hjust = 0, family="sans"),
        legend.title=element_text(colour = "white", size=12, hjust = 0.5, family="sans"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#262254", size=12, hjust = 0, family="sans"),
        legend.key=element_blank(),
        legend.position = c(0.15, 0.8),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1

#add title
p <- ggdraw(p1) + 
  draw_label(x=0.5, y=0.03, hjust=0.5, 
             "N. Rennie | Data: OpenStreetMap", color = "#262254", size = 12, fontfamily="sans") +
  draw_label(x=0.5, y=0.95, hjust=0.5, 
             "GLASGOW PUBS", color = "#262254", size = 24, fontface="bold", fontfamily="sans") +
  draw_label(x=0.5, y=0.9, hjust=0.5, 
             "(... and cafes, bars, and restaurants)", color = "#262254", size = 16, fontfamily="sans")
p

#save image
ggsave(p, filename="2021/viz/map_05.jpg")
