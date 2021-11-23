library(osrm)
library(sf)
library(tidyverse)
library(ggmap)
library(ragg)

foot <- osrmIsochrone(loc = c(-4.2581, 55.8591), breaks = c(30),
                      returnclass="sf", res=70, osrm.profile="foot")

bbox <- c(bottom=55.84, top=55.88, left=-4.3, right=-4.216)

map <- get_stamenmap(bbox, zoom=13, maptype="toner-background")

agg_png("map_29.png", units="in", width=8, height=8, res=650)
ggmap(map, fill="cornsilk")+
  geom_sf(data=foot, aes(geometry=geometry), fill="#014421", alpha=0.5, inherit.aes=FALSE, colour=NA)+
  geom_point(data=data.frame(x=-4.2581, y=55.8591), mapping=aes(x=x, y=y), colour = "#014421", size=3) +
  geom_text(data=data.frame(x=-4.253, y=55.8591), mapping=aes(x=x, y=y, label="Glasgow\nCentral\nStation"), colour = "#014421", family="serif") +
  labs(title="Glasgow",
       subtitle = "The green region indicates the areas that can be reached on foot\nfrom Glasgow Central Station within 30 minutes.", 
       caption="N. Rennie | Data: OpenStreetMap") +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#014421", size=28, hjust = 0.5, family="serif", face="italic"),
        plot.subtitle = element_text(colour = "#014421", size=12, hjust = 0.5, family="serif"),
        plot.caption = element_text(colour = "#014421", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.6, 0.5, 0.6, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()
