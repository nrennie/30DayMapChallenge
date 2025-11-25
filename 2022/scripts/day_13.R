library(osrm)
library(sf)
library(tidyverse)
library(ggmap)
library(ggtext)

foot <- osrmIsochrone(loc = c(-1.2714292057331058, 51.75662281192641),
                      breaks = c(5),
                      returnclass = "sf",
                      res = 70,
                      osrm.profile = "foot")
bike <- osrmIsochrone(loc = c(-1.2714292057331058, 51.75662281192641),
                      breaks = c(5),
                      returnclass = "sf",
                      res = 70,
                      osrm.profile = "bike")
car <- osrmIsochrone(loc = c(-1.2714292057331058, 51.75662281192641),
                      breaks = c(5),
                      returnclass = "sf",
                      res = 70,
                      osrm.profile = "car")

bbox <- c(bottom=51.74, top=51.78, left=-1.32, right=-1.22)

map <- get_stamenmap(bbox, zoom=13, maptype="toner-background")

st <- "How far can you travel from Oxford Train Station by <span style='color:#fcde9c font-weight:bold'><b>car</b></span>, <span style='color:#e34f6f font-weight:bold'><b>bike</b></span>, or on <span style='color:#7c1d6f font-weight:bold'><b>foot</b></span> in 5 minutes?"

# map
ggmap(map, fill="cornsilk")+
  geom_sf(data=car, 
          aes(geometry=geometry),
          fill="#fcde9c",
          alpha=0.5,
          inherit.aes=FALSE,
          colour=NA) +
  geom_sf(data=bike, 
          aes(geometry=geometry),
          fill="#e34f6f",
          alpha=0.7,
          inherit.aes=FALSE,
          colour=NA) +
  geom_sf(data=foot, 
          aes(geometry=geometry),
          fill="#7c1d6f",
          alpha=0.99,
          inherit.aes=FALSE,
          colour=NA) +
  geom_point(data=data.frame(x=-1.2714292057331058, y=51.75662281192641),
             mapping=aes(x=x, y=y),
             colour = "black",
             size=3) +
  geom_text(data=data.frame(x=-1.27, y=51.75),
            mapping=aes(x=x, y=y, label="Oxford\nTrain\nStation"),
            colour = "#014421",
            family="serif") +
  labs(title = "Oxford Train Station",
       subtitle = st,
       caption = "N. Rennie | Data: OpenStreetMap") +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#014421", size=28, hjust = 0.5, family="serif", face="italic"),
        plot.subtitle = element_markdown(size = 10, family="serif", face = "italic", hjust = 0.5),
        plot.caption = element_text(colour = "#014421", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.6, 0.5, 0.6, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# save
ggsave("2022/maps/day_13.png", height = 6, width = 6, bg = "grey90")





