library(marmap) 
library(ggplot2)

bbx <- getNOAA.bathy(lon1 = -65, lon2 = 7,
                    lat1 = 65, lat2 = 17, 
                    resolution = 5)

p <- autoplot(bbx, geom=c("r", "c")) + 
  scale_fill_etopo() + 
  labs(x = "", y = "", 
       title = "THE ATLANTIC OCEAN", 
       caption="N. Rennie | Data: marmap") +
  theme(plot.background = element_rect(fill = "transparent", colour="transparent"),
        panel.background = element_rect(fill = "transparent", colour="transparent"),
        plot.title = element_text(colour = "#23395d", size=40, hjust = 0.08, vjust=-8, family="sans", face="bold"),
        plot.caption = element_text(colour = "#23395d", size=12, hjust = 0.95, vjust=11, family="sans"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-1.6, -0.6, -0.8, -0.6), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p

#save image
ggsave(p, filename="2021/viz/map_18.jpg")




