library(sf)
library(ggplot2)
library(dplyr)
library(rcartocolor)

#read data
s_file <- st_read(dsn = "2021/data/MMO_AnonymisedAISderivedTrackLines2012_SHP_Full/data/Anonymised_AIS_Derived_Track_Lines_2012.shp") 
s_file2 <- filter(s_file, szone == "UK", size != "Null")

#plot shapefile
p <- ggplot() +
  geom_sf(data=s_file2, mapping=aes(colour=size), alpha=0.6) + 
  labs(title="UK Shipping Routes", caption="N. Rennie | Data: environment.data.gov.uk") +
  scale_colour_carto_d(name="", palette = "TealGrn", breaks=c("Small", "Medium", "Large")) +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#257d98", size=24, hjust = 0.5, family="serif", face="bold"),
        plot.caption = element_text(colour = "#257d98", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#257d98", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "grey90", fill="grey90"),
        legend.position = "top",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 2.5, 0.3, 2.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_20.jpg")






