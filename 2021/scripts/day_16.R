library(sf)
library(ggplot2)
library(rcartocolor)
library(cowplot)
library(magick)

#read in shapefile 
s_file <- st_read(dsn = "2021/data/SG_UrbanRural_2016/SG_UrbanRural_2016.shp") 

#plot shapefile
p <- ggplot() +
  geom_sf(data=s_file, mapping=aes(fill=as.character(UR6FOLD)), colour=NA) +
  labs(title="RURAL SCOTLAND", 
       tag="Remote rural areas are defined as areas\nwith a population of less than 3,000 people,\nand with a drive time of over 30\nminutes to a settlement of 10,000 or more.\n\nMost areas of Scotland are defined as\nrural areas. Just 2% of the land\nis classified as non-rural, and this\nis where 83% of the population\nlive.",
       caption="N. Rennie | Data: SpatialData.gov.scot") +
  scale_fill_carto_d(name="", palette = "SunsetDark", 
                     labels=c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", 
                              "Remote Small Towns", "Accessible Rural Areas", "Remote Rural Areas")) +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#832c77", size=28, hjust = 0.45, family="serif", face="bold"),
        plot.caption = element_text(colour = "#832c77", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#832c77", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "grey90"),
        plot.tag.position = c(-0.11, 0.77), 
        plot.tag = element_text(colour = '#832c77', family="serif", size=12, hjust=0),
        legend.position = c(0.06, 0.15),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 1.5, 0.3, 2.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_16.jpg")

