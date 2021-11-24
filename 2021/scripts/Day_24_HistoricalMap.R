library(sf)
library(sp)
library(ggplot2)
library(rcartocolor)
library(MapColoring) #devtools::install_github("hunzikp/MapColoring")
library(CEoptim)

#historic shapefile
s_file <- st_read(dsn = "2021/data/historic/UKDefinitionA.shp") 

#coloring
map1 <- as(s_file, "Spatial")
opt.colors <- getOptimalContrast(x=map1, col=carto_pal(7, "SunsetDark"))
s_file$opt_col <- opt.colors

#plot
p <- ggplot() +
  geom_sf(data=s_file, mapping=aes(fill=I(opt_col)), colour=NA) +
  labs(title="HISTORIC COUNTIES\nOF THE\nUNITED KINGDOM", 
       tag="The Historic County Borders\nProject digitised the borders\nof the 92 historic counties\nof the United Kingdom.",
       caption="N. Rennie | Data: www.county-borders.co.uk") +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#832c77", size=28, hjust = 0.5, family="serif", face="bold"),
        plot.caption = element_text(colour = "#832c77", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#832c77", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "grey90"),
        plot.tag.position = c(0.95, 0.5), 
        plot.tag = element_text(colour = '#832c77', family="serif", size=12, hjust=0.5),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 2.5, 0.3, 2.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_24.jpg")

