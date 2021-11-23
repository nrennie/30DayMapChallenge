library(raster)
library(sp)
library(leaflet)
library(rcartocolor)

# User input CRS
user_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
ghsl_crs <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'  

# lower left:
lower_left <- SpatialPoints(coords = data.frame(x = -800000, 
                                                y = 6000000), 
                            proj4string=CRS(ghsl_crs))
# top right: 
top_right <- SpatialPoints(coords = data.frame(x = -41000, 
                                               y =7000000), 
                           proj4string=CRS(ghsl_crs))

# If needed, R will make the transformation. 
if(as.character(crs(lower_left)) != ghsl_crs){
  lower_left <- spTransform(x = lower_left,
                            CRSobj = ghsl_crs)
}

if(as.character(crs(top_right)) != ghsl_crs){
  top_right <- spTransform(x = top_right,
                           CRSobj = ghsl_crs)
}

my_extent <- extent(c(as.data.frame(lower_left)$x, 
                      as.data.frame(top_right)$x, 
                      as.data.frame(lower_left)$y, 
                      as.data.frame(top_right)$y)) 

built_up <- crop(x = raster("2021/data/ghsl/ghsl.tif"),
                 y = my_extent)


plot_data <- as.data.frame(built_up, xy = T)
plot_data[which(plot_data$ghsl == 10),3] <- NA

p <- ggplot(plot_data, aes(x = x, y = y, fill = ghsl))+
  geom_raster() +
  scale_fill_carto_c(name = "", palette = "Sunset", direction = -1, na.value = "grey90") +
  labs(caption = "N. Rennie | Data: GHSL", title="Building the United Kingdom",
       tag = "The Global Human Settlement Layer\n(GHSL) provides information on how\nbuilt-up land is, at a resolution\nof 250m.") +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#5c53a5", size=28, hjust = 0.45, family="serif", face="bold"),
        plot.caption = element_text(colour = "#5c53a5", size=12, hjust = 0.5, family="serif"),
        plot.tag = element_text(colour = "#5c53a5", size=12, hjust = 0, family="serif"),
        plot.tag.position = c(0.1, 0.77), 
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#832c77", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "grey90"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.7, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_23.jpg")
