library(sf)
library(ggplot2)
library(dplyr)
library(magick)
library(cowplot)

#read in shapefiles
s_file <- st_read(dsn = "2021/data/Shetland_LNCS_GIS_data/Shetland_Local_Nature_Conservation_Sites.shp") 
s_file$col_choice <- rep("Local Nature\nConservation Site", 49)
s_file2 <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "SHETLAND_ISLANDS")

#plot shapefile
p <- ggplot() +
  geom_sf(data=s_file2, colour="#f6b665", fill="#f6b665") +
  geom_sf(data=s_file, mapping=aes(colour=col_choice, fill=col_choice)) +
  scale_fill_manual("", values="#b80d48") +
  scale_colour_manual("", values="#b80d48") +
  labs(title="SHETLAND ISLANDS", 
       tag = "There are 49 local nature\nconservation sites (LNCSs)\nwithin the Shetland Islands.\n\nLNCSs highlight areas with\nimportant biodiversity or\ngeodiversity interests.\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nThe Muckleflugga lighthouse\nas seen from the Hermaness\nNature Reserve.",   
       caption="N. Rennie | Data: Shetland Islands Council | Image:  Lia Tzanidaki @ Unsplash") +
  theme(plot.background = element_rect(fill = "#fcdbe7", colour="#fcdbe7"),
        panel.background = element_rect(fill = "#fcdbe7", colour="#fcdbe7"),
        plot.title = element_text(colour = "#b80d48", size=28, hjust = 0.5, family="serif", face="bold"),
        plot.caption = element_text(colour = "#b80d48", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#b80d48", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "#fcdbe7"),
        plot.tag.position = c(-0.11, 0.61), 
        plot.tag = element_text(colour = '#b80d48', family="serif", size=12, hjust=0),
        legend.position = c(0.95, 0.55),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 3, 0.3, 3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#add image
img <- image_read("2021/images/shetland.png") 
q <- ggdraw(p) + 
  draw_image(img, x=0.28, y=0.2, hjust=0.5, vjust=0.5, scale=0.3) 
q

#save image
ggsave(q, filename="2021/viz/map_19.jpg")

