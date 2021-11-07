library(tidyverse)
library(osmdata)
library(sf)
library(cowplot)

#choose area
bbx <- getbb("Great Britain")

#glasgow post offices 
post_offices_osm <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "amenity",
    value = c(
      "post_office"
    )
  ) %>%
  osmdata_sf()

#filter data
plot_data <- post_offices_osm$osm_points %>%
  filter(is.na(addr.country)|addr.country != "FR")

#Make plot
p <- ggplot() +
  geom_sf(data = plot_data, col = "#9E1A1A", size = 0.5) +
  theme(plot.background = element_rect(fill = "gray95", color = NA),
        panel.background = element_rect(fill = "gray95", colour="gray95"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p

#fix background and add title
q <- ggdraw(p) + 
  draw_label(x=0.8, y=0.03, hjust=0.5, 
             "N. Rennie | Data: OpenStreetMap", color = "#9E1A1A", size = 12, fontfamily="serif") +
  draw_label(x=0.75, y=0.8, hjust=0.5, 
             "GREAT\nBRITISH\nPOST\nOFFICES", color = "#9E1A1A", size = 28, fontface="bold", fontfamily="serif") +
  theme(panel.background = element_rect(fill = "gray95", colour = "gray95"))
q

#save image
ggsave(q, filename="2021/viz/map_06.jpg")
