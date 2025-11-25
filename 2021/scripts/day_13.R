library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(sf)
library(ggspatial)
library(rcartocolor)


europe <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf', continent="Europe")

p <- ggplot(europe) + 
  geom_sf(aes(fill=gdp_md_est), colour="#563d2d") +
  coord_sf(xlim = c(-24, 45), ylim = c(30, 73), expand = FALSE) +
  scale_y_continuous(breaks=c(35, 45, 55, 65)) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering(text_col = '#563d2d',
                                                                line_col = '#563d2d',
                                                                fill = "#bd9881")) +
  labs(title="  Europe", subtitle="    N. Rennie | Data: Natural Earth") +
  scale_fill_carto_c(name = "Estimated GDP",
                     type = "diverging", palette = "Earth", direction = 1, 
                     limits=c(0, 3000000), breaks=c(0, 1500000, 3000000), labels=c("0", "1.5M", "3M"),
                     guide = guide_colourbar(direction = "horizontal", 
                                             title.position = "top", 
                                             ticks.colour = "#bd9881")) +
  theme(legend.position = c(0.88, 0.13),
        legend.title=element_text(colour = "#563d2d", size=12, hjust = 0.5, family="serif", face="italic"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#563d2d", size=12, hjust = 0.5, family="serif", face="italic"),
        axis.ticks.length=unit(-0.25, "cm"), 
        panel.grid.major = element_line(colour = alpha(c("#563d2d"),0.2)),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(-1.5,0,0,0), "cm"), #top, right, bottom, left
        plot.background = element_rect(fill = "#bd9881", colour="#bd9881"),
        panel.background = element_rect(fill = "#bd9881", colour="#bd9881"),
        axis.text.x = element_text(margin=unit(c(-0.7,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin=unit(c(0.5,-1,0.5,0.3), "cm")),
        plot.title = element_text(colour = "#563d2d", size=28, hjust = 0, vjust=-10, family="serif", face="italic"),
        plot.subtitle = element_text(colour = "#563d2d", size=12, hjust = 0, vjust=-25, family="serif", face="italic"))
p

#save image
ggsave(p, filename="2021/viz/map_13.jpg")

