library(ggplot2)
library(sf)
library(rcartocolor)
library(dplyr)

#Get Scotland map
s_file <- st_read(dsn = "2021/data/scotland/NUTS_Level_1_(January_2018)_Boundaries.shp") %>%
  filter(nuts118nm == "Scotland")

#add temperature data
plot_data <- expand.grid(month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                         city = c("Inverness", "Aberdeen", "Dundee", "Perth", "Stirling", "Edinburgh", "Glasgow"))
plot_data$temp <- c(3,  3,  5,  7,  10, 12, 14, 14, 12, 8,  5,  3, 
                    4, 	4, 	6, 	7, 	9, 	12, 14, 14, 12, 10, 7, 	5,  
                    3, 	4, 	5, 	7, 	10, 12, 14, 14, 12, 9, 	6, 	4, 
                    3, 	4, 	5, 	7, 	10, 13, 15, 14, 12, 9, 	6, 	3, 
                    4, 	4, 	5, 	8, 	10, 13, 15, 15, 12, 9, 	6, 	4, 
                    4, 	4, 	6, 	7, 	10, 13, 15, 15, 13, 10, 6, 	4, 
                    4, 	4, 	6, 	8, 	11, 13, 15, 14, 12, 9, 	6, 	4)
plot_data$lat <- c(rep(57.4778, 12), rep(57.1497, 12), rep(56.4620, 12), rep(56.3950, 12), rep(56.1165, 12), rep(55.9533, 12), rep(55.8642, 12))
plot_data$lon <- c(rep(-4.2247, 12), rep(-2.0943, 12), rep(-2.9707, 12), rep(-3.4308, 12), rep(-3.9369, 12), rep(-3.1883, 12), rep(-4.25188, 12))
#convert to sf
p_data <- st_as_sf(plot_data, coords = c("lon", "lat"), crs=4326)
p_data1 <- st_transform(p_data, crs=27700)

#plot data
p <- ggplot() +
  geom_sf(data=s_file, size=0.5, colour="#8E1600") +
  geom_point(data=p_data1, aes(colour=temp, fill= temp, geometry = geometry), shape = 21, alpha=0.5, size=4, stat = "sf_coordinates") +
  geom_point(data=p_data1, aes(colour=temp, fill= temp, geometry = geometry), size=1, stat = "sf_coordinates") +
  facet_wrap(~month, ncol=12) +
  scale_fill_carto_c(name = "Temperature (°C)", palette = "RedOr", direction = 1, 
                       guide = guide_colourbar(direction = "horizontal", 
                                               title.position = "top", 
                                               ticks.colour = "#8E1600")) +
  scale_colour_carto_c(name = "Temperature (°C)", palette = "RedOr", direction = 1, 
                       guide = guide_colourbar(direction = "horizontal", 
                                               title.position = "top", 
                                               ticks.colour = "#8E1600")) +
  labs(x="", y="", title="HOT IN THE CITY", 
       subtitle="Average monthly temperatures in each of Scotland's seven cities.", 
       caption = "N. Rennie | Data: weatherspark.com") +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#8E1600", size=28, hjust = 0.5, family="serif", face="bold"),
        plot.subtitle = element_text(colour = "#8E1600", size=10, hjust = 0.5, family="serif"),
        plot.caption = element_text(colour = "#8E1600", size=10, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        strip.text = element_text(colour = "#8E1600", size=10, hjust = 0.5, family="serif"),
        strip.background = element_rect(fill = "grey90", colour="grey90"),
        legend.text = element_text(colour = "#8E1600", size=10, hjust = 0.5, family="serif"),
        legend.title = element_text(colour = "#8E1600", size=10, hjust = 0.5, family="serif"),
        legend.key = element_rect(colour = "grey90"),
        plot.tag.position = c(0, 0.81), 
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 0.3, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_27.jpg", width=12, unit="in")


