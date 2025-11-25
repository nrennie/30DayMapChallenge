library(tidyverse)
library(rcartocolor)

#read data
d <- read_csv("2021/data/fb.csv") 
colnames(d)[1] <- "region"
d[2,1] <- "USA"
d[9,1] <- "UK"

#world map
world <- map_data("world")

#join data
plot_data <- left_join(world, d, by="region")

#plot map
p <- ggplot(plot_data) +
  geom_map(data = plot_data, map = plot_data,
    aes(long, lat, map_id = region, fill=facebookUsers),
    color = NA, size = 0.1) +
  scale_fill_carto_c(name = "", palette = "Teal", direction = 1, na.value="grey80", 
                     limits=c(0, 300000000),
                     breaks=c(0, 150000000, 300000000), labels=c("0", "150M", "300M")) +
  labs(caption="N. Rennie | Data: worldpopulationreview.com", 
       title = "Registered Facebook (Meta) Users") +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#2a5674", size=28, hjust = 0.5, family="serif", face="bold"),
        plot.caption = element_text(colour = "#2a5674", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#2a5674", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "grey90"),
        legend.position = c(0.1, 0.3),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 0.5, 0.3, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_30.jpg")

