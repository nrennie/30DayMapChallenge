library(tidyverse)  
library(viridis)   
library(ggplot2)   
library(stringr)
library(sf)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(cowplot)
library(extrafont)
library(magick)
library(hexbin)

#read in shapefile 
s_file <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "GLASGOW_CITY")

#read population data
d <- read_csv("2021/data/03_population.csv") %>%
  filter(Sex == "Persons", 
         `Area code` %in% unique(s_file$CODE))
colnames(d)[1] <- "CODE"

#get coordinates of centre of each area
plot_data <- tibble(CODE=c(), x=c(), y=c())
for (i in 1:length(unique(s_file$CODE))){
  k <- st_coordinates(s_file$geometry[[i]])
  t <- tibble(CODE=rep(s_file$CODE[i], nrow(k)), 
         x=k[,"X"], 
         y=k[,"Y"])
  plot_data <- rbind(plot_data, t)
}

#join data
plot_data1 <- left_join(plot_data, d, by="CODE") %>%
  mutate(z = as.numeric(gsub(",","",`All Ages`)))

#make plot
p1 <- ggplot(plot_data1, aes(x=x, y=y, z=z)) +
  stat_summary_hex(bins=9, fun = "mean", colour="grey") +  
  scale_fill_gradient("Population", low="white", high="#c90055") +
  labs(title="GLASGOW", 
       subtitle="",
       caption="#30DapMapChallenge\nN. Rennie | Data: www.nrscotland.gov.uk & osdatahub.os.uk") +
  guides(colour = guide_colourbar(direction = "horizontal", 
                                  title.position = "top",
                                  ticks.colour="black",
                                  title.hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#c90055", size=28, hjust = 0, family="serif", face="bold"),
        plot.subtitle = element_text(colour = "#c90055", size=12, hjust = 0, family="serif"),
        plot.caption = element_text(colour = "#c90055", size=12, hjust = 0, family="serif"),
        legend.title=element_text(colour = "#c90055", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#c90055", size=12, hjust = 0.5, family="serif"),
        legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1

#fix background
p <- ggdraw(p1) + 
  draw_label(x=0.92, y=0.82, hjust=1, 
             "As of 2020, the total population of Glasgow City is 635,640.\nThe most populous area is the Anderston/City/Yorkhill region\nwith 35,735 people living there. The least is North East\nGlasgow with 21,045 people.", 
             color = "#c90055", size = 12, fontfamily="serif") +
  theme(panel.background = element_rect(fill = "grey90", colour = "grey90"))
p

#save image
ggsave(p, filename="2021/viz/map_04.jpg")

