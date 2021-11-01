library(sf)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(cowplot)
library(extrafont)
library(magick)

#read in licensed premises data
d <- read_csv("2021/data/01_glasgow_premises.csv") %>%
  mutate(grant_date = as.Date(str_sub(GRANTDATE, start = 1, end = 10), format = "%Y/%m/%d"), 
         tot_capacity = pmin(500, as.numeric(replace(CAPACITY_ON, is.na(CAPACITY_ON), 0) + replace(CAPACITY_OFF, is.na(CAPACITY_OFF), 0))))
#read in shapefile 
s_file <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "GLASGOW_CITY")

#make plot
p1 <- ggplot() +
  geom_sf(data=s_file, colour="white", fill=alpha("white", 0.5)) +
  geom_point(data = d, mapping = aes(x=MAP_EAST, y=MAP_NORTH, colour=as.numeric(tot_capacity)), 
             size=1) +
  scale_colour_gradient("Capacity", low="white", high="#c90055", 
                        limits=c(0,500), 
                        breaks=c(0,250,500), 
                        labels=c("0", "250", ">500")) +
  coord_sf(expand=F, xlim=c(250000, 275000)) +
  labs(title="GLASGOW", 
       subtitle="As of 29 October 2021, there are 1902 premises licensed to sell\nalcohol in the Glasgow area.\n",
       caption="#30DapMapChallenge\n\nN. Rennie | Data: data.glasgow.gov.uk & osdatahub.os.uk") +
  guides(colour = guide_colourbar(direction = "horizontal", 
                                  title.position = "top",
                                  ticks.colour="black",
                                  title.hjust = 0.5)) +
  theme(plot.background = element_rect(fill = "#575548", colour="#575548"),
        panel.background = element_rect(fill = "#575548", colour="#575548"),
        plot.title = element_text(colour = "white", size=28, hjust = 0, family="sans", face="bold"),
        plot.subtitle = element_text(colour = "white", size=12, hjust = 0, family="sans"),
        plot.caption = element_text(colour = "white", size=12, hjust = 0, family="sans"),
        legend.title=element_text(colour = "white", size=12, hjust = 0.5, family="sans"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "white", size=12, hjust = 0.5, family="sans"),
        legend.position = c(0.75, 0.15),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 10, 0.2, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1

#add image
img <- image_read("2021/images/01_glasgow.JPG") 
p <- ggdraw(p1) + 
  draw_image(img, x=1.3, hjust=1) +
  theme(panel.background = element_rect(fill = "#575548", colour = "#575548"))
p

#save image
ggsave(p, filename="2021/viz/map_01.jpg")

