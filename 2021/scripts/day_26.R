library(tidyverse)
library(cowplot)
library(showtext)
library(ragg) 
library(biscale) 
library(sf) 

#read in shapefile 
s_file <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "GLASGOW_CITY")

#read population data
d <- read_csv("2021/data/03_population.csv") %>%
  filter(Sex == "Persons", 
         `Area code` %in% unique(s_file$CODE))
colnames(d)[1] <- "CODE"


#join data
plot_data <- left_join(s_file, d, by="CODE") %>%
  mutate(pop = as.numeric(gsub(",","",`All Ages`)))

#make classes
g_classes <- bi_class(plot_data, x=HECTARES, y=pop, style="quantile", dim=3) 

#plot map
p <- ggplot() + 
  geom_sf(data=g_classes, mapping=aes(fill=bi_class), color="#2A5A5B", size=0.1, show.legend=FALSE) +
  bi_scale_fill(pal="DkCyan", dim=3, na.value="grey50") + 
  labs(title="Population Density\nof\nGlasgow",
       caption="N. Rennie | Data: www.nrscotland.gov.uk & osdatahub.os.uk") + 
  theme(plot.background = element_rect(fill = "#c6e4e5", colour="#c6e4e5"),
        panel.background = element_rect(fill = "#c6e4e5", colour="#c6e4e5"),
        plot.title = element_text(colour = "#2A5A5B", size=28, hjust = 0, family="serif"),
        plot.subtitle = element_text(colour = "#2A5A5B", size=18, hjust = 0, family="serif"),
        plot.caption = element_text(colour = "#2A5A5B", size=12, hjust = 0, family="serif"),
        legend.position="none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#add legend
p_legend <- bi_legend(pal="DkCyan",
                      dim=3,
                      xlab="Area (Hectares)",
                      ylab="Population",
                      size=16) + 
  theme(plot.background = element_rect(fill = "#c6e4e5", colour="#c6e4e5"),
        panel.background = element_rect(fill = "#c6e4e5", colour="#c6e4e5"),
        axis.title = element_text(colour = "#2A5A5B", size=12, hjust = 0, family="serif"),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p_legend
q <- ggdraw() + 
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(p_legend, 0.68, 0.72, 0.2, 0.2, scale=1)
q


#save image
ggsave(q, filename="2021/viz/map_26.jpg")

