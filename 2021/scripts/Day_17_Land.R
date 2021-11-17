library(sf)
library(ggplot2)
library(dplyr)
library(pals)

#read in shapefile 
s_file <- st_read(dsn = "2021/data/land_cover/SG_LandCoverScotland_1988.shp") 
plot_data <- s_file %>% filter(!is.na(LCS_LABEL))

plot_data$lcs_label2 <- recode(plot_data$LCS_LABEL, 
       "cliffs" = "Cliffs", 
       "blanket bog & peatlands" = "Bog & peatlands", 
       "smooth grassland" = "Grassland", 
       "heather moor" = "Moorland", 
       "maritime grasslands & heaths" = "Grassland", 
       "factories & urban" = "Urban", 
       "duneland" = "Duneland", 
       "salt marsh" = "Salt marsh",
       "improved grassland" = "Grassland", 
       "water" = "Water", 
       "quarries" = "Quarries", 
       "airfields" = "Airfields", 
       "montane vegetation" = "Vegetation", 
       "wetland" = "Wetland", 
       "coarse grassland" = "Grassland", 
       "low scrub" = "Other", 
       "recreational land" = "Recreational", 
       "coniferous plantation" = "Tree plantation", 
       "mixed woodland" = "Woodland", 
       "other land" = "Other", 
       "broadleaved woodland" = "Woodland",
       "arable" = "Farming", 
       "recently ploughed land" = "Farming", 
       "open canopy young plantation" = "Tree plantation", 
       "estuary" = "Water", 
       "bracken" = "Other", 
       "woodland recently felled" = "Woodland", 
       "road & rail" = "Road & rail",
       "ripping" = "Other"
       )


#plot shapefile
p <- ggplot() +
  geom_sf(data=plot_data, mapping=aes(fill=lcs_label2), colour=NA) +
  labs(title="LAND USE IN SCOTLAND", 
       tag="Land Cover Scotland (LCS) 1988 was the\nfirst ever national census of land cover\nin Scotland to describe the principal features\nand characteristics of the countryside.",
       caption="N. Rennie | Data: data.gov.uk") +
  scale_fill_manual(name="", values=stepped2(18)) +
  guides(fill=guide_legend(ncol=3)) +
  theme(plot.background = element_rect(fill = "grey90", colour="grey90"),
        panel.background = element_rect(fill = "grey90", colour="grey90"),
        plot.title = element_text(colour = "#5254a3", size=28, hjust = 0.5, family="serif", face="bold"),
        plot.caption = element_text(colour = "#5254a3", size=12, hjust = 0.5, family="serif"),
        legend.background = element_rect(fill = "transparent", colour="transparent"),
        legend.text = element_text(colour = "#5254a3", size=12, hjust = 0, family="serif"),
        legend.key = element_rect(colour = "grey90"),
        plot.tag.position = c(0, 0.81), 
        plot.tag = element_text(colour = '#5254a3', family="serif", size=12, hjust=0),
        legend.position = "bottom",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.8, 1, 0.3, 1), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p

#save image
ggsave(p, filename="2021/viz/map_17.jpg")

