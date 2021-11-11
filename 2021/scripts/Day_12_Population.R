library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)
library(geofacet)
library(cowplot)
library(patchwork)

#read in shapefile 
s_file <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "GLASGOW_CITY")

#read population data
d <- read_csv("2021/data/population_wards/electoral-wards-20-tabs_2001.csv", 
                   skip = 5) %>%
  filter(`...3` == "Persons", 
         .[[1]] %in% unique(s_file$CODE)) %>%
  mutate(year = 2001)
other_years <- 2002:2020
for (i in 1:length(other_years)){
  f_name <- paste("2021/data/population_wards/electoral-wards-20-tabs_",other_years[i],".csv", sep="")
  d_i <- read_csv(f_name, 
                  skip = 5) %>%
    filter(`...3` == "Persons", 
           .[[1]] %in% unique(s_file$CODE)) %>%
    mutate(year = other_years[i])
  d <- rbind(d, d_i)
}
colnames(d) <- c("CODE", "name", "Sex", "All", 0:90, "Year")

#tidy data
plot_data <- d[,c(1,2,5:96)] %>% 
  mutate(`0`=as.numeric(`0`), 
         `90`=as.numeric(`90`),
         `61`=as.numeric(`61`),
         `31`=as.numeric(`31`)) %>%
  pivot_longer(cols=3:93, names_to = "age", values_to="pop")

#make heatmap and facet
p <- ggplot(plot_data, aes(x=Year, y=as.numeric(age), fill= pop)) + 
  geom_tile() +
  coord_cartesian(expand=F) +
  scale_fill_distiller(palette = "RdPu") +
  facet_wrap(~name) +
  labs(x="", y="Age")
p

#create grid
glasgow_wards_grid <- data.frame(row=c(3, 2, 3, 1, 4, 2, 1, 2, 2, 3, 5, 2, 5, 6, 1, 5, 1, 2, 4, 3, 4, 1, 2), 
           col=c(4, 7, 5, 4, 2, 5, 1, 6, 1, 3, 2, 4, 4, 4, 3, 3, 7, 3, 3, 6, 4, 5, 2),
           code=c("S13002976", "S13002986", "S13002975", "S13002982", "S13002970",
                  "S13002988", "S13002980", "S13002984", "S13002979", "S13002971", 
                  "S13002969", "S13002977", "S13002973", "S13002967", "S13002981",
                  "S13002968", "S13003133", "S13002989", "S13002972", "S13002985", 
                  "S13002974", "S13002983", "S13002978"),
           name=c("Anderston/City/Yorkhill", "Baillieston", "Calton", "Canal", "Cardonald", 
                  "Dennistoun", "Drumchapel/Anniesland", "East Centre", 
                  "Garscadden/Scotstounhill", "Govan", "Greater Pollok", "Hillhead", 
                  "Langside", "Linn", "Maryhill", "Newlands/Auldburn", 
                  "North East", "Partick East/Kelvindale", "Pollokshields", "Shettleston", 
                  "Southside Central", "Springburn/Robroyston", "Victoria Park"))
grid_preview(glasgow_wards_grid)


#plot grid
p <- ggplot(plot_data, aes(x=Year, y=as.numeric(age), fill= pop)) + 
  geom_tile() +
  coord_cartesian(expand=F) +
  scale_fill_distiller(name="Population", palette = "RdPu") +
  facet_geo(~ name, grid = glasgow_wards_grid) +
  labs(x="", y="", title="Population of Glasgow", 
       tag = "As of 2020, the most populous area of Glasgow\nis the Anderston/City/Yorkhill region with\n35,735 people living there. Most population\ngrowth has occured in those under 25.",
       caption="N. Rennie | Data: www.nrscotland.gov.uk") + 
  theme(plot.background = element_rect(fill = "gray97", color = NA),
        panel.background = element_rect(fill = "gray97", colour="gray97"),
        plot.title = element_text(colour="#ae217e", face="bold", family="serif", size=28, hjust=0.5),
        plot.caption = element_text(colour = '#ae217e', family="serif", size=12, hjust=1),
        strip.background =element_rect(fill="gray97"),
        strip.text = element_text(colour = '#ae217e', family="serif", size=12, hjust=0.5),
        legend.text = element_text(colour = '#ae217e', family="serif", size=12, hjust=0.5),
        legend.title = element_text(colour = '#ae217e', family="serif", size=12, hjust=0.5),
        legend.position = c(0.65, 0.25),
        legend.background = element_rect(fill = "gray97", colour="gray97"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(colour = '#ae217e', family="serif", size=12, hjust=0.5),
        plot.margin = unit(c(0.9, 0.5, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.tag.position = c(.85, .25), 
        plot.tag = element_text(colour = '#ae217e', family="serif", size=12, hjust=0.5))
p

#save image
ggsave(p, filename="2021/viz/map_12.jpg")

#submit grid
#grid_submit(glasgow_wards_grid, name = "glasgow_wards_grid", desc = "A grid for the 23 electoral wards in Glasgow, Scotland.")
