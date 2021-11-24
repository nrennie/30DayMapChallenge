library(rayshader)
library(reshape2)
library(raster)
library(tidyverse)

#Set input directory
os50_height_dir <- "2021/data/os_grid/nn/"

#Load all terrain files in input directory
raster_layers <- list.files(os50_height_dir, "*.asc$")
raster_layers <- tibble(filename = list.files(os50_height_dir, "*.asc$")) %>% 
  mutate(raster =
           map(filename, .f = ~raster::raster(rgdal::readGDAL(paste0(os50_height_dir, .))))
  ) %>% 
  pull(raster)
raster_layers$fun <- mean
raster_mosaic <- do.call(raster::mosaic, raster_layers)
elmat = matrix( raster::extract(raster_mosaic, raster::extent(raster_mosaic), buffer = 1000), nrow = ncol(raster_mosaic), ncol = nrow(raster_mosaic) )

ggscot = elmat %>%
  melt() %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_contour(aes(x = Var1, y = Var2, z = value), color = "black", size=0.2) +
  scale_x_continuous("", expand = c(0, 0)) +
  scale_y_continuous("", expand = c(0, 0)) +
  scale_fill_gradientn("Z", colours = terrain.colors(10)) +
  coord_fixed() +
  labs(caption = "N. Rennie | Data: Ordnance Survey", title = "Ben Nevis & Glen Coe") +
  theme(plot.background = element_rect(fill = "#A0D600", colour="#A0D600"),
        panel.background = element_rect(fill = "#A0D600", colour="#A0D600"),
        legend.position = "none",
        axis.ticks = element_blank(),
        plot.title = element_text(colour = "#2A5A5B", size=14, hjust = 0.5, family="serif", face="bold"),
        plot.caption = element_text(colour = "#2A5A5B", size=10, hjust = 0.5, family="serif"),
        axis.text = element_blank(),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_gg(ggscot, width = 4, height = 4.2, raytrace = FALSE, preview = TRUE) 

