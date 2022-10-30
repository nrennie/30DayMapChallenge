library(tidyverse)
library(sf)
library(stars)
library(PrettyCols)

# UK Map from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-buc/explore?location=55.216238%2C-3.316413%2C6.38
uk <- sf::st_read("2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") 
scot_sf <- uk %>% 
  select(CTRY21NM, geometry) %>% 
  filter(CTRY21NM == "Scotland")

# rasterize 
scot_raster <- st_rasterize(scot_sf, nx = 100, ny = 100)

# Open a png file
png("2022/maps/day_07.png", width = 6, height = 6, units = "in", res = 300) 

# plot
plot(scot_raster,
     col = prettycols("Dark")[5],
     main = "",
     ylim=c(500000, 1300000),
     breaks = "equal")
title("Scotland", adj = 0.5,
      cex.main = 1.8,
      font.main = 13,
      col.main = "black")

dev.off() 

