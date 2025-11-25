library(tidyverse)
library(sf)
library(tmap)
library(viridis)

# get data
sa <- rnaturalearth::ne_countries(scale = "medium", continent = "south america", returnclass = "sf") 

# Open a png file
png("2022/maps/day_03.png", width = 6, height = 6, units = "in", res = 300) 

# plot
tm_shape(sa) +
  tm_polygons(col = "gdp_md_est",
              style = "cont",
              pal = viridis(3, direction = -1),
              title = "Estimated GDP") +
  tm_style("cobalt") +
  tm_layout(
    title = "South America",
    frame = FALSE,
    title.position = c("left", "center"),
    legend.position = c(0.02, 0.3)
  )

dev.off() 
