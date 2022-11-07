library(tanaka)
library(elevatr)
library(terra)
library(PrettyCols)

# use elevatr to get elevation data
ras <- get_elev_raster(locations = data.frame(x = c(-3.27, -3.15), y = c(54.43, 54.49)),
                       z = 10, prj = "EPSG:4326", clip = "locations")
ras <- rast(ras)

# Open a png file
png("2022/maps/day_18.png", width = 6, height = 6, units = "in", res = 300) 

# display the map
par(mar = c(4, 2, 4, 2))
tanaka(ras, col = PrettyCols::prettycols("Blues", n = 8, type = "continuous"), legend.pos = "n")
title(main = "Scafell Pike",
      adj = 0.5,
      cex.main = 1.8,
      font.main = 13,
      col.main = "black")
dev.off() 
