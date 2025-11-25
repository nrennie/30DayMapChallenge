library(leaflet)
library(htmlwidgets)
library(sf)
library(dplyr)
library(MapColoring)
library(CEoptim)
library(rcartocolor)

s_file <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "GLASGOW_CITY")
sf1 <- st_transform(s_file, crs=4326)

#coloring
map1 <- as(sf1, "Spatial")
opt.colors <- getOptimalContrast(x=map1, col=carto_pal(7, "SunsetDark"))
sf1$opt_col <- opt.colors

#leaflet
m <- leaflet(sf1) %>%
  addTiles() %>%
  addMarkers(lng=-4.2518, lat=55.8642, popup="Glasgow") %>%
  addPolygons(color="black", 
              fillColor = opt.colors,
              fillOpacity=0.5,
              weight=1, 
              highlightOptions = highlightOptions(
                weight = 1.1,
                opacity=1,
                color = "black",
                fillOpacity = 0.8,
                bringToFront = TRUE)) 
m 

saveWidget(m, "map_25.html", selfcontained = T)
