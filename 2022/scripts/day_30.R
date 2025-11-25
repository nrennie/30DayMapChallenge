library(leaflet)
library(tidyverse)
library(sf)
library(mapview)

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
uk <- sf::st_read("2022/data/UK_Counties/Counties_and_Unitary_Authorities_(April_2019)_Boundaries_UK_BGC.shp") %>% 
  sf::st_transform(crs = 4326)
wi <- uk %>% 
  filter(ctyua19nm == "Na h-Eileanan Siar")

# distilleries
dists <- tribble(
  ~distillery, ~lat, ~lon,
  "Abhainn Dearg Distillery",   58.17080848054152, -7.044804336988675,
  "Isle of Harris Distillery",   57.897728417665974, -6.80400818792223,
  "North Uist Distillery",  57.457960290993825, -7.396271114922883,
  "Isle of Barra Distillery", 56.95528307519276, -7.4991677400729895,
) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4277) %>% 
  sf::st_transform(crs = 4326)
dists <- cbind(dists, st_coordinates(dists))

# leaflet map
m <- leaflet() %>%
  addTiles() %>% 
  addPolygons(data = wi,
              weight = 1) %>% 
  addMarkers(data = dists,
             label = ~distillery, 
             labelOptions = labelOptions(noHide = T)) %>% 
  addControl("Na h-Eileanan Siar", position = "topright") %>% 
  setView(mean(dists$X), mean(dists$Y), zoom = 8)
m

# save as png
mapshot(m, file = "2022/maps/day_30.png")
