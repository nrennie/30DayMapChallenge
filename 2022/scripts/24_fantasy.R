library(tidyverse)
library(sf)
library(showtext)

# load font
font_add_google("MedievalSharp", "sharp")
showtext_auto()

# Shapefiles from https://github.com/jvangeld/ME-GIS
coastline <- sf::st_read("2022/data/ME-GIS/Coastline2.shp") 
forests <- sf::st_read("2022/data/ME-GIS/Forests.shp") 
lakes <- sf::st_read("2022/data/ME-GIS/Lakes.shp") 
rivers <- sf::st_read("2022/data/ME-GIS/Rivers.shp") 
cities <- sf::st_read("2022/data/ME-GIS/Cities.shp") 

# map
ggplot() +
  geom_sf(data = coastline, colour = "#DAA06D", fill = "#EADDCA") +
  geom_sf(data = forests, colour = alpha("#228B22", 0.5), fill = alpha("#228B22", 0.5)) +
  geom_sf(data = lakes, colour = "#5D92B1", fill = alpha("#8DB6CD", 0.8)) +
  geom_sf(data = rivers, colour = alpha("#5D92B1", 0.5), fill = alpha("#5D92B1", 0.5)) +
  geom_sf(data = cities, colour = "gray80", fill = "gray80", size = 3) +
  geom_sf_text(data = filter(cities, TYPE == "City",
                             is.na(ORIGIN),
                             Name != "Minas Morgul",
                             Name != "Minas Tirith"),
               aes(label = Name), colour = "#492500", family = "sharp", size = 12) +
  labs(title = "Middle Earth",
       subtitle = "Data: github.com/jvangeld/ME-GIS") +
  theme_void() +
  theme(plot.title = element_text(hjust = -0.2, family = "sharp", size = 90, colour = "#7B3F00"),
        plot.subtitle = element_text(margin = margin(t = 10), hjust = -0.17, family = "sharp", size = 24, colour = "#7B3F00"),
        plot.margin = margin(10,-100,-20,-15))

# save
ggsave("2022/maps/day_24.png", height = 6, width = 6, bg = "#EADDCA")



       