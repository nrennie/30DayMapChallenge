library(tidyverse)
library(osmdata)
library(sf)

#choose area
bbx <- getbb("Newcastle, UK")

# large roads
roads <- bbx %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary",
                            "secondary", "tertiary", "motorway_link",
                            "trunk_link", "primary_link", "secondary_link",
                            "tertiary_link")) %>%
  osmdata_sf()

# small roads
streets <- bbx %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "service",
                            "unclassified", "pedestrian", "footway",
                            "track", "path")) %>%
  osmdata_sf()

# draw a circle
centre = c(long = mean(bbx[1,]), lat = mean(bbx[2,]))
centre_proj <-
  tibble(lat = centre["lat"], long = centre["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)
circle <- tibble(lat = centre["lat"], long = centre["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = 4277) %>%
  st_buffer(dist = 3500, nQuadSegs = 200) %>%
  st_transform(crs = 4326)

# intersection
roads_new <- st_intersection(circle, roads$osm_lines)
streets_new <- st_intersection(circle, streets$osm_lines)

# plot
ggplot() +
  geom_sf(data = roads_new,
          size = 0.6, 
          colour = alpha("black", 0.9)) +
  geom_sf(data = streets_new,
          size = 0.6, 
          colour = alpha("black", 0.5)) +
  labs(title = "NEWCASTLE",
       caption = "N. Rennie | Data: {osmdata}") +
  theme_void() +
    theme(plot.title = element_text(family = "mono", size = 24, hjust = 0.5, margin = margin(t = 10)),
          plot.caption = element_text(family = "mono", size = 10, hjust = 0.5, margin = margin(b = 10)))

# save
ggsave("2022/maps/day_08.png", height = 6, width = 6, bg = "white")




