library(tidyverse)
library(osmdata)
library(sf)

#choose area
bbx <- getbb("Newcastle, UK")

#large roads
roads <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary"
    )
  ) %>%
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

# plot
ggplot() +
  geom_sf(data = roads_new,
          size = 0.6) +
  geom_sf(data = circle, fill = NA, size= 0.1) +
  labs(title = "NEWCASTLE") +
  theme_void() +
    theme(plot.title = element_text(family = "mono", size = 24, hjust = 0.5, margin = margin(t = 10)))

# save
ggsave("2022/maps/day_08.png", height = 6, width = 6, bg = "white")




