library(tidyverse)
library(osmdata)
library(sf)
library(cowplot)

#choose area
bbx <- getbb("Glasgow, UK")

#large roads
highways <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link"
    )
  ) %>%
  osmdata_sf()

#small roads
streets <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path"
    )
  ) %>%
  osmdata_sf()

#choose circle
crs2 <- 6384 
center = c(long = -4.2518, lat = 55.8642)
center_proj <-
  tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

dist <-  3500
circle <- tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

streets_lines <- st_intersection(circle, streets$osm_lines)
highways_lines <- st_intersection(circle, highways$osm_lines)

#Make plot
p <- ggplot() +
  geom_sf(
    data = streets_lines,
    col = "gray40",
    size = .4,
    alpha = .65
  ) +
  geom_sf(
    data = highways_lines,
    col = "gray35",
    size = .6,
    alpha = .8
  ) +
  geom_sf(data = circle, color = "gray35", fill = NA) +
  theme(plot.background = element_rect(fill = "gray15", color = NA),
        panel.background = element_rect(fill = "gray15", colour="gray15"),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 5, 0.5), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p

#fix background and add title
q <- ggdraw(p) + 
  draw_label(x=0.5, y=0.03, hjust=0.5, 
             "N. Rennie | Data: OpenStreetMap", color = "gray85", size = 12, fontfamily="serif") +
  draw_label(x=0.5, y=0.2, hjust=0.5, 
             "GLASGOW", color = "gray85", size = 28, fontface="bold", fontfamily="serif") +
  draw_label(x=0.5, y=0.1, hjust=0.5, 
             "55.8642° N, 4.2518° W", color = "gray85", size = 20, fontfamily="serif") +
  theme(panel.background = element_rect(fill = "gray15", colour = "gray15"))
q

#save image
ggsave(q, filename="2021/viz/map_09.jpg")
