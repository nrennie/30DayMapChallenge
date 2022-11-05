library(tidyverse)
library(sf)
library(osmdata)
library(camcorder)

# start recording
gg_record(
  dir = file.path("2022", "images", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 9, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# Data collection ---------------------------------------------------------

# choose area 
bbx <- getbb("Glasgow, UK")

# track lines
subway <- bbx %>%
  opq() %>%
  add_osm_feature(key = "railway", value = "subway") %>%
  osmdata_sf() 
subway_data <- subway$osm_lines %>% 
  filter(is.na(service))

# subway stops
stations <- bbx %>% 
  opq() %>% 
  add_osm_feature(key = "public_transport", value = "station") %>% 
  osmdata_sf() 
stations_data <- stations$osm_points %>% 
  filter(network == "Glasgow Subway")

# river data
river <- bbx %>%
  opq() %>%
  add_osm_feature(key = "water", value = "river") %>%
  osmdata_sf() 
river_data <- river$osm_multipolygons %>% 
  filter(name == "River Clyde")

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


# Make a circle -----------------------------------------------------------

points <- stations_data %>% 
  st_coordinates() %>% 
  as_tibble()
crs2 <- 6384 
center = c(long = mean(points$X), lat = mean(points$Y))
center_proj <-
  tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)
dist <-  3000
circle <- tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)
roads_lines <- st_intersection(circle, roads$osm_lines)
streets_lines <- st_intersection(circle, streets$osm_lines)
river_poly <- st_intersection(circle, river_data)

# Make map ----------------------------------------------------------------

# plot
ggplot() +
  geom_sf(data = roads_lines,
          size = 0.2,
          colour = alpha("lightgray", 0.9)) +
  geom_sf(data = streets_lines,
          size = 0.2,
          colour = alpha("lightgray", 0.7)) +
  geom_sf(data = river_poly,
          size = 0,
          colour = "#abb1ba") +
  geom_sf(data = subway_data,
          size = 2,
          colour = "#ff6600") +
  geom_sf(data = stations_data,
          size = 6,
          colour = "#ff6600") +
  geom_sf(data = stations_data,
          size = 3,
          colour = "#555556") +
  geom_sf_text(data = stations_data,
               aes(label = str_wrap(name, 10)),
               size = 3,
               nudge_x = c(-0.003, 0, 0, -0.003, 
                           -0.007, 0.007, 0.007, 0.004,
                           0.003, 0, 0, 0,
                           -0.007, -0.003, 0.003),
               nudge_y = c(0.003, 0.003, 0.003, -0.003,
                           0, 0, 0, -0.003,
                           0.003, -0.003, -0.003, -0.003,
                           0, -0.003, 0.003),
               colour = "black") +
  labs(title = "Glasgow Subway",
       caption = "N. Rennie | Data: {osmdata}") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(colour = "#555556", hjust = 0.5, size = 40, margin = margin(b = 20)),
        plot.caption = element_text(colour = "#555556", hjust = 0.5, size = 12))

# save
ggsave("2022/images/Glasgow_Subway.png", height = 9, width = 6, bg = "white")


# save gif
gg_playback(
  name = file.path("2022", "images","Glasgow_Subway.gif"),
  first_image_duration = 4,
  last_image_duration = 12,
  frame_duration = .25
)

