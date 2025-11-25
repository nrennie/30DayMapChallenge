library(tidyverse)
library(sf)
library(osmdata)

# https://data.london.gov.uk/dataset/tfl-bus-stop-locations-and-routes
buses <- readr::read_csv("2022/data/bus-stops.csv")
plot_data <- buses %>% 
  select(Location_Northing, Location_Easting) %>% 
  drop_na() %>% 
  st_as_sf(coords = c("Location_Easting", "Location_Northing"), crs = 27700) %>%
  st_transform(4326) 

# Street data
bbx <- getbb("London, UK")

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

# map
ggplot() +
  geom_sf(data = highways$osm_lines, size = 0.1, colour = "#A9A9A9") +
  geom_sf(data = plot_data, colour = "#C41E3A", size = 0.1) +
  labs(title = "London Bus Stops",
       caption = "N. Rennie | Data: data.london.gov.uk | #30DayMapChallenge") +
  theme_void() +
  theme(plot.title = element_text(family = "serif", colour = "#C41E3A", size = 24, hjust = 0.5, margin = margin(t = 20)),
        plot.caption = element_text(family = "serif", colour = "#C41E3A", size = 8, hjust = 0.5, margin = margin(b = 10)),
        plot.background = element_rect(colour = "#fafafa", fill = "#fafafa"),
        panel.background = element_rect(colour = "#fafafa", fill = "#fafafa"))

# save
ggsave("2022/maps/day_11.png", height = 6, width = 6, bg = "#fafafa")










