library(tidyverse)
library(sf)
library(osmdata)

# choose area and get data
bbx <- getbb("Glasgow, UK")
subway <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "railway",
    value = c(
      "subway"
    )
  ) %>%
  osmdata_sf() 

river <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "water",
    value = c(
      "river"
    )
  ) %>%
  osmdata_sf() 

# prep data
plot_data <- subway$osm_lines %>% 
  filter(is.na(service))

points <- plot_data %>% 
  st_coordinates() %>% 
  as_tibble()

river_data <- river$osm_multipolygons %>% 
  filter(name == "River Clyde")

# plot
ggplot() +
  geom_sf(data = river_data[2,],
          size = 0,
          colour = "lightgray") +
  geom_sf(data = plot_data,
          size = 2,
          colour = "#ff6600") +
  geom_text(data = points, 
            mapping = aes(x = mean(X),
                          y = mean(Y),
                          label = str_wrap("Glasgow Subway", 8)),
            size = 8,
            fontface = "italic",
            hjust = 0.2,
            vjust = 0.1) +
  theme_void()


# save
ggsave("2022/maps/day_06.png", height = 6, width = 6, bg = "white")




