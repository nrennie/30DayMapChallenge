library(tidyverse)
library(sf)
library(showtext)

# Load fonts
font_add_google("Ubuntu", "ubuntu")
font_add_google("Frijole", "frijole")
showtext_auto()

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
uk <- sf::st_read("2022/data/UK_Counties/Counties_and_Unitary_Authorities_(April_2019)_Boundaries_UK_BGC.shp") %>% 
  sf::st_transform(crs = 4277)

# Greggs locations fromhttps://automaticknowledge.org/training/bonusdata/
greggs <- readr::read_csv("2022/data/greggs.csv") 
greggs_sf <- greggs %>% 
  select(address.longitude, address.latitude) %>% 
  rename(lon = address.longitude, 
         lat = address.latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4277) %>% 
  sf::st_transform(crs = 4277)

# compute number of greggs per LA (11 with 0!)
intersections <- lengths(st_intersects(uk, greggs_sf))
map_data <- uk %>% 
  mutate(num_greggs = intersections)

ggplot() +
  geom_sf(data = map_data, 
          aes(fill = (num_greggs == 0)),
          show.legend=FALSE,
          colour = "#fab824",
          linewidth = 0.1) +
  labs(title = "(No) Greggs in the UK",
       subtitle = str_wrap("There are 11 county and unitary authority areas in the UK without a single branch of Greggs.", 80)) +
  scale_fill_manual(values = c("#00558e", "#fab824")) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 40,
                                  colour = "#fab824",
                                  family = "frijole"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 24,
                                     colour = "#fab824",
                                     margin = margin(t = 10),
                                     family = "ubuntu",
                                     lineheight = 0.4))

# save
ggsave("2022/maps/day_22.png", height = 6, width = 6, bg = "#00558e")
