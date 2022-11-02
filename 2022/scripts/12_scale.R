library(tidyverse)
library(sf)
library(PrettyCols)

# UK Map from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-buc/explore?location=55.216238%2C-3.316413%2C6.38
uk <- sf::st_read("2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") %>% 
  filter(CTRY21NM == "Scotland") %>% 
  sf::st_transform(crs = 4326) 

tx <- map_data("state", "texas") %>% 
  mutate(lat = lat + 25, 
         long = long + 95)

# subtitle
st <- "This map of Scotland and Texas shows how the WGS84 coordinate reference system can distort the scale of maps. In reality Texas is around 270,000 square miles, and Scotland only around 30,000. Here, Scotland appears proportionally larger due to the choice of coordinate reference system which makes countries further from the Equator appear larger than they are."

# map
ggplot() +
  geom_polygon(data = tx,
               aes(long, lat, group = group),
               fill = prettycols("Blues", n = 7)[4],
               size = 0.1,
               colour = "#2e4d5d") +
  geom_sf(data = uk,
          size = 0.1,
          fill = prettycols("Blues", n = 7)[1],
          colour = "#2e4d5d") +
  labs(title = "How big is Scotland?", 
       subtitle = str_wrap(st, 60)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(margin = margin(b = 20), colour = "#2e4d5d",
                                  size = 20, face = "bold", family = "serif"),
        plot.subtitle = element_text(hjust = 0, margin = margin(b = 20),
                                     colour = "#2e4d5d",
                                     size = 12,
                                     family = "serif"),
        plot.margin = unit(c(0.5, 2.5, 0.5, 0.5), unit = "cm"))

# save
ggsave("2022/maps/day_12.png", height = 6, width = 6, bg = prettycols("Blues", n = 7)[7])

