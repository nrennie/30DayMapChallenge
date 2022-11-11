library(tidyverse)
library(sf)
library(rcartocolor)
library(showtext)

# Load fonts
font_add_google("Barrio", "barrio")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()

#Data from https://www.data.gov.uk/dataset/6edd3b1b-16d8-477d-affd-702dd2f13936/geomorphology-of-the-cairngorm-mountains
cairngorm_poly <- sf::st_read("2022/data/cairngorm/CAIRNGORM_GEOMORPH_poly.shp") 

# map
ggplot() +
  geom_sf(data = cairngorm_poly,
          aes(fill = LANDFORM_A),
          colour = "transparent") +
  scale_fill_carto_d(palette = "Antique",
                     breaks = c("Landforms of glacial and glaciofluvial deposition",
                                "Landforms of glacial erosion",
                                "Postglacial and contemporary landforms and processes",
                                "Postglacial active alluvial fan surface",
                                "Relict periglacial landforms",
                                "Other"
                                )) +
  labs(title = "Cairngorms") +
  guides(fill = guide_legend(ncol = 2)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.margin = margin(10, 10, 10, 10),
        legend.title = element_blank(),
        legend.text = element_text(family = "ubuntu", lineheight = 0.5, size = 20),
        plot.title = element_text(hjust = 0.5, size = 100, family = "barrio", margin = margin(b = 30)))

# save
ggsave("2022/maps/day_20.png", height = 6, width = 6, bg = "grey70")
