library(scico)
library(R.utils)
library(tidyverse)
library(sf)
library(showtext)

# load fonts
font_add_google("MedievalSharp", "sharp")
showtext_auto()

# download data from https://data.humdata.org/dataset/kontur-population-germany
gunzip("2022/data/kontur_population_DE_20220630.gpkg.gz", remove=FALSE)
kontur <- st_read("2022/data/kontur_population_DE_20220630.gpkg")

# map
ggplot() +
  geom_sf(data = kontur, 
          aes(fill = population,
              colour = population)) +
  scale_fill_scico(direction = -1, trans = "pseudo_log", palette = "lajolla") +
  scale_color_scico(direction = -1, trans = "pseudo_log", palette = "lajolla") +
  labs(title = "Deutschland") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(colour = "#0B0B45", family = "sharp", size = 60, hjust = 0.5),
        plot.margin = margin(10, 10, 10, 10),
        plot.background = element_rect(fill = "#fafafa", colour = "#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour = "#fafafa")) 


# save
ggsave("2022/maps/day_21.png", height = 6, width = 6, bg = "#fafafa")

