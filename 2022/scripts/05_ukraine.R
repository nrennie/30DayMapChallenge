library(tidyverse)
library(sf)
library(rnaturalearth)

# get background map
ukraine <- rnaturalearth::ne_countries(scale = "large", country = "Ukraine", returnclass = "sf")

# kyiv 
kyiv <- data.frame(x = 30.5234, y = 50.4501) %>% 
  sf::st_as_sf(coords = c("x", "y")) %>% 
  sf::st_set_crs(4326)

# map
ggplot() +
  geom_sf(data = ukraine, 
          fill = "#fef100", 
          colour = "#017cc2") +
  geom_sf(data = kyiv, 
          colour = "#017cc2", 
          size = 2) +
  geom_sf_text(data = kyiv, 
               mapping = aes(label = "Kyiv"), 
               nudge_x = 1, 
               size = 5, 
               colour = "#017cc2",
               fontface = "italic") +
  labs(title = "Ukraine") +
  theme_void() +
  theme(plot.title = element_text(colour = "#fef100", hjust = 0.5, face = "italic", size = 30))

# save
ggsave("2022/maps/day_05.png", height = 6, width = 6, bg = "#017cc2")


# Updated Map -------------------------------------------------------------

# UK Map from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-buc/explore?location=55.216238%2C-3.316413%2C6.38
ukraine <- sf::st_read("2022/data/Ukraine/ukr_admbnda_adm0_sspe_20221005.shp") 

# kyiv 
kyiv <- data.frame(x = 30.5234, y = 50.4501) %>% 
  sf::st_as_sf(coords = c("x", "y")) %>% 
  sf::st_set_crs(4326)

# map
ggplot() +
  geom_sf(data = ukraine, 
          fill = "#fef100", 
          colour = "#017cc2") +
  geom_sf(data = kyiv, 
          colour = "#017cc2", 
          size = 2) +
  geom_sf_text(data = kyiv, 
               mapping = aes(label = "Kyiv"), 
               nudge_x = 1, 
               size = 5, 
               colour = "#017cc2",
               fontface = "italic") +
  labs(title = "Ukraine") +
  theme_void() +
  theme(plot.title = element_text(colour = "#fef100", hjust = 0.5, face = "italic", size = 30))

# save
ggsave("2022/maps/day_05.png", height = 6, width = 6, bg = "#017cc2")



