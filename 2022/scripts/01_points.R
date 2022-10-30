library(tidyverse)
library(sf)
library(cowplot)
library(grid)

# Greggs locations fromhttps://automaticknowledge.org/training/bonusdata/
greggs <- readr::read_csv("2022/data/greggs.csv") 

# UK Map from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-buc/explore?location=55.216238%2C-3.316413%2C6.38
uk <- sf::st_read("2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") 

# prep data
greggs_sf <- greggs %>% 
  select(address.longitude, address.latitude) %>% 
  rename(lon = address.longitude, 
         lat = address.latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4277) %>% 
  sf::st_transform(crs = 4277)

# plot points
p <- ggplot() +
  geom_sf(data = uk,
          colour = "#00558e",
          fill = "#fab824") +
  geom_sf(data = greggs_sf,
          size = 0.5,
          colour = "#00558e") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#00558e", colour = "#00558e"),
        panel.background = element_rect(fill = "#00558e", colour = "#00558e"),
        plot.margin = unit(c(0.5, 3.5, 0.5, 3.5), units = "cm"))

# add squares
rect1 <- rectGrob(
  x = unit(1, "cm"),
  y = unit(1, "npc") - unit(1, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

rect2 <- rectGrob(
  x = unit(1.8, "cm"),
  y = unit(1, "npc") - unit(1, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

rect3 <- rectGrob(
  x = unit(1, "cm"),
  y = unit(1, "npc") - unit(1.8, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

rect4 <- rectGrob(
  x = unit(1.8, "cm"),
  y = unit(1, "npc") - unit(1.8, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

# draw
ggdraw(p) +
  draw_grob(rect1) +
  draw_grob(rect2) +
  draw_grob(rect3) +
  draw_grob(rect4) +
  draw_label(label = "GREGGS", colour = "white", fontface = "bold", size = 30, x = 0.33, y = 0.89)

# save
ggsave("2022/maps/day_01.png", height = 6, width = 6)

