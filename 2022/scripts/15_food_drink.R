library(tidyverse)
library(sf)
library(cowplot)
library(grid)
library(nngeo)

# Greggs locations fromhttps://automaticknowledge.org/training/bonusdata/
greggs <- readr::read_csv("2022/data/greggs.csv") 

# prep data
greggs_sf <- greggs %>% 
  select(address.longitude, address.latitude) %>% 
  rename(lon = address.longitude, 
         lat = address.latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4277) %>% 
  sf::st_transform(crs = 4277)

# Nearest neighbours
nn <- st_nn(greggs_sf, greggs_sf, k = 10, progress = F)
greggs_nn <- st_connect(greggs_sf, greggs_sf, ids = nn, progress = F)

# map
p <- ggplot() + 
  geom_sf(data = greggs_nn, 
          size = 0.3,
          colour = "#fab824") + 
  labs(caption = "Every branch of Greggs connected to its 10 nearest neighbors.") +
  theme_void() + 
  theme(plot.background = element_rect(fill = "#00558e", colour = "#00558e"),
        panel.background = element_rect(fill = "#00558e", colour = "#00558e"),
        plot.margin = unit(c(0.5, 3.5, 0.5, 3.5), units = "cm"),
        plot.caption = element_text(hjust = 0.5, colour = "#fab824"))
p

# add squares
rect1 <- rectGrob(
  x = unit(8.5, "cm"),
  y = unit(1, "npc") - unit(3.5, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

rect2 <- rectGrob(
  x = unit(9.3, "cm"),
  y = unit(1, "npc") - unit(3.5, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

rect3 <- rectGrob(
  x = unit(8.5, "cm"),
  y = unit(1, "npc") - unit(4.3, "cm"),
  width = unit(0.5, "cm"),
  height = unit(0.5, "cm"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "#fab824", col = "#fab824")
)

rect4 <- rectGrob(
  x = unit(9.3, "cm"),
  y = unit(1, "npc") - unit(4.3, "cm"),
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
  draw_label(label = "GREGGS", colour = "white", fontface = "bold", size = 30, x = 0.82, y = 0.73)

# save
ggsave("2022/maps/day_15.png", height = 6, width = 6, bg = "#00558e")
