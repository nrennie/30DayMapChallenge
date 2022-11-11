library(tidyverse)
library(sf)
library(showtext)
library(ggrepel)

# load fonts
font_add_google("Uncial Antiqua", "antiqua")
showtext_auto()

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
uk <- sf::st_read("2022/data/UK_Counties/Counties_and_Unitary_Authorities_(April_2019)_Boundaries_UK_BGC.shp") 

# distilleries
dists <- tribble(
  ~distillery, ~lat, ~lon,
  "Abhainn Dearg Distillery",   58.17080848054152, -7.044804336988675,
  "Isle of Harris Distillery",   57.897728417665974, -6.80400818792223,
  "North Uist Distillery",  57.457960290993825, -7.396271114922883,
  "Isle of Barra Distillery", 56.95528307519276, -7.4991677400729895,
) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4277) %>% 
  sf::st_transform(crs = 4277)

dists <- cbind(dists, st_coordinates(dists))

# plot data
wi <- uk %>% 
  filter(ctyua19nm == "Na h-Eileanan Siar")

# plot map
ggplot() +
  geom_sf(data = wi,
          size = 0.1,
          colour = "white",
          fill = "#5c075d") + 
  geom_sf(data = dists,
          size = 3, 
          shape = 21,
          colour = "white",
          fill = "#B5338A") +
  geom_sf_text(data = dists,
               mapping = aes(label = str_wrap(distillery, 10)),
               size = 10, 
               nudge_x = c(-30000, 50000, -30000, 40000),
               fontface = "italic",
               lineheight = 0.5,
               colour = "white") +
  labs(title = "Na h-Eileanan Siar") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#110f3e", colour = "#110f3e"),
        panel.background = element_rect(fill = "#110f3e", colour = "#110f3e"),
        plot.title = element_text(hjust = 0.5, family = "antiqua", size = 40, colour = "white"),
        plot.margin = unit(c(0.5, 3.5, 0.5, 3.5), units = "cm"))

# save
ggsave("2022/maps/day_26.png", height = 6, width = 6)

