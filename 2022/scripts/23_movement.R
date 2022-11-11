library(tidyverse)
library(ggmap)
library(osmdata)
library(rcartocolor)
library(rStrava)
library(gganimate)

# data wrangling
strava_data %>% 
  as_tibble() %>% 
  select(-id)

# Define geographic area
getbb("Newcastle, UK")
bb <- matrix(c(-1.78, -1.34, 54.8, 55.1), 
             ncol = 2, 
             nrow = 2,
             byrow = TRUE,
             dimnames = list(c("x", "y"), c("min", "max")))

# Get a background map
bg_map <- get_map(bb,
                  source = "stamen",
                  maptype = "toner-hybrid", 
                  color = "bw")
ggmap(bg_map)

# plot using geom_point()
g <- ggmap(bg_map) +
  geom_point(data = strava_data,
             inherit.aes = FALSE,
             aes(colour = altitude, 
                 x = lng, 
                 y = lat), 
             size = 1) + 
  scale_colour_carto_c(name = "Altitude (m)", palette = "SunsetDark") +
  labs(title = "Great North Run 2022",
       caption = "N. Rennie") +
  theme_void() +
  theme(legend.position = c(0.2, 0.2), 
        axis.title = element_blank(), 
        legend.title = element_text(face = "bold", hjust = 0.5, colour = "white"),
        legend.text = element_text(colour = "white"),
        legend.margin = margin(10, 10, 10, 10),
        legend.background = element_rect(fill = alpha("black", 0.5)),
        plot.title = element_text(colour = "#dc3977",
                                  size = 16,
                                  hjust = 0.5,
                                  margin = margin(b = 10)), 
        plot.caption = element_text(colour = "#dc3977",
                                    size = 10, 
                                    hjust = 0.5,
                                    margin = margin(t = 10)), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))
g

# save
ggsave("2022/maps/day_23.png", height = 6, width = 6, bg = "white")

# animate 
g = g + 
  transition_time(time = time) +
  shadow_mark()
g
animate(g, height = 576, width = 576)
anim_save("2022/maps/day_23.gif")
