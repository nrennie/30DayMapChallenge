library(tidyverse)
library(ggforce)

# https://lco.global/education/activities/how-big-solar-system/
space <- readr::read_csv("2022/data/space.csv")

# prep data
plot_data <- space %>% 
  mutate(x0 = 0, 
         y0 = 0)

set.seed(1234)
pts_df <- data.frame(x = runif(100, -367, 367),
                     y = runif(100, -367, 367))

# plot
ggplot() +
  geom_point(data = pts_df, 
             aes(x = x, y = y),
             color = "white",
             size = 0.1) +
  geom_circle(data = plot_data,
              aes(x0 = x0,
                  y0 = y0,
                  r = cm),
              colour = "white") +
  geom_point(data = plot_data, 
             aes(x = 0, y = 0),
             colour = "yellow") +
  geom_text(data = slice_tail(plot_data, n = 6),
            aes(x = 0, y = cm + 15, label = Planet), 
            colour = "white",
            size = 3,
            fontface = "italic") +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"))

# save
ggsave("2022/maps/day_09.png", height = 6, width = 6)
