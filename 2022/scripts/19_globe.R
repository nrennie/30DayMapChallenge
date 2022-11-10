library(tidyverse)

world <- map_data("world")

ggplot(data = world,
       aes(long, lat)) +
  geom_map(map = world, aes(map_id = region), fill = "grey50") +
  coord_map("gilbert",
            xlim = c(-180, 180),
            ylim = c(-180, 180)) +
  labs(title = "Earth") +
  scale_x_continuous(breaks = seq(-180, 180, 15)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = alpha("grey80", 0.5)),
        panel.grid.minor = element_line(colour = "grey80"),
        plot.background = element_rect(fill = "grey10", color = NA),
        plot.title = element_text(colour = "grey80", size = 30, margin = margin(b = 10), hjust = 0.5, face = "italic"),
        plot.margin = margin(20, 20, 20, 20))

# save
ggsave("2022/maps/day_19.png", height = 6, width = 6, bg = "grey10")

### ----------------------------------------------------------------------------

ggplot(data = world,
       aes(long, lat)) +
  geom_map(map = world, aes(map_id = region), fill = "grey50") +
  coord_map("aitoff",
            xlim = c(-180, 180),
            ylim = c(-180, 180)) +
  labs(title = "Earth") +
  scale_x_continuous(breaks = seq(-180, 180, 15)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = alpha("grey80", 0.5)),
        panel.grid.minor = element_line(colour = "grey80"),
        plot.background = element_rect(fill = "grey10", color = NA),
        plot.title = element_text(colour = "grey80", size = 30, hjust = 0.5, face = "italic"),
        plot.margin = margin(20, 0, 20, 0))

# save
ggsave("2022/maps/day_19_aitoff.png", height = 6, width = 6, bg = "grey10")
