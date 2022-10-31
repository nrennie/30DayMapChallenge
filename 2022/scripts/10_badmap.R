library(tidyverse)
library(sf)

# UK Map from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-buc/explore?location=55.216238%2C-3.316413%2C6.38
uk <- sf::st_read("2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") 

# transform coords
uk_new <- uk %>% 
  sf::st_transform(crs = 3348)

# plot
ggplot() +
  geom_sf(data = uk_new, 
          mapping = aes(fill = CTRY21NM),
          size = 2, 
          colour = "yellow") +
  labs(title = "A Bad Map of the United Kingdom") +
  theme(legend.title = element_blank(), 
        legend.position = "bottom", 
        panel.background = element_rect(fill = "blue", colour = "blue"), 
        plot.background = element_rect(fill = "blue", colour = "blue"),
        legend.background = element_rect(fill = "blue"), 
        legend.text = element_text(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.5, margin = margin(t = 10, b = 10), size = 20))

# save
ggsave("2022/maps/day_10.png", height = 6, width = 6)
