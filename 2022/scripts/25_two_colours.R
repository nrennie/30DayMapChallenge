library(tidyverse)
library(sf)
library(biscale)
library(cowplot)
library(showtext)

# Load fonts
font_add_google("Ubuntu", "ubuntu")
font_add_google("Frijole", "frijole")
showtext_auto()

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
uk <- sf::st_read("2022/data/UK_Counties/Counties_and_Unitary_Authorities_(April_2019)_Boundaries_UK_BGC.shp") %>% 
  sf::st_transform(crs = 4277)

# Greggs locations fromhttps://automaticknowledge.org/training/bonusdata/
greggs <- readr::read_csv("2022/data/greggs.csv") 
greggs_sf <- greggs %>% 
  select(address.longitude, address.latitude) %>% 
  rename(lon = address.longitude, 
         lat = address.latitude) %>% 
  sf::st_as_sf(coords = c("lon", "lat")) %>% 
  sf::st_set_crs(4277) %>% 
  sf::st_transform(crs = 4277)

# compute number of greggs per LA (11 with 0!)
intersections <- lengths(st_intersects(uk, greggs_sf))
map_data <- uk %>% 
  mutate(num_greggs = intersections)

# define classes
uk_classes <- bi_class(map_data, x=st_areasha, y=num_greggs, style="quantile", dim=3) %>%
  mutate(bi_class = ifelse(str_detect(bi_class, "NA"), NA, bi_class))

# map
p <- ggplot() +
  geom_sf(data = uk_classes, 
          aes(fill = bi_class),
          show.legend=FALSE,
          colour = "#fafafa",
          linewidth = 0.1) +
  bi_scale_fill(pal="DkBlue", dim=3, na.value="grey50") +
  labs(title = "Density of Greggs in the UK",
       subtitle = str_wrap("There are 11 county and unitary authority areas in the UK without a single branch of Greggs.", 80)) +
  theme(plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 40,
                                  colour = "#3b4895",
                                  family = "frijole"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 24,
                                     colour = "#3b4895",
                                     margin = margin(t = 10),
                                     family = "ubuntu",
                                     lineheight = 0.4),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p

# add legend
p_legend <- bi_legend(pal="DkBlue",
                      dim=3,
                      xlab="County / Unitary Authority Area",
                      ylab="Number of Greggs",
                      size=20) +
  theme(plot.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        panel.background = element_rect(fill = "#fafafa", colour="#fafafa"),
        axis.title = element_text(hjust = 0, size = 30, colour = "#3b4895"))
p_legend

# draw legens
q <- ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(p_legend, 0.05, 0.01, 0.3, 0.3, scale=1)
q

# save
ggsave("2022/maps/day_25.png", height = 6, width = 6, bg = "#fafafa")
