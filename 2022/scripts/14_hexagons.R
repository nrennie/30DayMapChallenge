library(tidyverse)
library(geojsonio)
library(usefunc)
library(rcartocolor)

# Download the hexagons here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
us <- geojson_read("2022/data/us_states_hexgrid.geojson",  what = "sp")
us_map <- fortify(us, region="iso3166_2")

# Download data from https://worldpopulationreview.com/state-rankings/life-expectancy-by-state
life_exp <- readr::read_csv("2022/data/usa_life.csv")
life_exp <- life_exp %>% 
  mutate(id = US_name_to_abb(State))

# join data
us_map <- us_map %>% 
  left_join(life_exp, by = "id")

us_map_text <- us_map %>% 
  group_by(id) %>% 
  summarise(long = mean(long), 
            lat = mean(lat),
            overall = mean(overall))
# plot map
ggplot(data = us_map,
       mapping = aes(map_id = id,
                     x = long,
                     y = lat, 
                     fill = overall)) +
  geom_map(map = us_map,
           colour = "white",
           linewidth = 1.5) +
  geom_text(data = us_map_text,
            aes(label = id)) +
  labs(title = "Life Expectancy in the USA") +
  scale_fill_carto_c(name = "", palette = "SunsetDark") +
  coord_map() +
  theme_void() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5,
                                  margin = margin(t = 10), 
                                  size = 20,
                                  face = "italic"))

# save
ggsave("2022/maps/day_14.png", height = 6, width = 6, bg = "white")

