library(tidyverse)
library(PrettyCols)

# Map based on https://ourworldindata.org/forest-area
forest <- readr::read_csv("2022/data/forest.csv")

# prep data
plot_data <- forest %>% 
  filter(Year == "2020")

# world map
world <- map_data("world")

# join data
sort(unique(world$region))
sort(unique(plot_data$Entity))
plot_data <- plot_data %>% #old = new
  mutate(Entity = 
           recode(Entity, 
                 "United Kingdom" = "UK", 
                 "United States" = "USA",
                 "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                 "Congo" = "Republic of Congo", 
                 "Czechia" = "Czech Republic"))

map_data <- left_join(world, plot_data, by = c("region" = "Entity"))

# subtitle
st <- str_wrap("Forest area is land under natural or planted stands of trees of at least 5 meters in situ, whether productive or not, and excludes tree stands in agricultural production systems.", 80)

# plot
ggplot(data = map_data,
       mapping = aes(long,
                     lat,
                     map_id = region,
                     fill = `Forest cover`)) +
  geom_map(map = map_data,
           color = "black",
           size = 0.1) +
  scale_fill_pretty_c("Greens", direction = -1, limits = c(0, 100)) +
  labs(title = "Share of land covered by forest, 2020", 
       subtitle = st, 
       caption = "Data: Our World in Data") +
  guides(fill = guide_colorbar(title.position = "top", 
                               title = "Forest area (%)")) +
  theme_void() +
  theme(legend.position = "bottom", 
        legend.title = element_text(hjust = 0.5,
                                    face = "italic",
                                    size = 10),
        plot.title = element_text(face = "italic",
                                  size = 20), 
        plot.subtitle = element_text(face = "italic",
                                     size = 12), 
        plot.caption = element_text(face = "italic",
                                    size = 12), 
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"))

# save
ggsave("2022/maps/day_04.png", height = 6, width = 6, bg = "white")
