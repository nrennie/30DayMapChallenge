library(tidyverse)
library(showtext)
library(camcorder)
library(ggforce)
library(geofacet)
library(ggtext)
library(rcartocolor)

# load fonts
font_add_google("Raleway", "raleway")
font_add_google("Kelly Slab", "kelly")
showtext_auto()

# load data
state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')
station_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/station_info.csv')

# data wrangling
plot_data <- state_stations %>% 
  select(state, format) %>% 
  group_by(state) %>% 
  mutate(n = n()) %>% 
  filter(str_detect(format, pattern = "News")) %>% 
  mutate(n_rel = n()) %>% 
  select(-format) %>% 
  distinct() %>% 
  mutate(prop = 100*n_rel/n) %>% 
  select(state, prop) %>% 
  mutate(state = str_replace(state, "_", " ")) %>% 
  left_join(geofacet::us_state_grid2, by = c("state" = "name"))

# subtitle
st <- "Ohio has a significantly smaller proportion of radio stations whose format description<br>contains the word *news*, in comparison to other US states."

# plot
ggplot(data = plot_data) +
  geom_circle(mapping = aes(x0 = col + 0,
                            y0 = row + 0,
                            r = 0.4,
                            fill = prop),
              size = 0.2) +
  geom_arc_bar(mapping = aes(x0 = col + 0,
                             y0 = row + 0,
                             r0 = 0,
                             r = 0.4,
                             start = 0,
                             end = (2*pi*0.01)*prop),
               fill = "black",
               size = 0.2) +
  geom_spoke(mapping = aes(x = col + 0, 
                           y = row + 0,
                           angle = (pi/2) - (2*pi*0.01)*prop),
             radius = 0.4,
             colour = "white",
             linewidth = 0.5) +
  geom_text(mapping = aes(x = col + 0,
                          y = row + 0.2,
                          label = code,
                          colour = (code %in% c("UT", "MN"))), 
            fontface = "bold",
            family = "kelly",
            size = 8) +
  scale_fill_carto_c(palette = "SunsetDark",
                     limits = c(0, 15),
                     breaks = c(0, 5, 10, 15),
                     name = str_wrap("Proportion of radio stations in a news format", 8)) +
  scale_colour_manual(values = c("black", "white"), guide = "none") +
  guides(fill = guide_colourbar(ticks.colour = "#eadeca",
                                title.position = "left")) +
  scale_y_reverse() +
  coord_fixed() +
  labs(title = "News on the radio in the USA", 
       subtitle = st,
       caption = "N. Rennie | Data: Wikipedia") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#eadeca", colour = "#eadeca"),
        panel.background = element_rect(fill = "#eadeca", colour = "#eadeca"),
        legend.title = element_text(hjust = 0.5, lineheight = 0.4, vjust = 0.5, margin = margin(b = 0), family = "raleway", size = 24),
        legend.text = element_text(hjust = 0.5, margin = margin(b = 0), family = "raleway", size = 24),
        legend.position = c(0.85, 0.15),
        plot.title = element_text(hjust = 0.5, margin = margin(t = 0), family = "kelly", size = 60),
        plot.subtitle = element_markdown(hjust = 0.5, margin = margin(t = 10), family = "raleway", size = 30, lineheight = 0.5),
        plot.caption = element_text(hjust = 0.5, margin = margin(b = 0), family = "raleway", size = 30))

# save
ggsave("2022/maps/day_27.png", height = 6, width = 6, bg = "#eadeca")
