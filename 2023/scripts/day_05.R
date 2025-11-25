# Inspired by https://charts.substack.com/p/typewriter-chartography
# Blog at https://nrennie.rbind.io/blog/creating-typewriter-maps-r/

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(sf)
library(nrBrand)
library(showtext)
library(rgeoboundaries)


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
showtext_auto()


# Prep data ---------------------------------------------------------------

# Get boundaries data
italy <- geoboundaries("Italy")
st_crs(italy) = 4326
italy_sf <- italy |> 
  sf::st_transform(crs = 3003)

# Get elevation data
elev_data <- elevatr::get_elev_raster(locations = italy_sf,
                                      z = 3,
                                      clip = "locations")


# Parameters --------------------------------------------------------------

title <- "ITALY"
chars <- c("l", "I", "H", "M")
size <- 3.3
text_size <- 22
bg_col <- "gray95"
text_col <- "grey10"

# prep text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = "elite"
)


# Plot --------------------------------------------------------------------

# prep data
elev_mat <- terra::as.matrix(elev_data, wide = TRUE)
colnames(elev_mat) <- 1:ncol(elev_mat)
elev_df <- elev_mat |> 
  as_tibble() |> 
  mutate(y = row_number()) |> 
  pivot_longer(-y, names_to = "x") |> 
  mutate(x = as.numeric(x))
# characters to use
chars_map <- data.frame(value = seq_len(length(chars)),
                        value_letter = chars)
elev_plot <- elev_df |> 
  mutate(value = ntile(value, n = length(chars))) |> 
  left_join(chars_map, by = "value") |> 
  drop_na()

# plot
g <- ggplot() +
  geom_text(data = elev_plot, 
            mapping = aes(x = x, y = y, label = value_letter),
            family = "elite",
            colour = text_col,
            size = size) +
  labs(title = title,
       caption = social) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void(base_size = text_size) +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_text(family = "elite",
                                  size = text_size*3,
                                  face = "bold",
                                  hjust = 0.8,
                                  colour = text_col,
                                  margin = margin(t = 20, b = -10)),
        plot.caption = element_textbox_simple(
          colour = text_col,
          lineheight = 0.5,
          family = "elite",
          halign = 0.5,
          hjust = 0.5,
          margin = margin(b = 5, t = 15)
        ))

# save
ggsave("2023/maps/05_analog.png",
       plot = g,
       width = 5,
       height = 7,
       bg = "gray95",
       unit = "in")
