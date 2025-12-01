# Inspired by https://charts.substack.com/p/typewriter-chartography


# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(nrBrand)
library(rnaturalearth)
library(showtext)


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
showtext_auto()


# Function to format data and plot ----------------------------------------

make_map <- function(elev_data,
                     title,
                     chars = c("l", "I", "H", "M"),
                     size = 4,
                     text_size = 22,
                     bg_col = "#fafafa",
                     text_col = "grey10",
                     caption = TRUE) {
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
    labs(title = title) +
    scale_y_reverse() +
    coord_fixed() +
    theme_void(base_size = text_size) +
    theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          plot.margin = margin(10, 10, 10, 10),
          plot.title = element_text(family = "elite",
                                    size = text_size*3,
                                    face = "bold",
                                    colour = text_col,
                                    margin = margin(t = 20, b = -20)),
          plot.caption = element_textbox_simple(
            colour = text_col,
            lineheight = 0.5,
            family = "elite",
            halign = 0.5,
            hjust = 0.5,
            margin = margin(b = 5, t = 15)
          ))
  if (caption) {
    social <- nrBrand::social_caption(
      bg_colour = bg_col,
      icon_colour = text_col,
      font_colour = text_col,
      font_family = "elite"
    )
    g <- g + labs(caption = social)
  }
  return(g)
}



# Plot Australia ----------------------------------------------------------

aus <- ne_countries(
  country = "Australia"
)
elev_data <- elevatr::get_elev_raster(locations = aus,
                                      z = 3,
                                      clip = "locations")
# create map and save
make_map(elev_data, title = "AUSTRALIA", text_size = 8, size = 0.4)

ggsave(
  filename = "2025/maps/day_29.png",
  bg = "#FAFAFA", height = 6, width = 7, units = "in"
)
