# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)
library(osmdata)
library(sf)
library(ggfx)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-10")
haunted_places <- tuesdata$haunted_places


# Load fonts --------------------------------------------------------------

font_add_google("Nosifer", "nosifer")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()
showtext_opts(dpi = 300)


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"
highlight_col <- "#FF4500"

body_font <- "ubuntu"
title_font <- "nosifer"


# Data wrangling ----------------------------------------------------------

plot_data <- haunted_places |>
  filter(city == "New York City") |>
  drop_na() |>
  select(-c(starts_with("city_"), city, country, state, state_abbrev))

bb <- getbb("New York City, United States")
bb <- matrix(c(-74.1, -73.9, 40.68, 40.85), 2, 2, byrow = TRUE)
colnames(bb) <- c("min", "max")
rownames(bb) <- c("x", "y")

streets <- bb |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway", "primary"
    )
  ) |>
  osmdata_sf()

small_streets <- bb |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "secondary", "tertiary", "residential"
    )
  ) |>
  osmdata_sf()

plot_sf <- plot_data |>
  filter(longitude >= bb["x", "min"] & longitude <= bb["x", "max"]) |>
  filter(latitude >= bb["y", "min"] & latitude <= bb["y", "max"]) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)


# Define text -------------------------------------------------------------

social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:grey90;'>&#xf08c;</span><span style='color:grey10;'>.</span><span style='font-family:ubuntu;color:grey90;'>nicola-rennie</span><span style='color:grey10;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:grey90;'>&#xe671;</span><span style='color:grey10;'>.</span><span style='font-family:ubuntu;color:grey90;'>nrennie</span><span style='color:grey10;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:grey90;'>&#xf09b;</span><span style='color:grey10;'>.</span><span style='font-family:ubuntu;color:grey90;'>nrennie</span>"
title <- "The Ghosts of"
st <- "New York City"
cap <- glue(
  "**Data**: The Shadowlands Haunted Places Index | OpenStreetMap<br>**Graphic**: {social}"
)
tag <- glue(
  "<span style='font-family: {body_font}; font-size: 22pt;'>*{title}*</span><br><span style='font-family: {title_font}; color: {highlight_col}; font-size: 32pt;'>{st}</span><br><br>{cap}"
)


# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_sf(
    data = small_streets$osm_lines,
    color = "#CC3600",
    size = 0.1,
    alpha = 0.5
  ) +
  geom_sf(
    data = streets$osm_lines,
    color = highlight_col,
    size = 0.5
  ) +
  with_outer_glow(
    geom_sf(
      data = plot_sf,
      colour = text_col,
      size = 2,
    ),
    colour = "white",
    sigma = 12,
    expand = 12
  ) +
  labs(
    caption = tag
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 11, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(-30, -80, 0, -70),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 1,
      halign = 1,
      margin = margin(b = 5, t = 5, r = 30),
      lineheight = 1,
      family = body_font
    )
  )


# Annotations -------------------------------------------------------------

p +
  geom_textbox(
    data = plot_sf[2, ],
    mapping = aes(
      x = longitude - 0.06, y = latitude - 0.01,
      label = glue("**{location}**<br>{description}")
    ),
    family = body_font,
    colour = text_col,
    fill = alpha(bg_col, 0.7)
  ) +
  annotate("curve",
    x = -74, xend = -73.985,
    y = 40.765, yend = 40.775,
    arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
    curvature = -0.1,
    colour = text_col,
    linewidth = 0.7
  ) +
  geom_textbox(
    data = plot_sf[1, ],
    mapping = aes(
      x = longitude + 0.05, y = latitude - 0.01,
      label = glue("**{location}**<br>{str_to_sentence(description)}")
    ),
    family = body_font,
    colour = text_col,
    maxwidth = unit(1.5, "in"),
    fill = alpha(bg_col, 0.7)
  ) +
  annotate("curve",
    x = -73.961, xend = -73.975,
    y = 40.71, yend = 40.72,
    arrow = arrow(length = unit(0.1, "inches"), type = "closed"),
    curvature = -0.1,
    colour = text_col,
    linewidth = 0.7
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/01_points.png",
  bg = bg_col, height = 8, width = 6, units = "in"
)
