# Set up ------------------------------------------------------------------

library(glue)
library(tidyverse)
library(camcorder)
library(nrBrand)
library(ggtext)
library(osmdata)
library(sf)


# Fonts -------------------------------------------------------------------

sysfonts::font_add_google("Carter One", "Carter")
sysfonts::font_add_google("Ubuntu", "ubuntu")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  height = 5.5,
  width = 4.25,
  units = "in",
  dpi = 300
)


# Parameters --------------------------------------------------------------

dist <- 500
x_nudge <- 0
y_nudge <- 0
local_crs <- 27700
font_size <- 12
body_font <- "ubuntu"
title_font <- "Carter"
bg_col <- "#741E6D"
text_col <- "grey98"
circle_col <- "grey98"


# Data --------------------------------------------------------------------

center <- c(long = -2.7973 + x_nudge, lat = 56.3415 + y_nudge)
center_proj <-
  tibble::tibble(lat = center["lat"], long = center["long"]) |>
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
circle <- tibble::tibble(lat = center["lat"], long = center["long"]) |>
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  sf::st_transform(crs = local_crs) |>
  sf::st_buffer(dist = dist) |>
  sf::st_transform(crs = 4326)
circle_bbx <- matrix(sf::st_bbox(circle), nrow = 2, ncol = 2, byrow = FALSE)
rownames(circle_bbx) <- c("x", "y")
colnames(circle_bbx) <- c("min", "max")
bbox <- sf::st_bbox(c(
  xmin = circle_bbx[1, 1],
  ymin = circle_bbx[2, 1],
  xmax = circle_bbx[1, 2],
  ymax = circle_bbx[2, 2]
))
bbox_sf <- sf::st_as_sfc(bbox, crs = 4326)

highways <- circle_bbx |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "primary",
      "secondary", "tertiary", "residential",
      "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path",
      "sidewalk"
    )
  ) |>
  osmdata_sf()

buildings <- circle_bbx |>
  opq() |>
  add_osm_feature(
    key = "building"
  ) |>
  osmdata_sf()

grass <- circle_bbx |> 
  opq() |>
  add_osm_feature(
    key = "natural"
  ) |>
  osmdata_sf()

water <- circle_bbx |> 
  opq() |>
  add_osm_feature(
    key = "waterway"
  ) |>
  osmdata_sf()

paths_cropped <- suppressWarnings(
  sf::st_intersection(circle, highways$osm_lines |> filter(highway != "primary"))
)
roads_cropped <- suppressWarnings(
  sf::st_intersection(circle, highways$osm_lines |> filter(highway == "primary"))
)
bp_cropped <- suppressWarnings(
  sf::st_intersection(circle, buildings$osm_polygons)
)
beach_cropped <- suppressWarnings(
  sf::st_intersection(circle, grass$osm_polygons |> filter(natural == "beach"))
)
grass_cropped <- suppressWarnings(
  sf::st_intersection(circle, grass$osm_polygons |> filter(natural == "wood"))
)
golf_cropped <- suppressWarnings(
  sf::st_intersection(circle, grass$osm_multipolygons)
)
water_cropped <- suppressWarnings(
  sf::st_intersection(circle, water$osm_points)
)

# https://geoportal.statistics.gov.uk/datasets/d4f6b6bdf58a45b093c0c189bdf92e9d_0/explore?filters=eyJDVFJZMjRDRCI6WyJTOTIwMDAwMDMiXX0%3D
scotland <- read_sf("2025/data/Countries_December_2024_Boundaries_UK_BFC_-6467230212120045634/CTRY_DEC_2024_UK_BFC.shp") |> 
  sf::st_transform(crs = 4326)
scot_cropped <- suppressWarnings(
  sf::st_intersection(circle, scotland)
) 


# Text --------------------------------------------------------------------

social <- social_caption(
  mastodon = NA,
  linkedin = NA,
  bluesky = NA,
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
cap <- glue(
  "**Data**: OpenStreetMap | **Graphic**: {social}"
)
title <- "St Andrews"
subtitle <- c("56.3415° N, 2.7973° W")


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = circle,
    fill = "#6EB8D8",
    colour = "#6EB8D8",
    linewidth = 1.5
  ) +
  geom_sf(
    data = scot_cropped,
    fill = circle_col,
    colour = circle_col
  ) +
  geom_sf(
    data = beach_cropped,
    fill = "#FECE85",
    colour = "#FECE85"
  ) +
  geom_sf(
    data = grass_cropped,
    fill = "#77AA74",
    colour = "#77AA74"
  ) +
  geom_sf(
    data = golf_cropped,
    fill = "#60985D",
    colour = "#60985D"
  ) +
  geom_sf(
    data = water_cropped,
    fill = "#6EB8D8",
    colour = "#6EB8D8",
    linewidth = 1.5
  ) +
  geom_sf(
    data = paths_cropped,
    col = "grey40",
    linewidth = 0.5
  ) +
  geom_sf(
    data = roads_cropped,
    col = "grey25",
    linewidth = 1
  ) +
  geom_sf(
    data = bp_cropped,
    fill = "grey60",
    colour = "grey60"
  ) +
  geom_sf(
    data = circle,
    fill = "transparent",
    colour = "#000066",
    linewidth = 1.5
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  coord_sf() +
  labs(title = str_to_upper(title), subtitle = subtitle, caption = cap) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_textbox_simple(
      halign = 0.5, hjust = 0.5,
      colour = text_col,
      family = title_font,
      margin = margin(t = 10),
      size = rel(1.9)
    ),
    plot.subtitle = element_textbox_simple(
      halign = 0.5, hjust = 0.5,
      colour = text_col,
      family = body_font,
      margin = margin(t = 5, b = 5)
    ),
    plot.caption = element_textbox_simple(
      halign = 0.5, hjust = 0.5,
      colour = text_col,
      family = body_font,
      margin = margin(t = 10)
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/14_openstreetmap.png",
  bg = bg_col, height = 5.5,
  width = 4.25, units = "in"
)
