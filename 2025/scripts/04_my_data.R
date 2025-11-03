library(tidyverse)
library(gpx)
library(sf)
library(camcorder)
library(ggtext)
library(showtext)
library(glue)


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
showtext_auto()
showtext_opts(dpi = 300)


# Params ------------------------------------------------------------------

bg_col <- "#33658A"
line_col <- "grey90"
text_col <- "grey10"
body_font <- "Oswald"


# Start recording ---------------------------------------------------------

gg_record(
  device = "png",
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)


# Data --------------------------------------------------------------------

raw_gpx <- read_gpx("2025/data/strathearn-marathon.gpx")

points_data <- raw_gpx$tracks$`Strathearn Marathon Complete` |>
  as_tibble() |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

line_data <- points_data |>
  st_combine() |>
  st_cast("LINESTRING")

bbx <- st_bbox(line_data)
buffer <- 500
line_proj <- st_transform(line_data, 27700)
bbx <- st_bbox(line_proj)
bbx_expanded <- bbx
bbx_expanded[c('xmin', 'ymin')] <- bbx_expanded[c('xmin', 'ymin')] - buffer
bbx_expanded[c('xmax', 'ymax')] <- bbx_expanded[c('xmax', 'ymax')] + buffer
bbx_expanded <- st_bbox(st_as_sfc(bbx_expanded), crs = 27700)
bbx_expanded <- st_transform(st_as_sfc(bbx_expanded), 4326)

highways <- bbx_expanded |>
  osmdata::opq() |>
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "primary", "secondary", "tertiary", "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path"
    )
  ) |>
  osmdata::osmdata_sf()
roads_cropped <- sf::st_crop(highways$osm_lines, bbx_expanded)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = roads_cropped, colour = "#2D5676",
    linewidth = 0.5
  ) +
  geom_sf(
    data = line_data, colour = line_col,
    linewidth = 1.5
  ) +
  geom_sf(data = head(points_data, 1), colour = line_col,
          size = 3) +
  geom_sf(data = tail(points_data, 1), colour = line_col,
          size = 3) +
  labs(
    title = "STRATHEARN MARATHON",
    subtitle = "12 JUNE 2016",
    caption = glue(
      "<span style='color: {text_col}; font-size:16pt;'>**NICOLA RENNIE**</span> #168"
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_family = body_font, base_size = 13) +
  theme(
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.background = element_rect(
      fill = line_col, colour = line_col
    ),
    panel.grid = element_blank(),
    plot.margin = margin(5, 20, 5, 20),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      lineheight = 1,
      family = body_font,
      face = "bold",
      size = rel(1.6)
    ),
    plot.subtitle = element_textbox_simple(
      colour = alpha(text_col, 0.7),
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 5),
      lineheight = 1,
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = alpha(text_col, 0.7),
      hjust = 1,
      halign = 1,
      margin = margin(b = 10, t = 10),
      lineheight = 1,
      family = body_font
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/04_my_data.png",
  bg = line_col, height = 8, width = 6, units = "in"
)
