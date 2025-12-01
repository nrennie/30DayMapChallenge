# Set up ------------------------------------------------------------------

library(glue)


# Fonts -------------------------------------------------------------------

sysfonts::font_add_google("Carter One", "Carter")
sysfonts::font_add_google("Ubuntu", "ubuntu")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Parameters --------------------------------------------------------------

location <- "Lancaster, UK"
dist <- 2000
x_nudge <- -0.07
y_nudge <- -0.03
local_crs <- 7855
font_size <- 12
body_font <- "Ubuntu"
title_font <- "Carter"
highlight_col <- "white"


# Data --------------------------------------------------------------------

bbx <- osmdata::getbb(location)
center <- c(long = mean(bbx[1, ]) + x_nudge, lat = mean(bbx[2, ]) + y_nudge)
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

bbox <- sf::st_bbox(c(xmin = circle_bbx[1,1], 
                      ymin = circle_bbx[2,1], 
                      xmax = circle_bbx[1,2],
                      ymax = circle_bbx[2,2]))
bbox_sf <- sf::st_as_sfc(bbox, crs = 4326)

highways <- circle_bbx |>
  osmdata::opq() |>
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "primary"
    )
  ) |>
  osmdata::osmdata_sf()

buildings <- circle_bbx |>
  osmdata::opq() |>
  osmdata::add_osm_feature(
    key = "building"
  ) |>
  osmdata::osmdata_sf()

bp <- buildings$osm_polygons |>
  dplyr::select(geometry, building) |>
  dplyr::mutate(
    building = dplyr::case_when(
      is.na(building) ~ "other",
      building == "yes" ~ "other",
      TRUE ~ building
    )
  )
bp_cropped <- sf::st_crop(bp, bbox_sf)
roads_cropped <- sf::st_crop(highways$osm_lines, bbox_sf)


# Text --------------------------------------------------------------------

social <- glue("<span style='font-family:\"Font Awesome 6 Brands\";color:{highlight_col};'>&#xf09b;</span><span style='color:black;'>.</span><span style='font-family:ubuntu;color:{highlight_col};'>nrennie</span>")
cap <- glue(
  "**Data**: OpenStreetMap | **Graphic**: {social}"
)
tag <- glue(
  "<span style='font-family: {body_font}; font-size: 18pt;'>*Churches of* </span><span style='font-family: {title_font}; color: {highlight_col}; font-size: 24pt;'>Lancaster</span><br>{cap}"
)

# Plot --------------------------------------------------------------------

p <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = roads_cropped,
    col = "grey10"
  ) +
  ggplot2::geom_sf(
    data = bp_cropped,
    mapping = ggplot2::aes(fill = (building %in% c("church"))),
    col = NA
  ) +
  ggplot2::scale_fill_manual(
    values = c("grey15", "white")
  ) +
  ggplot2::labs(
    tag = tag
  ) +
  coord_sf(expand = FALSE) +
  maprints:::theme_map("black", highlight_col, body_font, font_size) +
  ggplot2::theme(
    plot.tag.position = c(0.97, 0.99),
    plot.margin = margin(0, 0, 0, 0),
    plot.tag = ggtext::element_textbox_simple(
      margin = ggplot2::margin(b = -30, t = 30),
      hjust = 1,
      halign = 1,
      colour = highlight_col,
      family = "ubuntu",
      size = 11
    )
  )
p


# Save --------------------------------------------------------------------

ggplot2::ggsave(p,
  filename = "2025/maps/day_03.png",
  bg = "black", height = 8, width = 8, units = "in"
)
