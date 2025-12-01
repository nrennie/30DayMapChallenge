
# Fonts -------------------------------------------------------------------

sysfonts::font_add_google("Carter One", "Carter")
sysfonts::font_add_google("Ubuntu")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Parameters --------------------------------------------------------------

location <- "Melbourne, Australia"
x_nudge <- -0.3
y_nudge <- 0.08
local_crs <- 7855
dist <- 35000
bg_col <- "#311E5C"
line_col <- "#8465CD"
font_size <- 12
body_font <- "Ubuntu"
title_font <- "Carter"


# Data --------------------------------------------------------------------

bbx <- osmdata::getbb(location)
highways <- bbx |>
  osmdata::opq() |>
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "primary"
    )
  ) |>
  osmdata::osmdata_sf()
streets <- bbx |>
  osmdata::opq() |>
  osmdata::add_osm_feature(
    key = "highway",
    value = c(
      "secondary", "tertiary", "residential"
    )
  ) |>
  osmdata::osmdata_sf()

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

streets_lines <- suppressWarnings(
  sf::st_intersection(circle, streets$osm_lines)
)
highways_lines <- suppressWarnings(
  sf::st_intersection(circle, highways$osm_lines)
)


# Text --------------------------------------------------------------------

cap <- maprints:::map_text(location,
  coords = center,
  graphic = "Nicola Rennie",
  font_color = line_col,
  icon_color = line_col,
  font_size = font_size,
  title_font_family = title_font,
  font_family = body_font
)


# Plot --------------------------------------------------------------------

p <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = streets_lines,
    col = line_col,
    linewidth = 0.1,
    alpha = 0.7
  ) +
  ggplot2::geom_sf(
    data = highways_lines,
    colour = line_col,
    linewidth = 0.4
  ) +
  ggplot2::geom_sf(
    data = circle,
    colour = line_col,
    linewidth = 3,
    fill = NA
  ) +
  ggplot2::labs(
    caption = cap
  ) +
  maprints:::theme_map(bg_col, line_col, body_font, font_size)


# Save --------------------------------------------------------------------

ggplot2::ggsave(p,
  filename = "2025/maps/day_02.png",
  bg = bg_col, height = 11, width = 8.5, units = "in"
)
