
# Set up ------------------------------------------------------------------

library(glue)
library(tidyverse)
library(camcorder)
library(nrBrand)
library(patchwork)
library(ggtext)


# Fonts -------------------------------------------------------------------

sysfonts::font_add_google("Carter One", "Carter")
sysfonts::font_add_google("Ubuntu", "ubuntu")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  width = 4,
  height = 8,
  units = "in",
  dpi = 300
)


# Parameters --------------------------------------------------------------

dist <- 1000
x_nudge <- 0
y_nudge <- 0
local_crs <- 3763
font_size <- 12
body_font <- "ubuntu"
title_font <- "Carter"
bg_col <- "#C2F9FF"
text_col <- "#00373D"
circle_col <- "grey98"
col_palette <- c("#00818F", "#D8973C", "#A4243B", "#8A1C7C", "grey70")
names(col_palette) <- c("residential", "education", "commercial", "religious", "other")

# Data --------------------------------------------------------------------

center <- c(long = -8.6371 + x_nudge, lat = 41.1519 + y_nudge)
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
      "primary",
      "secondary", "tertiary", "residential"
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
      building %in% c("apartments", "dormitory", "house", "residential", "terrace") ~ "residential",
      building %in% c("college", "school", "university") ~ "education",
      building %in% c("commercial", "retail") ~ "commercial",
      building %in% c("church", "chapel") ~ "religious",
      TRUE ~ "other"
    )
  )
bp_cropped <- suppressWarnings(
  sf::st_intersection(circle, bp)
)
roads_cropped <- suppressWarnings(
  sf::st_intersection(circle, highways$osm_lines)
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
title <- "Porto"
subtitle <- c("41.1519° N, 8.6371° W")

# Plot --------------------------------------------------------------------

plot_function <- function(highlight_group) {
  highlight_col <- unname(col_palette[highlight_group])
  bp_cropped$title <- str_to_upper(highlight_group)
  ggplot() +
    geom_sf(
      data = circle,
      fill = circle_col,
      colour = circle_col,
      linewidth = 1
    ) +
    geom_sf(
      data = roads_cropped,
      col = "grey80"
    ) +
    geom_sf(
      data = bp_cropped,
      mapping = aes(fill = (building == highlight_group)),
      col = NA
    ) +
    geom_sf(
      data = circle,
      fill = "transparent",
      colour = highlight_col,
      linewidth = 1
    ) +
    facet_wrap(~title) +
    scale_fill_manual(
      values = c(unname(col_palette[5]), highlight_col)
    ) +
    coord_sf() +
    theme_void() +
    theme(legend.position = "none",
          strip.text = element_text(
            family = title_font,
            colour = highlight_col,
            size = rel(1.2)
          ))
}

p1 <- plot_function("residential")
p2 <- plot_function("commercial")
p3 <- plot_function("education")
p4 <- plot_function("religious")

main_plot <- ggplot() +
  geom_sf(
    data = circle,
    fill = circle_col,
    colour = circle_col,
    linewidth = 1.5
  ) +
  geom_sf(
    data = roads_cropped,
    col = "grey60"
  ) +
  geom_sf(
    data = bp_cropped,
    mapping = aes(fill = building),
    col = NA
  ) +
  geom_sf(
    data = circle,
    fill = "transparent",
    colour = text_col,
    linewidth = 1.5
  ) +
  scale_fill_manual(
    values = col_palette
  ) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "none")

# Join
p <- (main_plot / ((p1 + p2) / (p3 + p4))) +
  plot_annotation(
    title = str_to_upper(title),
    subtitle = subtitle,
    caption = cap,
    theme = theme(
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
  )
gg_stop_recording()
p


# Save --------------------------------------------------------------------

ggsave(p,
       filename = "2025/maps/day_08.png",
       bg = bg_col, height = 8, width = 4, units = "in"
)

