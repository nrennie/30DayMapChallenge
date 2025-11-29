# Inspired by https://jamiehudson.netlify.app/post/distance_map/

library(openrouteservice)
library(tidyverse)
library(osmdata)
library(sf)
library(showtext)
library(nrBrand)
library(ggtext)
library(MetBrewer)
library(camcorder)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Ubuntu"


# Define colours ----------------------------------------------------------

text_col <- "grey97"
bg_col <- "grey10"
col_palette <- met.brewer("Tam", n = 5, type = "discrete")


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  width = 9,
  height = 5,
  units = "in",
  dpi = 300
)


# Text --------------------------------------------------------------------

title <- "How far can you get from Edinburgh Waverley train station in 15 minutes..."
social <- social_caption(
  mastodon = NA,
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
cap <- paste0(
  "**Data**: openrouteservice.org by HeiGIT and OpenStreetMap | **Graphic**: ", social
)


# Data --------------------------------------------------------------------

coordinates <- data.frame(lon = c(-3.188486), lat = c(55.952403))
waverley_walk_iso <- ors_isochrones(
  locations = coordinates,
  profile = "foot-walking",
  range = 900, interval = 180,
  output = "sf"
) |>
  arrange(desc(dplyr::row_number()))
waverley_cycle_iso <- ors_isochrones(
  locations = coordinates,
  profile = "cycling-regular",
  range = 900, interval = 180,
  output = "sf"
) |>
  arrange(desc(dplyr::row_number()))
waverley_drive_iso <- ors_isochrones(
  locations = coordinates,
  profile = "driving-car",
  range = 900, interval = 180,
  output = "sf"
) |>
  arrange(desc(dplyr::row_number()))

x <- c(-3.29, -3.08)
y <- c(55.88, 55.993)

custom_waverley <- rbind(x, y)
colnames(custom_waverley) <- c("min", "max")
streets <- custom_waverley |>
  opq() |>
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway", "primary",
      "secondary", "tertiary",
      "trunk", "secondary_link", "tertiary_link",
      "residential", "living_street",
      "unclassified",
      "service", "footway"
    )
  ) |>
  osmdata_sf()
streets_sf <- streets$osm_lines |>
  select(osm_id, geometry)

streets_intersect <- function(i, data) {
  output <- st_intersection(st_make_valid(data[i, ]), streets_sf)
  return(output)
}

walk_df <- map(
  .x = seq_len(nrow(waverley_walk_iso)),
  .f = ~ streets_intersect(.x, data = waverley_walk_iso)
) |>
  bind_rows() |>
  mutate(
    type = "...on foot?",
    value = value / 60
  )

cycle_df <- map(
  .x = seq_len(nrow(waverley_cycle_iso)),
  .f = ~ streets_intersect(.x, data = waverley_cycle_iso)
) |>
  bind_rows() |>
  mutate(
    type = "...by bike?",
    value = value / 60
  )

drive_df <- map(
  .x = seq_len(nrow(waverley_drive_iso)),
  .f = ~ streets_intersect(.x, data = waverley_drive_iso)
) |>
  bind_rows() |>
  mutate(
    type = "...by car?",
    value = value / 60
  )

all_data <- do.call("rbind", list(
  walk_df, cycle_df, drive_df
)) |>
  mutate(
    type = factor(type, levels = c(
      "...on foot?", "...by bike?", "...by car?"
    ))
  )


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = streets_sf,
    color = "grey25",
    linewidth = 0.2
  ) +
  geom_sf(
    data = all_data,
    mapping = aes(
      colour = as.factor(value)
    ),
    linewidth = 0.2
  ) +
  facet_wrap(~type, nrow = 1) +
  scale_colour_manual(values = col_palette) +
  guides(colour = guide_legend(
    nrow = 1,
    override.aes = list(fill = col_palette, linewidth = 4),
    title.position = "right"
  )) +
  labs(
    title = title,
    caption = cap,
    colour = "minutes"
  ) +
  coord_sf(
    xlim = custom_waverley[1, ],
    ylim = custom_waverley[2, ],
    expand = FALSE
  ) +
  theme_void(base_family = body_font, base_size = 12) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col, family = title_font,
      halign = 0.5, hjust = 0.5,
      size = rel(1.6),
      face = "bold",
      margin = margin(t = 0, b = 15)
    ),
    plot.caption = element_textbox_simple(
      halign = 0.5, hjust = 0.5,
      colour = text_col,
      family = body_font,
      margin = margin(t = 25)
    ),
    legend.position = "bottom",
    legend.text.position = "bottom",
    legend.title = element_text(
      colour = text_col,
      size = rel(0.8), vjust = 0.1
    ),
    legend.text = element_text(
      colour = text_col,
      size = rel(0.8)
    ),
    legend.key.width = unit(0.6, "in"),
    legend.key.spacing = unit(0.05, "in"),
    legend.box.spacing = unit(0.3, "in"),
    panel.spacing = unit(0.1, "in"),
    strip.text = element_text(
      family = title_font,
      size = rel(1.3),
      face = "bold",
      colour = text_col,
      margin = margin(b = 5)
    )
  )


# Save --------------------------------------------------------------------

ggsave("2025/maps/day_26.png",
  height = 5,
  width = 9,
  bg = bg_col,
  unit = "in"
)
