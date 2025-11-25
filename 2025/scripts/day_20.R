# Set up ------------------------------------------------------------------

library(tidyverse)
library(camcorder)
library(ggtext)
library(osmdata)
library(sf)


# Fonts -------------------------------------------------------------------

sysfonts::font_add_google("Oswald")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Params ------------------------------------------------------------------

bg_col <- "#32435D"
highlight_col <- "#AFBED4"
body_font <- "Oswald"


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  height = 5,
  width = 4,
  units = "in",
  dpi = 300
)


# Data --------------------------------------------------------------------

bbx <- getbb("Loch Lomond")
water <- bbx |> 
  opq() |>
  add_osm_feature(
    key = "natural",
    value = "water"
  ) |>
  osmdata_sf()
map_data <- water$osm_multipolygons |> 
  filter(water == "lake") |> 
  filter(name == "Loch Lomond")


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = map_data,
    fill = highlight_col,
    colour = NA
  ) +
  labs(caption = "Loch Lomond") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.caption = element_textbox_simple(
      halign = 1, hjust = 1,
      colour = highlight_col,
      family = body_font,
      margin = margin(t = -20, b = 10, r = -60),
      face = "bold",
      size = rel(1)
    ),
    plot.caption.position = "plot"
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/20_water.png",
  bg = bg_col, height = 5,
  width = 4, units = "in"
)

