# Package -----------------------------------------------------------------

library(tidyverse)
library(sf)
library(osrm)
library(ggmap)
library(showtext)
library(glue)
library(ggtext)
library(ggspatial)
library(camcorder)


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  width = 4.6,
  height = 6,
  units = "in",
  dpi = 300
)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours ----------------------------------------------------------

bg_col <- "grey80"
text_col <- "#00558e"


# Data --------------------------------------------------------------------

# Greggs locations from https://automaticknowledge.org/training/bonusdata/
greggs <- readr::read_csv("2025/data/greggs-locations-24-oct-2023.csv")
greggs_newcastle <- greggs |>
  filter(str_starts(address.postCode, "NE"))

iso_10 <- list()
for (i in 1:nrow(greggs_newcastle)) {
  iso_10[[i]] <- osrmIsochrone(
    loc = c(
      greggs_newcastle$address.longitude[i],
      greggs_newcastle$address.latitude[i]
    ),
    breaks = 10,
    osrm.profile = "foot"
  )
  print(paste0(i, " / ", nrow(greggs_newcastle)))
}
iso_10_fixed <- lapply(iso_10, st_make_valid)
iso_10_geom <- do.call(rbind, iso_10_fixed)


# Text --------------------------------------------------------------------

st <- glue("<span style='font-size:18pt; font-family:{title_font}'>If you have a Newcastle postcode, is there a Greggs <span style='color:#fab824;'>**within a 10 minute walk**</span>?</span>")
cap <- paste0(
  st, "<br><br>",
  "**Data**: automaticknowledge.org<br>**Map**: OpenStreetMap<br>**Graphic**: ",
  "<span style='font-family:\"Font Awesome 6 Brands\";color:#00558e;'>&#xf09b;</span><span style='color:transparent;'>.</span><span style='font-family:Nunito;color:#00558e;'>nrennie</span>"
)


# Plot --------------------------------------------------------------------

ggplot() +
  annotation_map_tile(zoom = 10, type = "osm", alpha = 0.4) +
  geom_sf(data = iso_10_geom, fill = "#fab824", color = "#fab824") +
  labs(tag = cap) +
  theme_void() +
  theme(
    plot.tag.position = c(0.03, 0.98),
    plot.tag = element_textbox_simple(
      hjust = 0, halign = 0,
      vjust = 1, valign = 1,
      colour = text_col,
      family = body_font,
      size = rel(0.8),
      maxwidth = 0.72,
      lineheight = 1.6
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/day_13.png",
  bg = "white", height = 6, width = 4.6, units = "in"
)
