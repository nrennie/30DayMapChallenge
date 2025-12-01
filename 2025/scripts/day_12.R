
# Packages ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(showtext)
library(ggtext)
library(ggfx)


# Load data ---------------------------------------------------------------

# https://www.data.gov.uk/dataset/52513679-c41a-4c29-8371-fbd7160116e5/futurecoast-erosion-polygons-2100-for-high-emissions-scenario-slr
scotland2100 <- read_sf("2025/data/DYNAMICCOAST2_SCOTLAND_SHP_27700/shp/DC2_RCP8_FUTURE_ERO_2100_PUB.shp")
# https://geoportal.statistics.gov.uk/datasets/d4f6b6bdf58a45b093c0c189bdf92e9d_0/explore?filters=eyJDVFJZMjRDRCI6WyJTOTIwMDAwMDMiXX0%3D
scotland <- read_sf("2025/data/Countries_December_2024_Boundaries_UK_BFC_-6467230212120045634/CTRY_DEC_2024_UK_BFC.shp")


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours ----------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Scotland in 2100"
st <- "**Anticipated erosional areas**, between the 2020 and anticipated 2100 Mean High Water Spring tide lines, based on a High Emissions Scenario sea level rise projection and *do nothing* coastal management approach."
cap <- paste0(
  "**Data**: SpatialData.gov.scot<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

p <- ggplot() +
  geom_sf(data = scotland,
          fill = "grey15", colour = "grey15") +
  with_outer_glow(
    geom_sf(
      data = scotland2100, 
      fill = "white", colour = "white"
    ),
    colour = "white",
    sigma = 3,
    expand = 0
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -30, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = -100, t = 40),
      family = body_font,
      maxwidth = 0.7,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 0),
      family = body_font
    ),
    plot.margin = margin(5, 5, 5, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )


# Save --------------------------------------------------------------------

ggsave(p,
  filename = "2025/maps/day_12.png",
  bg = bg_col, height = 6, width = 4, units = "in"
)

