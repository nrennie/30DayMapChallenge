
# Set up ------------------------------------------------------------------

library(tidyverse)
library(camcorder)
library(sf)
library(showtext)
library(ggfx)
library(gganimate)


# Data --------------------------------------------------------------------

# https://geoportal.statistics.gov.uk/datasets/d4f6b6bdf58a45b093c0c189bdf92e9d_0/explore?filters=eyJDVFJZMjRDRCI6WyJTOTIwMDAwMDMiXX0%3D
scotland <- read_sf("2025/data/Countries_December_2024_Boundaries_UK_BFC_-6467230212120045634/CTRY_DEC_2024_UK_BFC.shp")
# https://opendata.nature.scot/datasets/scottish-wildfire-and-muirburn-extents
fires <- read_sf("2025/data/Scottish_Wildfire_and_Muirburn_Extents/Scottish_Wildfire_and_Muirburn_Extents.shp")


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
highlight_col <- "#FF5C00"


# Text --------------------------------------------------------------------

st <- str_wrap("Data on the extent of wildfires and muirburns in Scotland is incomplete due to staff resources and limitations in satellite imagery availability resulting from cloud cover.", 35)
cap <- paste0(
  "Data: opendata.nature.scot\nGraphic: nrennie.rbind.io"
)


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  height = 6,
  width = 4,
  units = "in",
  dpi = 300
)


# Data wrangling ----------------------------------------------------------

plot_data <- fires |> 
  mutate(year = year(MONTHYEAR)) |> 
  select(year, geometry)


# Plot --------------------------------------------------------------------

g <- ggplot(data = plot_data) +
  geom_sf(data = scotland,
          fill = "grey15", colour = "grey15") +
  with_outer_glow(
    geom_sf(
      fill = highlight_col, colour = highlight_col
    ),
    colour = "white",
    sigma = 3,
    expand = 0
  ) +
  labs(
    title = "Scottish Wildfires in {current_frame}",
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_text(
      colour = text_col,
      hjust = 0,
      margin = margin(b = 0, t = 20),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_text(
      colour = text_col,
      hjust = 0,
      margin = margin(b = -100, t = 90),
      family = body_font,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_text(
      colour = text_col,
      hjust = 0,
      margin = margin(b = 10, t = 0),
      family = body_font
    ),
    plot.margin = margin(0, 5, 0, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )

# animate
g_anim <- g + 
  transition_manual(year) +
  exit_fade()

animate(g_anim, height = 6, width = 4, units = "in", res = 300, bg = bg_col)

anim_save("2025/maps/15_fire.gif")


# Save --------------------------------------------------------------------

g_save <- g %+% filter(plot_data, year == 2022)
g_save + labs(title = "Scottish Wildfires in 2022")  +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_text(
      colour = text_col,
      hjust = 0,
      margin = margin(b = -30, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_text(
      colour = text_col,
      hjust = 0,
      margin = margin(b = -100, t = 40),
      family = body_font,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_text(
      colour = text_col,
      hjust = 0,
      margin = margin(b = 0, t = 0),
      family = body_font
    ),
    plot.margin = margin(0, 5, 0, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none"
  )
ggsave(
  filename = "2025/maps/15_fire.png",
  bg = bg_col, height = 6,
  width = 4, units = "in"
)
