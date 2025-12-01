library(tidyverse)
library(sf)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(rnaturalearth)
library(ggfx)


# Data --------------------------------------------------------------------

plague <- read_csv("2025/data/Historical_plague_outbreaks.txt")

plot_data <- plague |> 
  select(Year_of_Outbreak, Lon = Lat_WGS84, Lat = Lon_WGS84) |> # mislabelled columns!!
  filter(Lon < 1000,
         Year_of_Outbreak > 5, Year_of_Outbreak < 1900) |> 
  mutate(century = 100 * floor((Year_of_Outbreak / 100)),
         century = paste0(century, "s")) |> 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

world <- ne_countries(scale = 50)


# Colours -----------------------------------------------------------------

bg_col <- "grey10"
text_col <- "grey90"


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
title <- "Six centuries of plague"
st <- "Each dot represents a plague outbreak occuring between 1347 and 1899 in Europe."
cap <- paste0(
  "**Data**: envidat.ch/#/metadata/digitizing-historical-plague<br>**Graphic**: ", social
)


# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = world,
    fill = "grey15", colour = bg_col
  ) +
  with_outer_glow(
    geom_sf(
      data = plot_data, 
      fill = "white", colour = "white",
      size = 0.1
    ),
    colour = "white",
    sigma = 3,
    expand = 0
  ) +
  facet_wrap(~century, ncol = 3) +
  coord_sf(
    xlim = c(-12, 39),
    ylim = c(29, 62)
  ) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.6),
      vjust = 0
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 5),
      family = body_font,
      maxwidth = 1,
      lineheight = 1,
      vjust = 1
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    plot.margin = margin(5, 5, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    strip.text = element_text(
      family = title_font,
      colour = text_col,
      margin = margin(b = 3)
    )
  )


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/day_28.png",
  bg = bg_col, height = 5, width = 6, units = "in"
)

