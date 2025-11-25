
library(geodata)
library(ggplot2)
library(dplyr)
library(biscale)
library(elevatr)
library(sf)
library(showtext)
library(nrBrand)
library(ggtext)
library(cowplot)


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Space Grotesk", "Space")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Space"


# Define colours ----------------------------------------------------------

bg_col <- "grey97"
text_col <- "grey10"


# Text --------------------------------------------------------------------

title <- "Up a hill? It's probably raining!"
st <- "Areas with higher elevation also tend to experience higher rainfall, with the hilly area of the North Wessex Downs, Chilterns, and Cotswolds being an exception with little rainfall."
social <- social_caption(
  mastodon = NA,
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = body_font
)
cap <- paste0(
  "**Data**: AWS Terrain Tiles and WorldClim<br>**Graphic**: ", social
)


# Data --------------------------------------------------------------------

# Rainfall
rainfall <- worldclim_country(
  country = "GBR", var = "prec", res = 2.5,
  path = tempdir()
)
annual_rainfall <- sum(rainfall)
rainfall_df <- as.data.frame(annual_rainfall, xy = TRUE)
colnames(rainfall_df) <- c("lon", "lat", "rainfall")
rainfall_df <- na.omit(rainfall_df)

rainfall_sf <- rainfall_df |> 
  as_tibble() |> 
  rename(x = lon, y = lat) |> 
  st_as_sf(coords = c('x','y'), crs = 4326)

# Elevation over same space
elev_data <- get_elev_point(locations = rainfall_sf, src = "aws")

# Biscale class
plot_data <- bi_class(elev_data, x = elevation, y = rainfall, style = "quantile", dim = 3)


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = plot_data
) +
  geom_sf(mapping = aes(color = bi_class), size = 0.005, pch = 15) +
  bi_scale_color(pal = "GrPink", dim = 3) +
  labs(
    title = title,
    subtitle = st,
    caption = cap
  ) +
  theme_void(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_text(
      colour = text_col, family = title_font,
      hjust = 0,
      size = rel(1.6),
      face = "bold",
      margin = margin(t = 0, b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      halign = 0, hjust = 0,
      colour = text_col,
      family = body_font,
      margin = margin(t = 5, b = 10),
      maxwidth = 1.1
    ),
    plot.caption = element_textbox_simple(
      halign = 0, hjust = 0,
      colour = text_col,
      family = body_font,
      margin = margin(t = 15)
    )
  )

legend <- bi_legend(pal = "GrPink",
                    dim = 3,
                    xlab = "Higher elevation ",
                    ylab = "More rain ",
                    pad_color = bg_col,
                    base_family = body_font,
                    size = 7.5) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col)
  )

finalPlot <- ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(legend, 0.08, .6, 0.32, 0.32)


# Save --------------------------------------------------------------------

ggsave("2025/maps/22_natural_earth.png", finalPlot,
       height = 7,
       width = 4,
       bg = bg_col,
       unit = "in"
)
