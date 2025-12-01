# Packages ----------------------------------------------------------------

library(geodata)
library(rayshader)
library(ggplot2)
library(tidyr)
library(dplyr)
library(viridis)


# Data --------------------------------------------------------------------

rainfall <- worldclim_country(
  country = "GBR", var = "prec", res = 2.5,
  path = tempdir()
)
annual_rainfall <- sum(rainfall)
rainfall_df <- as.data.frame(annual_rainfall, xy = TRUE)
colnames(rainfall_df) <- c("lon", "lat", "rainfall")
rainfall_df <- na.omit(rainfall_df)


# Plot --------------------------------------------------------------------

p <- ggplot(
  data = filter(rainfall_df, lon >= -13.6, lat <= 61),
  mapping = aes(x = lon, y = lat, fill = rainfall)
) +
  geom_tile() +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(
      fill = "#d3f2a3",
      colour = "#d3f2a3"
    )
  )


# 3D ----------------------------------------------------------------------

plot_gg(p,
  solid = FALSE,
  width = 5,
  height = 5,
  windowsize = c(1000, 1000),
  zoom = 0.6,
  phi = 80,
  background = "#d3f2a3",
  theta = 0,
  sunangle = 270,
  scale = 250
)


# Save --------------------------------------------------------------------

render_snapshot("2025/maps/day_06")

# Final edits made in Inkscape

