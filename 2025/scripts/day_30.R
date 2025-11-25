# Set up ------------------------------------------------------------------

library(metR)
library(tidyverse)
library(camcorder)
library(sf)
library(showtext)
library(elevatr)
library(MetBrewer)
library(ggtext)
library(glue)
library(nrBrand)


# Data --------------------------------------------------------------------

# use elevatr to get elevation data
ras <- get_elev_raster(
  locations = data.frame(x = c(-3.27, -3.15), y = c(54.43, 54.49)),
  z = 10, prj = "EPSG:4326", clip = "locations"
)
elev_mat <- terra::as.matrix(ras, wide = TRUE)
colnames(elev_mat) <- 1:ncol(elev_mat)
elev_df <- elev_mat |>
  as_tibble() |>
  mutate(y = row_number()) |>
  pivot_longer(-y, names_to = "x") |>
  mutate(x = as.numeric(x))


# Fonts -------------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"

col_palette <- met.brewer("Demuth", n = 13, direction = -1)


# Start recording ---------------------------------------------------------

gg_record(
  "recording",
  device = "png",
  height = 5,
  width = 5,
  units = "in",
  dpi = 300
)


# Text --------------------------------------------------------------------

txt <- glue("<span style='font-family:{title_font}; font-size:14pt;'>**Scafell Pike**</span><br><span style='font-size:12pt;'>978m</span><br><br>**Graphic**: <span style='font-family:\"Font Awesome 6 Brands\";color:#591C19;'>&#xf09b;</span><span style='font-family:Nunito;color:#591C19;'> nrennie</span>")


# Plot --------------------------------------------------------------------

ggplot(
  data = elev_df,
  mapping = aes(x = x, y = y)
) +
  geom_contour_fill(mapping = aes(z = value), bins = 13) +
  geom_contour_tanaka(mapping = aes(z = value), bins = 13) +
  geom_textbox(
    data = data.frame(x = 0, y = 0, label = txt),
    mapping = aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1,
    colour = col_palette[13],
    fill = alpha(col_palette[7], 0.7),
    box.margin = unit(c(15, 5, 5, 14), "pt"),
    family = body_font,
    size = 2.5
  ) +
  scale_fill_met_c("Demuth", direction = -1) +
  scale_y_reverse() +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(-4, 0, 0, 0))


# Save --------------------------------------------------------------------

ggsave(
  filename = "2025/maps/30_makeover.png",
  bg = bg_col, height = 5,
  width = 5, units = "in"
)
