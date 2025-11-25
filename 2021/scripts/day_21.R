library(tidyverse)
library(rayshader)
library(raster)

#Set input directory
os50_height_dir <- "2021/data/os_grid/nn/"

#Load all terrain files in input directory
raster_layers <- list.files(os50_height_dir, "*.asc$")

raster_layers <- tibble(filename = list.files(os50_height_dir, "*.asc$")) %>% 
  mutate(raster =
           map(filename, .f = ~raster::raster(rgdal::readGDAL(paste0(os50_height_dir, .))))
  ) %>% 
  pull(raster)
raster_layers$fun <- mean
raster_mosaic <- do.call(raster::mosaic, raster_layers)

elmat = matrix( raster::extract(raster_mosaic, raster::extent(raster_mosaic), buffer = 1000), nrow = ncol(raster_mosaic), ncol = nrow(raster_mosaic) )
elmat %>%
  sphere_shade(zscale = 50, sunangle = 270, texture = "imhof4") %>%
  add_water(detect_water(elmat, min_area = 50), color="imhof4") %>%
  add_shadow(ray_shade(elmat, anglebreaks = seq(60, 90), sunangle = 270, multicore = TRUE, lambert = FALSE)) %>%
  add_shadow(ambient_shade(elmat, multicore = TRUE)) %>% 
  plot_3d(elmat, zscale = 50, soliddepth = -75)


col_ramp <- colorRampPalette(c("#54843f", "grey", "white"))

png("elevation_shading.png", width=ncol(raster_mosaic), height=nrow(raster_mosaic), units = "px", pointsize = 1)

par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i") 

raster::image(
  raster_mosaic,
  col = col_ramp(12),
  maxpixels = raster::ncell(raster_mosaic),
  axes = FALSE
)

dev.off()

terrain_image <- png::readPNG("elevation_shading.png")

terrain_image %>% 
  add_water(detect_water(elmat, min_area = 50), color="imhof4") %>%
  add_shadow(ray_shade(elmat, anglebreaks = seq(60, 90), sunangle = 270, multicore = TRUE, lambert = FALSE)) %>%
  add_shadow(ambient_shade(elmat, multicore = TRUE)) %>% 
  plot_3d(elmat, zscale = 50, soliddepth = -75)


render_snapshot("2021/viz/map_21",
                title_text = "Ben Nevis & Glen Coe",
                title_offset = c(60,50),
                title_color = "black",
                title_size = 40,
                title_font = "serif",
                vignette = 0.3)

