library(elevatr)
library(rayshader)
library(sf)
library(rgeoboundaries)
library(tidyverse)

#Get Scotland map
scotland <- gb_adm1(c("United Kingdom"), type = "sscgs") %>%
  filter(shapeName == "Scotland")

#Get elevation data
scot_raster <- get_elev_raster(locations = scotland$geometry, z = 8, clip = "locations")

#Convert to matrix for rayshader plotting
scot_rayshader = raster_to_matrix(scot_raster)

scot_rayshader %>% 
  sphere_shade(texture = "bw") %>% 
  plot_3d(scot_rayshader, zscale = 10, fov = 0, theta = 0, zoom = 0.9, phi = 45, windowsize = c(800, 800))

render_snapshot("2021/viz/map_10",
                title_text = "The Hills\n      of\n Scotland",
                title_offset = c(60,50),
                title_color = "black",
                title_size = 40,
                title_font = "serif",
                vignette = 0.3)
