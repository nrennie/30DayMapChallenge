library(rayshader)
library(elevatr)
library(tidyverse)
library(sf)

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
uk <- sf::st_read("2022/data/UK_Counties/Counties_and_Unitary_Authorities_(April_2019)_Boundaries_UK_BGC.shp") 
wi <- uk %>% 
  filter(ctyua19nm == "Na h-Eileanan Siar")

# Get elevation data
scot_raster <- get_elev_raster(locations = wi$geometry, z = 8, clip = "locations")

# Convert it to a matrix
elmat <- raster_to_matrix(scot_raster)
elmat[is.na(elmat)] <- 0

# add padding to each side to make square
m1 = matrix(0, ncol = 1570, nrow = 260)
m2 = matrix(0, ncol = 1570, nrow = 261)
elmat <- rbind(m1, elmat, m2)

# add extra padding top and bottom
m1 = matrix(0, ncol = 20, nrow = 1610)
m2 = matrix(0, ncol = 20, nrow = 1610)
elmat <- cbind(m1, elmat, m2)
  
# 3d plot
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(watermap = detect_water(elmat), color = "imhof3") %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 0, phi = 85,
          windowsize = c(800, 800),
          zoom = 0.6,
          background = "white")
  render_clouds(elmat,
                zscale = 10,
                start_altitude = 30,
                end_altitude = 1000,
                attenuation_coef = 2,
                cloud_cover = 0.4,
                layers = 5,
                clear_clouds = T)
  render_snapshot(filename = "2022/maps/day_28.png",
                  clear = FALSE,
                  title_text = "Na h-Eileanan Siar",
                  title_size = 50,
                  title_color = "white",
                  title_font = "serif")
