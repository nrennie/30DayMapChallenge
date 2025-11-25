library(sf)
library(ggplot2)
library(roughsf)
library(dplyr)

s_file <- st_read(dsn = "2021/data/boundaries/Data/GB/district_borough_unitary_ward_region.shp") %>%
  filter(FILE_NAME == "SHETLAND_ISLANDS")

#prep data
si <- st_cast(s_file, "POLYGON")
si$fill <- "#64513B"
si$color <- "#64513B"
si$stroke <- 1
si$fillweight <- 0.5

#plot
p <- roughsf(si,
            title = "SHETLAND ISLANDS", caption = "N. Rennie | Data: osdatahub.os.uk",
            title_font = "48px Pristina", 
            font = "30px Pristina", 
            caption_font = "30px Pristina",
            roughness = 4, bowing = 1, simplification = 1)
p

#save image
save_roughsf(p, file="map_22.png", background="#B9A37E")


