library(tidyverse)
library(sf)
library(roughsf)

# UK Map from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-buc/explore?location=55.216238%2C-3.316413%2C6.38
uk <- sf::st_read("2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") 
scotland <- uk %>% 
  filter(CTRY21NM == "Scotland")

#prep data
sc_sf <- st_cast(scotland, "POLYGON")
sc_sf$fill <- "blue"
sc_sf$colour <- "blue"
sc_sf$stroke <- 1
sc_sf$fillweight <- 0.5
sc_sf$fillstyle <- "cross-hatch"

#plot
p <- roughsf(sc_sf,
             caption = "\nSCOTLAND",
             caption_font = "56px Pristina",
             roughness = 20,
             bowing = 5,
             width = 1800, 
             height = 1800)
p

#save image
save_roughsf(p, file = "2022/maps/day_02.png")