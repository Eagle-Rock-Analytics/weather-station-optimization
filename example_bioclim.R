## Bioclim example plot

library(tidyverse)
library(raster)
library(sf)
library(tmap)

load("map_data/map_objects.RData")

worldclim <- stack("map_data/CA_worldclim_crop.tif")
bio4 <- subset(worldclim, 4)

ca_box <- list(y = c(32,43), x = c(-125,-112))

bio4_crop <- crop(bio4, ca_box)

tm_shape(bio4_crop) + tm_raster(palette = "YlGnBu", title = "Temperature seasonality", ) +
  tm_shape(CA) + tm_borders() +
  tm_layout(legend.position = c("right", "top"))

