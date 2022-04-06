library(raster)
library(maptools)
library(tidyverse)

# output dir
map_data <- "map_data/"

#This is tile #11 pacific/west coast
bioclim.data.11 <- getData(name = "worldclim",
                           var = "bio",
                           res = 0.5,
                           lat = 38.5,
                           lon = -125)

#Tile 12 is mountain west US + great plains
bioclim.data.12 <- getData(name = "worldclim",
                           var = "bio",
                           res = 0.5,
                           lat = 38.5,
                           lon = -105)

# ## Read in from saved tiles instead of downloading
# bioclim.11.files <- list.files(paste0(map_data, "worldclim/bio_11"), pattern=".bil$",full.names = T)
# bioclim.data.11 <- stack(bioclim.11.files)
# 
# bioclim.12.files <- list.files(paste0(map_data, "worldclim/bio_12"), pattern=".bil$",full.names = T)
# bioclim.data.12 <- stack(bioclim.12.files)
# 

#This section of code is slow
temp <- raster::merge(bioclim.data.11,bioclim.data.12)

#Subset raster stacks to California domain
library(tigris)
states.sf <-states(cb=FALSE)
ca.sf <- states.sf %>%
  filter(STUSPS=='CA')

#Mask out data outside of CA
bioclim.data.CA <- mask(temp,ca.sf,snap="out")                       #mask will mask out data points outside CA shapefile

#Save this output for future runs

#saveRDS(bioclim.data.CA,file="CA_worldclim_crop.RDS")
#writeRaster(bioclim.data.CA,file="CA_worldclim_crop.grd")
#brick(bioclim.data.CA,file="CA_worldclim_crop.tif")

outfile <- writeRaster(bioclim.data.CA, filename=paste0(map_data,'CA_worldclim_crop.tif'), format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

