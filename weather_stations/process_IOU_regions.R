# Owen Doherty - Eagle Rock Analytics
# Create a reliable dataset to call including all major IOU shapefiles
# Feb 24, 2021

library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library("sf")
library('dplyr')

#Load IOU shapefiles
#Load fire-weather shapefiles
#Merge into single product; save/export

####################
#Section 1: IOU data
###################

#Load IOU shapefile
service_area <- st_read(
  "/home/owen/research/wildfire/service_areas/service-areas.shp")

#Drop non-IOUs from analysis
IOU_areas <- service_area %>%
  filter(Type=='IOU')

# Change column names
# Re-grid?

####################
#Section 2: Fire Weather Regions
###################

#Load IOU shapefile
service_area <- st_read("/home/owen/research/wildfire/fire-weather-shapes/*.shp")

input_path <- "/home/owen/research/wildfire/fire-weather-shapes/"
files <- list.files(input_path, pattern="[.]shp$", full.names=TRUE)
allShapes <- lapply(files, readOGR)

allShapes[[1]]@data$id<-"Bay_Area"
allShapes[[2]]@data$id<-"Central_Coast"
allShapes[[4]]@data$id<-"Modoc"
allShapes[[5]]@data$id<-"Northwest"
allShapes[[6]]@data$id<-"San Diego"

#LA's shape file is named differently
allShapes[[3]]@data$id<-"LA"
allShapes[[3]]@data$Name <- NULL

#Fix Sierra's East
colnames(allShapes[[7]]@data)[1] <- "id"
allShapes[[7]]@data$id<-"Sierras_East"

#Fix Sierra's West
colnames(allShapes[[8]]@data)[1] <- "id"
allShapes[[8]]@data$id<-"Sierras_West"

#Review Output
lapply(allShapes, print)

#Colapse list
library(raster)
firewx <- do.call(bind, allShapes)
firewx.sf <- st_as_sf(firewx)  #Refrom into sf object for merging


####
# Section 2: Merge
#####

#align the naming of regions and the object type
colnames(firewx.sf)[1] <- "region"
colnames(IOU_areas)[3] <- "region"
IOU_areas$region <- as.character(IOU_areas$region)

#align projections
# wx regions are "+proj=longlat +datum=WGS84 +no_defs":
st_crs(firewx.sf)$proj4string
crs_standard <- st_crs(firewx.sf)$proj4string

# IOUs are:
st_crs(IOU_areas)$proj4string

# Bring IOU projection to a standard grid
IOU_areas.sf <- st_transform(IOU_areas, crs=crs_standard)
library(dplyr)
IOU_small.sf <- IOU_areas.sf %>%
  dplyr::select(region,geometry)

#all_shapes.sf <- merge(firewx.sf,IOU_areas,by="region",all.x=TRUE,all.y=TRUE)
all_shapes.sf <- st_join(firewx.sf,IOU_areas.sf)
#all_shapes2.sf <- rbind(firewx.sf,IOU_small.sf)

####
# Section 3: Output
###
setwd('/home/owen/research/wildfire/rds/')
saveRDS(all_shapes.sf,"firewx_regions.RDS")

#output individual
saveRDS(IOU_areas.sf,"IOU_areas.RDS")
saveRDS(firewx.sf,"firewx.sf.RDS")


#Load the various fire weather regions

#service_area <- readOGR("/home/owen/research/wildfire/service_areas/service-areas.shp")



# # liberty_raster <- raster("/home/owen/research/wildfire/liberty_service_territory/")
# # 
# library(raster)
# #paccorp_raster <- raster(paccorp_area)
# #coordinates(paccorp_raster) <- ~ Longitude + Latitude
# 
# library(rgdal)
# #liberty_transformed <- st_transform(liberty_service, CRS(proj4string(bioclim.data))) #align projections
# paccorp_transformed <- st_transform(paccorp_area, CRS(proj4string(bioclim.data))) #align projections
# 
# 
# #keep only data within the PacCorp domain
# bioclim.paccorp<-raster::crop(bioclim.data,paccorp_transformed)
# #bioclim.paccorp<-raster::extract(bioclim.data,st_zm(paccorp_transformed))
