### Plot example thresholded similarity score layer
### Grace Di Cecco
### Eagle Rock Analytics

#### Setup ####

library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(tigris)
library(rJava)
library(colorspace)
library(sf)
library(rdgal)
library(knitr)
library(kableExtra)


## Directories
map_data <- "map_data/"
weather_dir <- "fire-weather-shapes/"
fig_dir <- "figs/"
station_dir <- "weather_stations/"

## Read in merged dataset
active.all <- readRDS(paste0(map_data, "wx_station.rds"))

#set level of data quality; 1 highest, 2 iou/state, 3 universities, 4 private, 5 citizen
quality.accept = 2

active.sub <- active.all %>%
  filter(Quality<=quality.accept)

#Load GIS objects
load(file = paste0(map_data,"map_objects.RData"))

#Load threshold raster for PG&E
pge_raster <- raster("pred_rasters/model_PacificGas&ElectricCompany_wxstathresh_2_threshscore.grd")
pge_df <- rasterToPoints(pge_raster) %>%
  as.data.frame()

# Determine geographic extent of our data
max.lat <- ceiling(max(pge_df$y))
min.lat <- floor(min(pge_df$y))
max.lon <- ceiling(max(pge_df$x))
min.lon <- floor(min(pge_df$x))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

#Plot all stations
labtext = 'Pacific Gas & Electric Company priority regions'
#subtext = 'All Long Term ISD Stations'
captext = 'Produced in support of EPC-18-026'
fout = 'threshold_map.pdf'
a <- ggplot(data = active.sub, aes(x = Longitude, y = Latitude), alpha = 0) + 
  geom_polygon(data = CA, aes(x=long, y = lat, group = group), fill = 'grey70', color='black', size=1) +
  geom_tile(data=pge_df, aes(x=x, y=y, fill=as.factor(layer)), alpha=0.8) + 
  #scale_fill_gradientn("similarity score",colours=c("#FFECB3","#E85285","#3e0020"), na.value = NA, limits=c(0,1)) +
  #scale_fill_gradientn("similarity score",colours=c("#FFECB3","#FFA500","#1e003e"), na.value = NA, limits=c(0.1,0.7)) +
  #scale_fill_continuous_sequential(palette = "Viridis", begin = 0.1, end = 0.7)+
  scale_fill_manual(values = c("#414487FF", "#FDE725FF"), labels = c('0' = 'Low priority', '1' = 'High priority'))+
  labs(fill = " ") +
  #scale_fill_gradientn("similarity score",colours = colorspace::Viridis(7), na.value = NA, limits=c(0.1,0.7)) +
  geom_polygon(data = CA.counties, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = F)+
  geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.05, color="black", alpha=1, show.legend = F)+
  # geom_point(alpha=0.5, size=0.1, color="black")+
  
  coord_fixed(xlim = c(min.lon,max.lon),  ylim = c(min.lat, max.lat), ratio=1.3)+
  
  
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  theme_bw() +
  labs(title=labtext,caption=captext,x="Longitude",y="Latitude")
