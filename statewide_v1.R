# January 11, 2021
# Statewide Analysis - Rough (no layers)
# Based off code by Jeff Oliver -- jcoliver@email.arizona.edu

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

#### Statewide weather stations ####

# Commented out code to produce data file in git repo
# Read in statewide weather stations
# setwd('~/research/wildfire')
# in.data <- readRDS(file = "statewide_stations.csv")
# 
# #Remove all rows with na's in them
# in.data <- in.data[rowSums(is.na(in.data)) == 0,]
# 
# #Remove stations with incorrect metadata
# in.data<-in.data[!(in.data$Station.Name=="CW5652 Two Harbors"),]  #this station has a wrong longitude
# 
# #filter to only include recently active stations
# in.data$last.year <- as.numeric(format(as.Date(in.data$Last.Data.Date,'%Y-%m-%d'),'%Y'))
# 
# # revise and revisit later - maybe include stations that had data in last quarter of 2020?
# active.stations <- in.data %>% filter(last.year>=2021)
# 
# #options to filter based on data quality
# data.q<-read.csv(file="data_quality_networks.csv")
# data.q.short <- data.q %>%
#   select(c("Abbreviation","Quality","Tier"))
# colnames(data.q.short)[1] <-"Mesonet"
# 
# active.all <- merge(active.stations,data.q.short,by="Mesonet",all=TRUE)

## Read in merged dataset
active.all <- readRDS(paste0(map_data, "wx_station.rds"))

#set level of data quality; 1 highest, 2 iou/state, 3 universities, 4 private, 5 citizen
quality.accept = 3

active.sub <- active.all %>%
    filter(Quality<=quality.accept)

#### Create a map of all stations ####

states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

#ca_base + theme_nothing() + 
ca_base + theme_bw() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA) +  # get the state border back on top +
  geom_point(data=active.sub,aes(x=Longitude,y=Latitude,group=Mesonet,color=Tier),size=1) 

  #geom_point(data=active.stations,aes(x=Longitude,y=Latitude,color=Mesonet,group=NULL))


#Create a subset of the stations with just latitude and longitude
obs.data <- active.sub %>%
  select('Latitude','Longitude')
colnames(obs.data)[1:2] <-c("latitude","longitude")

# Check the data to make sure it loaded correctly
summary(obs.data)

# Notice NAs - drop them before proceeding
obs.data <- obs.data[!is.na(obs.data$latitude), ]

# Make sure those NA's went away
summary(obs.data)

# Determine geographic extent of our data
max.lat <- ceiling(max(obs.data$latitude))
min.lat <- floor(min(obs.data$latitude))
max.lon <- ceiling(max(obs.data$longitude))
min.lon <- floor(min(obs.data$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# # Load the data to use for our base map
# data(wrld_simpl)
# data("state.vbm")
# 
# # Plot the base map
# plot(wrld_simpl, 
#      xlim = c(min.lon, max.lon),
#      ylim = c(min.lat, max.lat),
#      axes = TRUE, 
#      col = "grey95")
# 
# # Add the points for individual observation
# points(x = obs.data$longitude, 
#        y = obs.data$latitude, 
#        col = "olivedrab", 
#        pch = 20, 
#        cex = 0.75)
# # And draw a little box around the graph
# box()

# withold 20% of the data for testing the model
stationocc=data.frame(obs.data$longitude,obs.data$latitude)
fold <- kfold(stationocc, k=5)
stationtest <- stationocc[fold == 1, ]
stationtrain <- stationocc[fold != 1, ]

#Get basic weather data
bioclim.data.1 <- getData(name = "worldclim",
                          var = "bio",
                          res = 0.5,
                          lat = (min.lat+max.lat)/2,
                          lon = min.lon)

#pge strattles the line need second tile
bioclim.data.2 <- getData(name = "worldclim",
                          var = "bio",
                          res = 0.5,
                          lat = min.lat,
                          lon = max.lon)

#This section of code hangs a long time...
temp <- raster::merge(bioclim.data.1,bioclim.data.2)
#bioclim.data <- mosaic(bioclim.data.1,bioclim.data.2,fun=mean)

#Subset raster stacks to California domain
states.sf <-states(cb=FALSE)
ca.sf <- states.sf %>%
    filter(STUSPS=='CA')

# options(tigris_class="sp")
# states.sp <-states(cb=FALSE)
# ca.sp <- states.sp %>%
#   filter_state("california")

bioclim.data <- mask(temp,ca.sf)                       #mask will mask out data points outside CA shapefile
bioclim.data <- crop(bioclim.data,extent(geographic.extent))  #crop will grab all data within the cube defined by extent

#Identify background points
bg <- randomPoints(bioclim.data, 1000) #background "pseudoabsences"

#fit the maxent model
# beta multiplier can be tweaked to improve fit of model - higher values run the risk of overspecified models however
rattler.me <- maxent(bioclim.data, stationtrain, a=bg,
                     args = c("betamultiplier=0.5"))
# AUC = 0.566

## Notes on how to evaluate Maxent model output
## AUC scores of 0.5 indicate model is performing no better than random chance
## AUC scores above ~0.8 indicate robust model fit
## But, look out for cases where AUC scores are very high (>0.9) and variable importance plots show only one or two variables with strong correlations
## This indicates models are not picking up on an interpretable signal, but rather whatever env variable has the strongest gradient in the area

# plot showing importance of each variable
plot(rattler.me)

# response curves
response(rattler.me)

# predict to entire dataset
rattler.pred <- predict(rattler.me, bioclim.data)

#Convert the predicted coverage to a spatial data frame
test_spdf <- as(rattler.pred, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)

#Save workspace for later
#save(bioclim.data,stationocc,stationtest,stationtrain,rattler.me,rattler.pred,obs.data,file="/media/owen/data2/wildfire/statewide_test.RData") 

#plot predictions
plot(rattler.pred, main="Statewide Similarity")
map('worldHires', fill=FALSE, add=TRUE)
points(obs.data$longitude, obs.data$latitude, pch="+", cex=0.2)


#Load Weather Region Shapefiles
ff <- list.files(weather_dir, pattern="\\.shp$", full.names=TRUE)
fire.wx.l <-lapply(ff,shapefile)
fire.wx.reg <- do.call(bind,fire.wx.l)

#Load GIS objects
load(file = paste0(map_data,"map_objects.RData"))

#Plot all stations
labtext = 'Statewide Station Similarity'
#subtext = 'All Long Term ISD Stations'
captext = 'Produced in support of EPC-18-026'
fout = 'statewide_map.pdf'
a <- ggplot(data = active.sub, aes(x = Longitude, y = Latitude)) + 
  geom_polygon(data = CA, aes(x=long, y = lat, group = group), fill = 'grey70', color='black', size=1) +
  geom_tile(data=test_df, aes(x=x, y=y, fill=layer), alpha=0.8) + 
  #scale_fill_gradientn("similarity score",colours=c("#FFECB3","#E85285","#3e0020"), na.value = NA, limits=c(0,1)) +
  #scale_fill_gradientn("similarity score",colours=c("#FFECB3","#FFA500","#1e003e"), na.value = NA, limits=c(0.1,0.7)) +
  #scale_fill_continuous_sequential(palette = "Viridis", begin = 0.1, end = 0.7)+
  scale_fill_continuous_sequential(palette = "Viridis")+
  #scale_fill_gradientn("similarity score",colours = colorspace::Viridis(7), na.value = NA, limits=c(0.1,0.7)) +
  geom_polygon(data = CA.counties, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
  geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.05, color="black", alpha=1, show.legend = TRUE)+
  geom_point(alpha=0.5, size=0.1, color="black")+
  
  coord_fixed(xlim = c(min.lon,max.lon),  ylim = c(min.lat, max.lat), ratio=1.3)+
  
  
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  theme_bw() +
  labs(title=labtext,caption=captext,x="Longitude",y="Latitude")

#a
ggsave(file=paste0(fig_dir, fout), plot = a, device = "pdf", dpi = 300)

#Plot: Statewide by Fire Regions 
labtext = 'Statewide Station Similarity'
#subtext = 'All Long Term ISD Stations'
captext = 'Produced in support of EPC-18-026'
fout = 'statewide_map_by_firewx.pdf'
a <- ggplot(data = active.sub, aes(x = Longitude, y = Latitude)) + 
  geom_polygon(data = CA, aes(x=long, y = lat, group = group), fill = 'grey70', color='black', size=1) +
  geom_tile(data=test_df, aes(x=x, y=y, fill=layer), alpha=0.8) + 
  scale_fill_continuous_sequential(palette = "Viridis")+
  geom_polygon(data = fire.wx.reg, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
  #geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.01, color="black", alpha=1, show.legend = TRUE)+
  geom_point(alpha=0.5, size=0.1, color="black")+
  
  coord_fixed(xlim = c(min.lon,max.lon),  ylim = c(min.lat, max.lat), ratio=1.3)+
  
  
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  theme_bw() +
  labs(title=labtext,caption=captext,x="Longitude",y="Latitude")

#a
ggsave(file=paste0(fig_dir, fout), plot = a, device = "pdf", dpi = 300)

####Plot by Service Area ####

#Load GIS objects
service <- readOGR(dsn = paste0(map_data, "service_areas/", "."))
service <- spTransform(service,CRSobj = CA@proj4string)

service.sf <- st_as_sf(service)
service.sf <- service.sf %>%
  filter(Type == "IOU")


service.st <- as(service.sf, "Spatial")

labtext = 'Statewide Station Similarity'
#subtext = 'All Long Term ISD Stations'
captext = 'Produced in support of EPC-18-026'
fout = 'statewide_map_by_IOU.pdf'
a <- ggplot(data = active.sub, aes(x = Longitude, y = Latitude)) + 
  geom_polygon(data = CA, aes(x=long, y = lat, group = group), fill = 'grey70', color='black', size=1) +
  geom_tile(data=test_df, aes(x=x, y=y, fill=layer), alpha=0.8) + 
  scale_fill_continuous_sequential(palette = "Viridis")+
  geom_polygon(data = service.st, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
  #geom_sf(data = service.sf, color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
  #geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.01, color="black", alpha=1, show.legend = TRUE)+
  geom_point(alpha=0.5, size=0.1, color="black")+
  
  coord_fixed(xlim = c(min.lon,max.lon),  ylim = c(min.lat, max.lat), ratio=1.3)+
  
  
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  theme_bw() +
  labs(title=labtext,caption=captext,x="Longitude",y="Latitude")

#a
ggsave(file=paste0(fig_dir, fout), plot = a, device = "pdf", dpi = 300)

#### Eval proposed weather stations ####
# Read in SDG&E proposed weather stations
new.data <- read.csv(file = paste0(station_dir, "2020 New Weather Stations_redacted.csv"))

#Create a subset of the stations with just latitude and longitude
prop.data <- new.data %>%
  select('Lat','Lon')
colnames(prop.data)[1:2] <-c("latitude","longitude")

# Check the data to make sure it loaded correctly
summary(prop.data)

#Plot map with proposed stations
#Plot all stations

labtext = 'SDG&E Proposed Station Similarity'
#subtext = 'All Long Term ISD Stations'
captext = 'Produced in support of EPC-18-026'
fout = 'sdg&e_map_proposed.pdf'
a <- ggplot(data = obs.data, aes(x = longitude, y = latitude)) + 
  geom_polygon(data = CA, aes(x=long, y = lat, group = group), fill = 'grey70', color='black', size=1) +
  geom_tile(data=test_df, aes(x=x, y=y, fill=layer), alpha=0.8) + 
  scale_fill_gradientn("similarity score",colours=c("#FFECB3","#E85285","#3e0020"), na.value = NA, limits=c(0,1)) +
  geom_polygon(data = CA.counties, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
  geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.05, color="black", alpha=1, show.legend = TRUE)+
  geom_point(alpha=0.8, size=0.2, color="black")+
  geom_point(data=prop.data, aes(x=longitude,y=latitude),alpha=1, size=0.75, color="blue")+
  
  coord_fixed(xlim = c(min.lon,max.lon),  ylim = c(min.lat, max.lat), ratio=1.3)+
  
  
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  theme_bw() +
  labs(title=labtext,caption=captext,x="Longitude",y="Latitude")

#a
ggsave(file=paste0(fig_dir, fout), plot = a, device = "pdf", dpi = 300)


#Match new points with a score
prop.rev = prop.data[,c(2,1)] #reverse lon/lat
prop.rev <- prop.rev[!is.na(prop.data$latitude), ]  #drop NAN

prop_spdf <- SpatialPointsDataFrame(
  prop.rev, proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),data=prop.rev) #convert to spatial data frame

#extract one modeled similarity score for proposed new location
extracted.vals<-extract(rattler.pred,prop_spdf,method="simple")


#Merge extracted data with station information from SDG&E
prop.data<-na.omit(prop.data)
scores.df<-cbind(prop.data,extracted.vals)

colnames(scores.df)[1:2] <-c("Lat","Lon")
scores.merged.df<-merge(scores.df,new.data,by=c("Lat","Lon"))

#Make a pretty table

#Create a subset of the stations with just latitude and longitude
less.merged.df <- scores.merged.df %>%
  select('Lat','Lon','extracted.vals','Proposed.Weather.Station','Community') %>%
  arrange(desc(extracted.vals)) %>%
  `colnames<-`(c("Lat", "Lon", "Similarity", "Station Name", "Community"))


x <- head(less.merged.df, n = 5) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"))
save_kable(x, file=paste0(fig_dir, "highest_sdge_stations.png"))

x <- tail(less.merged.df, n = 5) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"))
save_kable(x, file=paste0(fig_dir,"lowest_sdge_stations.png"))
