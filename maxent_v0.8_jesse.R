# September 15, 2021
# Owen Doherty - Eagle Rock Analytics
# support@pyregence.org or owen@eaglerockanalytics.com
# By region w/ outputs for plotting
# version 0.9 release shortly

# Acknowledgement
# Based off code by Jeff Oliver -- jcoliver@email.arizona.edu

# Updates
# Grace Di Cecco - Eagle Rock Analytics
# April 5, 2022

#### Libraries #####
#install.packages(c('sf', 'raster', 'sp', 'dismo', 'stars', 'dplyr', 'tidyverse', 'rJava', 'mapdata', 'ggplot2', 'colorspace', 'kableExtra'))

library("sp")
library("raster")
library("dismo")
library(stars)
library(dplyr)
library(tidyverse)
library(rJava)
library(mapdata)
library(ggplot2)
library(colorspace)
library(kableExtra)


#### setup directories #### 

map_data <- "map_data/"
weather_stations <- "weather_stations/"
output_dir <- "data_out/"
fig_dir <- "figs/"

#Mapping assets
load(file = paste0(map_data, "map_objects.RData"))

#### Params #####

# Parameters from Website: Analysis Type; Station Quality Flag, Location Flag

# Analysis type
# Options: Fire Weather or Climatology
# will direct to wrf or daymet data

# Station Quality Flag
# set level of data quality; 1 highest, 2 iou/state, 3 universities, 4 private, 5 citizen
#quality.accept <- input from webserver
quality.accept <- 2
quality.string <- c('highest','iou/state','universities', 'private', 'citizen')

# Location Flag
# Regions and IOUs
#location.flag <- input from webserver
location.flag <- 3

# Variables to Pass Flag


#### Section 1: Geographic domain ####


#Flag values: 
# 1: Bear Valley Electric Service
# 2: Liberty Utilities (a.k.a. CalPeco for California Pacific Electric Co.)
# 3: Pacific Gas and Electric Company (PG&E)
# 4: PacifiCorp
# 5: San Diego Gas & Electric (SDG&E)
# 6: Southern California Edison (SCE)

#Fire Weather shapes:
#7: Bay_Area
#8: Central_Coast
#9: LA
#10: Modoc
#11: Northwest
#12: San_Diego
#13: Sierras_East
#14: Sierras_West

all_shapes <- readRDS(file=paste0(map_data, "firewx_regions.RDS")) #all shapes has joined objects, so defines regions where IOU and fire weather overlap
IOU_areas.sf <-readRDS(file=paste0(map_data, "IOU_areas.RDS")) #stand alone IOU areas (no info about firew wx)
firewx.sf <- readRDS(file=paste0(map_data, "firewx.sf.RDS")) #stand alone fire wx areas (no info about IOU territories)

#### Section 2: Find the Weather Station Data ####

#Load weather data as its own dataset

wx.station <- readRDS(file = paste0(map_data, "wx_station.rds"))

# just for eval of potential new PG&E sites
# 
# new_sites <- read.csv("weather_stations/potential_pge_sites.csv", stringsAsFactors = F)
# new_sites_formatted <- new_sites %>%
#   dplyr::select(-asset_type) %>%
#   rename(Station.Name = "ï..stn_name", Latitude = "lat", Longitude = "lon") %>%
#   mutate(Mesonet = NA, Station.ID = NA,
#          Elevation = NA, County = NA, 
#          First.Data.Date = NA, Last.Data.Date = NA,
#          Status = NA, last.year = NA, Quality = 2, Tier = "IOU")
# 
# wx.station <- readRDS(file = paste0(map_data, "wx_station.rds")) %>%
#   bind_rows(new_sites_formatted)

# #set level of data quality; 1 highest, 2 iou/state, 3 universities, 4 private, 5 citizen
# #quality.accept = 3
# 
# station.sub <- wx.station %>%
# #  filter(Quality<=quality.accept) %>%
#   drop_na(Latitude,Longitude)  #drop rows where Lat and Lon are missing
# 
# #create a spatial points data.frame for extraction by region below
# xy <-station.sub %>%
#   dplyr::select(Longitude,Latitude)
# 
# wx.station.spdf <- SpatialPointsDataFrame(coords=xy, data=station.sub, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#### Section 3: Define Regions of Interest ####

loc.number<-c('Bear Valley Electric Service','Liberty Utilities','Pacific Gas & Electric Company','PacifiCorp','San Diego Gas & Electric','Southern California Edison','Bay_Area','Central_Coast','LA', 'Modoc', 'Northwest', 'San Diego', 'Sierras_East','Sierras_West')
location.string = loc.number[location.flag]

#Load shape files for user selected regions
#6 or lower is an IOU; 7 or higher is a fire weather region

if(location.flag <= 6) {
  region_shape = IOU_areas.sf %>%
    filter(region==location.string)
  
} else {
  region_shape = firewx.sf%>%
    filter(region==location.string)
}

#### Section 4: Basic Climate Data ####

## The model run in Section 5 will work with different climate data, read in in this stage
## Underlying climate data needs to be a raster brick of gridded data for the study area
## Models at smaller scales (individual IOU regions or fire weather regions) require fine-scale environmental data
## Fine-scale meaning <=1 km resolution

#See process_worldclim.R
#bioclim.data.CA <- stackOpen("CA_worldclim_crop.stk")
bioclim.data.CA<-brick(paste0(map_data, "CA_worldclim_crop.tif"))

#bioclim.data.CA <-readRDS(file="CA_worldclim_crop.RDS") #avoid RDS for raster stacks
#bioclim.data.CA <- readAll(bioclim.data.CA) #reads entire object into RAM - avoid writing rasters as RDS in the future

#bioclim.data.CA <-raster("CA_worldclim_crop.grd")


#### Section 5: Loops ####

auc_table <- data.frame(loc = c(), quality = c(), auc = c())

for (l in 2:length(loc.number)) {
#for (l in 3) {
  for (t in 1:5 ) {

    # #debug values
    # l = 6
    # t = 2
    
    #### Section 5a: Load shape files for user selected regions ####
    
    #6 or lower is an IOU; 7 or higher is a fire weather region
    location.string = loc.number[l]
    print(location.string)
    file.location.string = gsub(" ", "", location.string, fixed = TRUE)
    
    thres.string = quality.string[t]
    print(thres.string)
    
    if(l <= 6) {
      region_shape = IOU_areas.sf %>%
        filter(region==location.string)
      
    } else {
      region_shape = firewx.sf%>%
        filter(region==location.string)
    }
    
    shape.extent <- extent(region_shape)
    
    #### Section 5b: Crop climate data & weather stations for region of interest ####
    
    cr <- raster::crop(bioclim.data.CA, extent(region_shape), snap="out")    #crop out a subset of data                
    fr <- rasterize(st_zm(region_shape), cr)   #rasterize the plolygon && use `st_zm(...)` to coerce to XY dimensions
    
    
    #fr.sp <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(fr), as_points = FALSE, merge = TRUE)) #create a spatial polygon for use later  -- FAST (creates overlaps)
    fr.sp <- rasterToPolygons(fr, dissolve=TRUE) #create a spatial polygon for use later -- SLOW OPTION (no overlaps)
    fr.sf <-  st_as_sf(fr.sp)
    
    #check: does the masked data get passed to maxent
    region_bioclim <- mask(x=cr, mask=fr)    #create a poly-raster
    
        #to-do: test plots output here
        #plot(region_bioclim)
        
    #filter weather stations by data quality flag
        #set level of data quality; 1 highest, 2 iou/state, 3 universities, 4 private, 5 citizen
    station.sub <- wx.station  %>%
      dplyr::filter(Quality<=t) %>%
      drop_na(Latitude,Longitude)  #drop rows where Lat and Lon are missing
    
    #create a spatial points data.frame for extraction by region below
    xy <-station.sub %>%
      dplyr::select(Longitude,Latitude)
    
    wx.station.spdf <- SpatialPointsDataFrame(coords=xy, data=station.sub, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))   
    #now convert spatial points df to simple feature
    wx.station.sf <- st_as_sf(wx.station.spdf)
    
    #extract stations within the polygon of region (fr)
    wx.stations.within.sf <- st_intersection(fr.sf,wx.station.sf)
    
    #### Section 5c: Maxent ####
    
    # withold 10% of the data for testing the model
    stationocc<-st_drop_geometry(wx.stations.within.sf %>%
      dplyr::select(Longitude,Latitude)
    )
    
     fold <- kfold(stationocc, k=9)          #withold 10%, k = 10; withold 20% k= 5; withold 11.1%, k=9
     stationtest <- stationocc[fold == 1, ]
     stationtrain <- stationocc[fold != 1, ]
    # 
    #this logic will help with regions where there are less than 10 stations
    #fold fails if the number of weather stations < number of folds 
    
    # if (length(stationocc)>10){
    #   fold <- kfold(stationocc, k=10)          #withold 10%, k = 10; withold 20% k= 5
    #   stationtest <- stationocc[fold == 1, ]
    #   stationtrain <- stationocc[fold != 1, ]
    # } else {
    #   stationtest <- stationocc[1, ]
    #   stationtrain <- stationocc[2:length(stationocc), ]
    # }
    # 
    #Identify background points
    bg <- randomPoints(region_bioclim, 10000) #background "pseudolocations" -1000 probably sufficient
    
    #fit the maxent model
    # beta multiplier can be tweaked to improve fit of model - higher values run the risk of overspecified models however
    maxent.me <- maxent(region_bioclim, stationtrain, a=bg,
                        args = c("betamultiplier=0.5"))
    
    # predict to entire dataset
    maxent.pred <- predict(maxent.me, region_bioclim)
    
    #### Section 6: Diagnostic Plots ####
    
    ## Notes on how to evaluate Maxent model output
    ## AUC scores of 0.5 indicate model is performing no better than random chance
    ## AUC scores above ~0.8 indicate robust model fit
    ## But, look out for cases where AUC scores are very high (>0.9) and variable importance plots show only one or two variables with strong correlations
    ## This indicates models are not picking up on an interpretable signal, but rather whatever env variable has the strongest gradient in the area
    
    # plot showing importance of each variable
    
    pdf(file=paste0(fig_dir, "var_import_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
      plot(maxent.me, main = paste("Predictor Importance \n ",location.string))
    dev.off()

    # response curves
    pdf(file=paste0(fig_dir, "res_curves_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
      response(maxent.me, main = paste("Response Curves \n ",location.string))
    dev.off()


    # ROC plots
    pvtest <- data.frame(raster::extract(region_bioclim, stationtrain))
    avtest <- data.frame(raster::extract(region_bioclim, bg))
    
    testp <- predict(maxent.me, pvtest) 
    testa <- predict(maxent.me, avtest) 
    
    e3 <- evaluate(p=testp, a=testa)
    # 
    pdf(file=paste0(fig_dir, "roc_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
    plot(e3, 'ROC')
    mtext(location.string, side = 3)
    dev.off()

    ## save AUC values for each model
    auc_table <- bind_rows(auc_table, data.frame(loc = l, quality = t, auc = e3@auc))
    
    #Convert the predicted coverage to a spatial data frame
    test_spdf <- as(maxent.pred, "SpatialPixelsDataFrame")
    test_df <- as.data.frame(test_spdf)
    
    # threshold output
    thresh_out <- threshold(e3, stat = 'spec_sens') # eval threshold level - maximize sum of sensitivity and specificity
    
    # threshold breaks
    breaks <- matrix(c(0, thresh_out, 1,
                thresh_out, 1, 0), ncol = 3, byrow = T)
    
    # threshold raster
    thresh_raster <- reclassify(maxent.pred, breaks)
    
    #Save workspace for later
    #save(bioclim.data,stationocc,stationtest,stationtrain,maxent.me,maxent.pred,obs.data,file="/media/owen/data2/wildfire/statewide_test.RData") 
    
    #plot predictions
    

pdf(file=paste0(fig_dir, "ss_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
  plot(maxent.pred, main=paste("Similarity Score For \n ",location.string))
  map('worldHires', fill=FALSE, add=TRUE)
  points(stationtest$longitude, stationtest$latitude, pch="+", cex=0.2)
  points(stationtrain$longitude, stationtrain$latitude, pch="o", cex=0.2)
dev.off()

    #### Section 7: Output nice map ####
    
    #Plot all stations
    labtext = paste("Similarity Score For ",location.string)
    subtext = paste('Weather Station Quality Threshold', t, 'of 5: ', thres.string) 
    captext = 'Pyregence.org [EPC-18-026]'
    fout = paste0(fig_dir, "ss_",file.location.string,"_wxstathresh_",t,'.pdf')
    
    a <- ggplot(data = wx.stations.within.sf, aes(x = Longitude, y = Latitude)) + 
      geom_polygon(data = CA, aes(x=long, y = lat, group = group), fill = 'grey70', color='black', size=1) +
      geom_tile(data=test_df, aes(x=x, y=y, fill=layer), alpha=0.8) + 
      scale_fill_continuous_sequential(palette = "Viridis")+
      geom_polygon(data = CA.counties, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
      geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.05, color="black", alpha=1, show.legend = TRUE)+
      geom_point(alpha= 0.8, size=0.25, color="black")+
       
      coord_fixed(xlim = c(shape.extent@xmin,shape.extent@xmax),  ylim = c(shape.extent@ymin, shape.extent@ymax), ratio=1.3)+
      
      theme(plot.title = element_text(size = 25, face = "bold"),
            legend.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            axis.title.x = element_text(size = 20, vjust = -0.5),
            axis.title.y = element_text(size = 20, vjust = 0.2),
            legend.text = element_text(size = 10)) +
      theme_bw() +
      labs(title=labtext,caption=captext,subtitle=subtext, x="Longitude",y="Latitude")
    
    #a
    ggsave(file=fout, plot = a, device = "pdf", dpi = 300)
    # 
    # #### Section 8: Data dump ####
    # 
    fout = paste0(output_dir, "model_",file.location.string,"_wxstathresh_",t,'.RData')
    save(list=c("maxent.me", "maxent.pred", "thresh_raster"), file=fout)

    fout = paste0(output_dir, "prediction",file.location.string,"_wxstathresh_",t,'.RDS')
    saveRDS(test_spdf,fout)
  } # end weather station quality threshold loop
} #end location loop

#### Section 9: Pretty AUC table ####

# write auc table
write.csv(auc_table, paste0(output_dir, "maxent_bioclim_auc_values.csv"), row.names = F)

auc_table <- read_csv(paste0(output_dir, "maxent_bioclim_auc_values.csv"))

auc_labels <- auc_table %>%
  mutate(Location = loc.number[loc],
         Quality = quality.string[quality]) %>%
  dplyr::select(Location, Quality, auc) %>%
  rename(AUC = auc)

x <- auc_labels %>%
  filter(Location %in% loc.number[1:6]) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"))
save_kable(x, file=paste0(fig_dir, "maxent_bioclim_auc_iou.png"))

x <- auc_labels %>%
  filter(Location %in% loc.number[7:14]) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"))
save_kable(x, file=paste0(fig_dir, "maxent_bioclim_auc_fire_reg.png"))

#### Section 10: SDG&E proposed weather stations ####

# Read in SDG&E proposed weather stations
setwd('~/research/wildfire')
new.data <- read.csv(file = "2020 New Weather Stations_redacted.csv")

#Create a subset of the stations with just latitude and longitude
library('dplyr')
prop.data <- new.data %>%
  select('Lat','Lon')
colnames(prop.data)[1:2] <-c("latitude","longitude")

# Check the data to make sure it loaded correctly
summary(prop.data)

#Plot map with proposed stations
setwd('/home/owen/research/wildfire/')
#Plot all stations
library(ggplot2)
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
  geom_point(alpha=1, size=0.75, color="black")+
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
ggsave(file=fout, plot = a, device = "pdf", dpi = 300)


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
library(knitr)
library(kableExtra)

#Create a subset of the stations with just latitude and longitude
library('dplyr')
less.merged.df <- scores.merged.df %>%
  select('Lat','Lon','extracted.vals','Proposed.Weather.Station','Community') %>%
  arrange(desc(extracted.vals)) %>%
  `colnames<-`(c("Lat", "Lon", "Similarity", "Station Name", "Community"))


x <- head(less.merged.df, n = 5) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"))
save_kable(x, file="highest_sdge_stations.png")

x <- tail(less.merged.df, n = 5) %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped"))
save_kable(x, file="lowest_sdge_stations.png")

