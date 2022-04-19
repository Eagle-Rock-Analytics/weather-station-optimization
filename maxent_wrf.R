### Run maxent on weather stations
### Input climate vars: WRF

#### Libraries #####

library(sp)
library(raster)
library(maptools) #depreciating
library(rgdal) #depreciating
library(dismo)
library(stars)
library(dplyr)
library(tidyverse)
library(rgeos) #depreciating
library(rJava)
library(mapdata)
library(ggplot2)
library(colorspace)


#### setup directories #### 

map_data <- "map_data/"
wrf_data <- "wrf_out/"
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

#### Section 4: WRF climate data ####

## read in wrf bricks

wrf_files <- list.files(wrf_out)[grepl("grd", list.files(wrf_out))]

## Select relevant wrf variables summary variables from each wrf file
## Keep all vars for wind
## Temp, psfc use IQR and max
## Precip, humidity IQR and min
## canwat mean, IQR, min
## PBLH [boundary layer height] mean, IQR, max
wrf_list <- purrr::map(wrf_files, ~{
  f <- .
  
  var_name <- word(f, 1, 1, sep = "_")
  
  wrf_raster <- brick(paste0(wrf_out, f))

  
  if(var_name %in% c("U10", "V10")) {
    wrf_final <- wrf_raster
    names(wrf_final) <- paste0(names(wrf_final), "_", var_name)
  } 
  
  if(var_name %in% c("T2", "PSFC")) {
    wrf_final <- subset(wrf_raster, c("IQR", "max"))
    names(wrf_final) <- paste0(names(wrf_final), "_", var_name)
  }
  
  if(var_name %in% c("PREC", "Q2")) {
    wrf_final <- subset(wrf_raster, c("IQR", "min"))
    names(wrf_final) <- paste0(names(wrf_final), "_", var_name)
  }
  
  if(var_name %in% c("CANWAT")) {
    wrf_final <- subset(wrf_raster, c("mean", "IQR", "min"))
    names(wrf_final) <- paste0(names(wrf_final),"_", var_name)
  }
  
  if(var_name %in% c("PBLH")) {
    wrf_final <- subset(wrf_raster, c("mean", "IQR", "max"))
    names(wrf_final) <- paste0(names(wrf_final), "_", var_name)
  }
  
  wrf_final
  
})

## Stack list to one climate variable brick
wrf_brick <- brick(wrf_list)

#### Section 5: Loops ####

auc_table <- data.frame(loc = c(), quality = c(), auc = c())

#for (l in 2:length(loc.number)) {
for (l in 2:length(loc.number)) {
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
    
    cr <- raster::crop(wrf_brick, extent(region_shape), snap="out")    #crop out a subset of data                
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
                                   select(Longitude,Latitude)
    )
    # 
    # fold <- kfold(stationocc, k=9)          #withold 10%, k = 10; withold 20% k= 5; withold 11.1%, k=9
    # stationtest <- stationocc[fold == 1, ]
    # stationtrain <- stationocc[fold != 1, ]
    # 
    #this logic will help with regions where there are less than 10 stations
    #fold fails if the number of weather stations < number of folds 
    
    if (length(stationocc)>10){
      fold <- kfold(stationocc, k=9)          #withold 10%, k = 10; withold 20% k= 5
      stationtest <- stationocc[fold == 1, ]
      stationtrain <- stationocc[fold != 1, ]
    } else {
      stationtest <- stationocc[1, ]
      stationtrain <- stationocc[2:length(stationocc), ]
    }

    #Identify background points
    bg <- randomPoints(region_bioclim, 10000) #background "pseudolocations" -1000 probably sufficient
    
    #fit the maxent model
    maxent.me <- maxent(region_bioclim, stationtrain, a=bg,
                        args = c("betamultiplier=0.5"))
    
    # predict to entire dataset
    maxent.pred <- predict(maxent.me, region_bioclim)
    
    #### Section 6: Diagnostic Plots ####
    
    # plot showing importance of each variable
    
    pdf(file=paste0(fig_dir, "wrf_var_import_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
    plot(maxent.me, main = paste("Predictor Importance \n ",location.string))
    dev.off()
    
    # response curves
    pdf(file=paste0(fig_dir, "wrf_res_curves_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 12, height = 8)
    response(maxent.me, main = paste("Response Curves \n ",location.string))
    dev.off()
    
    
    # ROC plots
    pvtest <- data.frame(raster::extract(region_bioclim, stationtrain))
    avtest <- data.frame(raster::extract(region_bioclim, bg))
    
    testp <- predict(maxent.me, pvtest) 
    testa <- predict(maxent.me, avtest) 
    
    e3 <- evaluate(p=testp, a=testa)
    # 
    pdf(file=paste0(fig_dir, "wrf_roc_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
    plot(e3, 'ROC')
    mtext(location.string, side = 3)
    dev.off()
    
    ## save AUC values for each model
    auc_table <- bind_rows(auc_table, data.frame(loc = l, quality = t, auc = e3@auc))
    
    #Convert the predicted coverage to a spatial data frame
    test_spdf <- as(maxent.pred, "SpatialPixelsDataFrame")
    test_df <- as.data.frame(test_spdf)
    
    #Save workspace for later
    #save(bioclim.data,stationocc,stationtest,stationtrain,maxent.me,maxent.pred,obs.data,file="/media/owen/data2/wildfire/statewide_test.RData") 
    
    #plot predictions
    
    
    pdf(file=paste0(fig_dir, "wrf_ss_",file.location.string,"_wxstathresh_",t, ".pdf"), width = 8, height = 6)
    plot(maxent.pred, main=paste("Similarity Score For \n ",location.string))
    map('worldHires', fill=FALSE, add=TRUE)
    points(stationtest$longitude, stationtest$latitude, pch="+", cex=0.2)
    points(stationtrain$longitude, stationtrain$latitude, pch="o", cex=0.2)
    dev.off()
    
    # #simplest way to use 'evaluate'
    # e1 <- evaluate(me, p=occtest, a=bg, x=predictors)
    # 
    # # alternative 1
    # # extract values
    # pvtest <- data.frame(extract(predictors, occtest))
    # avtest <- data.frame(extract(predictors, bg))
    # 
    # e2 <- evaluate(me, p=pvtest, a=avtest)
    # 
    # # alternative 2 
    # # predict to testing points 
    # testp <- predict(me, pvtest) 
    # head(testp)
    # testa <- predict(me, avtest) 
    # 
    # e3 <- evaluate(p=testp, a=testa)
    # e3
    # threshold(e3)
    # 
    # plot(e3, 'ROC')
    # 
    # 
    # 
    # #testing the model
    # # background data
    # bg <- randomPoints(bioclim.data, 1000) #background "pseudoabsences"
    # 
    # #simplest way to use 'evaluate'
    # e1 <- evaluate(rattler.me, p=stationtest, a=bg, x=bioclim.data)
    # 
    # plot(e1, 'ROC')
    # rattlerChangePoints = extract(rattler.change, rattlerocc)
    # hist(rattlerChangePoints, main="", xlab="Change in habitat suitability")
    # abline(v=0, col="red")
    # 
    # rattlerMitChangePoints = extract(rattler.mit.change, rattlerocc)
    # hist(rattlerChangePoints, main="", x)
    # abline(v=0, col="red")
    
    
    #### Section 7: Output nice map ####
    
    #Plot all stations
    labtext = paste("Similarity Score For ",location.string)
    subtext = paste('Weather Station Quality Threshold', t, 'of 5: ', thres.string) 
    captext = 'Pyregence.org [EPC-18-026]'
    fout = paste0(fig_dir, "wrf_ss_",file.location.string,"_wxstathresh_",t,'.pdf')
    
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
    fout = paste0(output_dir, "wrf_model_",file.location.string,"_wxstathresh_",t,'.RData')
    save(list=c("maxent.me", "maxent.pred"), file=fout)
    
    fout = paste0(output_dir, "wrf_prediction",file.location.string,"_wxstathresh_",t,'.RDS')
    saveRDS(test_spdf,fout)
  } # end weather station quality threshold loop
} #end location loop

#### Pretty AUC table ####

# write auc table
write.csv(auc_table, paste0(output_dir, "maxent_wrf_auc_values.csv"), row.names = F)
