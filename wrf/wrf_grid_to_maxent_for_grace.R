#### Import packages ####

library(ncdf4)
library(sp)
library(rgdal)
library(raster)
library(tidyverse)
library(sf)
library(stars)
library(colorspace)

#### Set up directories ####

wrf_dir <- "wrf/"
wrf_out <- "wrf_out/"
map_dir <- "map_data/"
fig_dir <- "figs/"

#### Regrid WRF to rasters ####

#Generate fixed fields from a static file
#Need a reference native WRF grid for regridding
varin <- paste0(wrf_dir, 'WRF_grid.nc')   #sample file
ncid<-nc_open(varin,write=FALSE)

#Load variables
wrflat<-ncvar_get(ncid,"XLAT")
wrflon<-ncvar_get(ncid,"XLONG")

# Regrid fn: arguments - wrf variable to process, directory where summarized wrf files are, output directory for bricked regrid data
regrid_wrf <- function(wrf_var = c("CANWAT", "PBLH", "PREC_ACC_NC", "PSFC", "Q2", "T2", "U10", "V10"),
                       in_directory = wrf_dir,
                       out_directory = wrf_out) {

  #Read in the WRF Summarized Files
  #vars=c("CANWAT", "PBLH", "PREC_ACC_NC", "PSFC", "Q2", "T2", "U10", "V10") 
  # moisture in canopy - veg fuel content; boundary layer height - thermal structure/vertical mixing; precip volumetric; psfc suface pressure
  # q2 specific humidity; t2 temperature; u ew component of wind; v ns wind
  # wind statistical summaries are unrelated - want all of these
  # temperature annual range and max value
  # humidity annual range and max value
  
  wrf.base <- readRDS(paste0(wrf_dir,paste0(wrf_var, '_historical_wrf_summary.RDS')))
  
  print("reading in file...")
  
  #Rebuild the WRF Lambert Cylindrical Grid
  wrf.base$xlat<- apply(wrf.base, 1, function(x) wrflat[x['east'],x['nor']])
  wrf.base$xlon<- apply(wrf.base, 1, function(x) wrflon[x['east'],x['nor']])
  
  #Convert the data.frame to a spatial data frame
  # coordinates(wrf.base) = ~xlon+xlat # this errors
  
  #Convert irregular WRF grid to spatial pixels data frame
  pixels <- SpatialPixelsDataFrame(data.frame(wrf.base$xlon, wrf.base$xlat), tolerance = 0.916421, wrf.base)
  #proj4string(pixels) = CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.700012 +lon_0=-98.0 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m ")
  proj4string(pixels) = CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=29.9 +lon_0=-130.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m ") #passing the middle of grid from points2grid
  
  #This works!
  rg <- raster(ncols=329,nrows=400) #create a raster to project onto
  extent(rg) <- c(min(wrflon),max(wrflon),min(wrflat),max(wrflat))
  
  #rasterize the object - now converted to typical lat-lon grid
  median.raster <- rasterize(pixels,rg,field='median')
  mean.raster <- rasterize(pixels,rg,field='mean')
  IQR.raster <- rasterize(pixels,rg,field='IQR')
  sd.raster <- rasterize(pixels,rg,field='sd')
  mad.raster <- rasterize(pixels,rg,field='mad')
  min.raster <- rasterize(pixels,rg,field='min')
  max.raster <- rasterize(pixels,rg,field='max')
  q5.raster <- rasterize(pixels,rg,field='q5')
  q95.raster <- rasterize(pixels,rg,field='q95')
  
  print("layers rasterized...")
  
  #If all NA then skip
  #problems with NAs - only for CANWAT (or other variables which are mostly zeros)
  
  if(sum(is.na(pixels$kurtosis)) == 0) {
    kurtosis.raster <- rasterize(pixels,rg,field='kurtosis')
    skewness.raster <- rasterize(pixels,rg,field='skewness')
    
    out.brick <- brick(list(median.raster,mean.raster,IQR.raster,sd.raster,mad.raster,min.raster,max.raster,q5.raster,q95.raster,
                            kurtosis.raster, skewness.raster))
    names(out.brick) <- c('median','mean','IQR','sd','mad','min','max','q5','q95', 'kurtosis', 'skewness') 
    
    writeRaster(out.brick, filename=paste0(wrf_out, wrf_var, '_regrid.grd'), format="raster", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
    
    print("file written")
    
  } else {
    out.brick <- brick(list(median.raster,mean.raster,IQR.raster,sd.raster,mad.raster,min.raster,max.raster,q5.raster,q95.raster))
    names(out.brick) <- c('median','mean','IQR','sd','mad','min','max','q5','q95') 
    
    writeRaster(out.brick, filename=paste0(wrf_out, wrf_var, '_regrid.grd'), format="raster", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
    
    print("file written")
  }
  
  #test plot
  #plot(max.raster)
  
  }


## Apply regrid to all vars
regrid_wrf("Q2")

#read command for later
ImportName <- brick(paste0(wrf_out, "FileNAme.grd"))

#### Figure output #####

#Sample Figure Output
# Cut to a single domain
firewx.sf <- readRDS(file=paste0(map_data, "firewx.sf.RDS")) #stand alone fire wx areas (no info about IOU territories)

location.string = "Northwest"
region_shape = firewx.sf %>%
    dplyr::filter(region==location.string)

#rasterize the shape
region.raster <- rasterize(region_shape,IQR.raster)

#crop the raster based on shape
cropped.raster <- raster::mask(skewness.raster,region.raster)
plot(cropped.raster)

#convert raster to a spatial data frame for plotting
test_spdf <- as(cropped.raster, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
#colnames(test_df) <- c("value", "x", "y")

t = 5
thres.string = "all"

#Mapping assets
load(file = paste0(map_dir, "map_objects.RData"))

shape.extent <- extent(region_shape)
fr <- st_rasterize(region_shape,crs = st_crs(IQR.raster))

region_layer <- st_crop(x=st_as_stars(IQR.raster), y=fr, crop=TRUE)    #create a poly-raster

#Plot all stations
labtext = paste("WRF Similarity Score For ",location.string)
subtext = paste('Weather Station Quality Threshold', t, 'of 5: ', thres.string) 
captext = 'Pyregence.org [EPC-18-026] - Expertimental Data'
fout = paste0("ss_northwest_wrftestcase.png")

a <- ggplot(data = CA, aes(x=long, y = lat)) + 
  geom_polygon(aes(group = group), fill = 'grey70', color='black', size=1) +
  geom_tile(data=test_df, aes(x=x, y=y, fill=layer), alpha=0.8) + 
  #geom_raster(data=cropped.raster, aes(x=xlat, y=ylat, fill=data))+ 
  scale_fill_continuous_sequential(palette = "Viridis")+
  geom_polygon(data = CA.counties, aes(x=long, y=lat, group=group), color="black",fill=NA, size=0.5, alpha=0, show.legend = TRUE)+
  geom_path(data = highways.in, aes(x=long, y=lat, group=group), inherit.aes = FALSE, size=0.05, color="black", alpha=1, show.legend = TRUE)+
  geom_point(alpha=0.5, size=0.1, color="black")+
  
  coord_fixed(xlim = c(shape.extent@xmin,shape.extent@xmax),  ylim = c(shape.extent@ymin, shape.extent@ymax), ratio=1.3)+
  
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10)) +
  theme_bw() +
  labs(title=labtext,caption=captext,subtitle=subtext, x="Longitude",y="Latitude")

a
ggsave(file=paste0(fig_dir, fout), plot = a, device = "png", dpi = 300) 


#### Scratch code #####


plot(test.raster,xlim=c(-150,-100),ylim=c(29,50))


wrf.trans = spTransform(pixels,geo_proj)
wrf.trans = projectRaster(test.raster, crs=geo_proj, method="bilinear")

out<-rasterize(wrf.trans$max)


wrf.trans = projectRaster(pixels, crs=geo_proj, method="bilinear", alignOnly = TRUE)

#this will give you the center of the subsetted grid
grid.in<-points2grid(wrf.base, tolerance=0.5, round=1)

out <- raster(pixels@data,)


test.spdf<-SpatialPixels(points=wrf.base, data=wrf.base, tolerance = sqrt(.Machine$double.eps),
              proj4string = CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.700012 +lon_0=-98.0 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m "), round = NULL, grid = grid.in)

test_spdf <- as(wrf.base, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)


#extracted from WRF invariant netcdf
#https://epsg.io/102009
#proj4string(wrf.base)<- CRS("+proj=lcc +lat_1=28 +lat_2=50 +lat_0=39.700012 +lon_0=-98.0 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m ")

#Transform the projection to match maxent projections
geo_proj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#Get domain information from climatology

setwd('/home/owen/research/wildfire/rds/')
bioclim.data.CA<-brick("CA_worldclim_crop.tif")
cr <- raster::crop(bioclim.data.CA, extent(region_shape), snap="out")    #crop out a subset of data  

raster.domain=extent(bioclim.data.CA)

test.raster <- rasterize(wrf.base,raster.domain)
wrf.trans = projectRaster(test.raster, crs=geo_proj, method="bilinear")

#wrf.trans = spTransform(wrf.base,geo_proj)

#Convert the predicted coverage to a spatial data frame
test_spdf <- as(wrf.trans, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)

ggplot(wrf.trans,aes(x=xlon,y=xlat))+
  geom_tile(fill=data$max)



# add_xlon_xlat<-function(ni,ei,wrflat,wrflon){
#   xlat <- wrflat[ni,ei]
#   xlon <- wrflon[ni,ei]
# }




cr <- raster::crop(bioclim.data.CA, extent(region_shape), snap="out")    #crop out a subset of data                
fr <- rasterize(st_zm(region_shape), cr)   #rasterize the plolygon && use `st_zm(...)` to coerce to XY dimensions