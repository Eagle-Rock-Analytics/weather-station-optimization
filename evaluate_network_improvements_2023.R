### Visualize changes between 2022 and 2023 model outputs
## Eagle Rock Analytics
## Grace Di Cecco
## August 2023

library(tidyverse)
library(raster)
library(sf)
library(tmap)

# map directory
map_data <- 'map_data/'

# figure directory
figs <- 'figs_2023/'

## For each IOU/fire weather region and quality flag level
## Calculate difference between 2022 similarity score and 2023 similarity score

# lists of simscore prediction files
data22 <- 'data_out/'
data23 <- 'data_out_2023/'

files23 <- list.files(data23)[grepl('^prediction', list.files(data23))]

# calculate differences and write out raster of deltas

map(files23, ~{
  f23 <- .
  
  r23 <- readRDS(paste0(data23, f23))
  
  r22 <- readRDS(paste0(data22, f23))
  
  diff <- raster(r23) - raster(r22)
  
  # retain positive values to see where networks improved
  pos_raster <- reclassify(diff, c(-1, 0, NA))
  
  # write this delta raster
  fname <- substr(f23, 11, nchar(f23) - 4)
  
  writeRaster(pos_raster, paste0(data23, 'delta2023_', fname), overwrite = T)
  
  print(paste0('file written ', f23))
})

## Plot delta values to show places where network coverage improved

# mapping data
load(file = paste0(map_data, "map_objects.RData"))

states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")

all_shapes <- readRDS(file=paste0(map_data, "firewx_regions.RDS")) #all shapes has joined objects, so defines regions where IOU and fire weather overlap
IOU_areas.sf <-readRDS(file=paste0(map_data, "IOU_areas.RDS")) #stand alone IOU areas (no info about firew wx)
firewx.sf <- readRDS(file=paste0(map_data, "firewx.sf.RDS")) #stand alone fire wx areas (no info about IOU territories)

# weather station locations - ID stations added in 2023 file update
wx.station <- readRDS(file = paste0(map_data, "wx_station_2023.rds")) %>%
  mutate_at(c('last.year'), ~case_when(is.na(last.year) & Tier == 'IOU' ~ 2023,
                                       TRUE ~ last.year))
wx.23 <- filter(wx.station, last.year == 2023)

# rasters of delta values to plot
# skip federal level quality, no change to stations
# skip Liberty Utilities, no new stations to evaluate
deltas <- list.files(data23)[grepl('delta2023', list.files(data23)) & 
                               grepl('grd', list.files(data23)) &
                               !grepl('wxstathresh_1', list.files(data23)) &
                               !grepl('Liberty', list.files(data23))]

# plot each delta raster with weather stations
map(deltas, ~{
  d <- .
  
  # plot filename
  plotname <- substr(d, 1, nchar(d) - 4)
  
  # get loc string and wx thresh string
  loc.number<-c('Liberty Utilities','Pacific Gas & Electric Company','PacifiCorp','San Diego Gas & Electric','Southern California Edison','Bay Area','Central Coast','LA', 'Modoc', 'Northwest', 'San Diego', 'Sierras East','Sierras West')
  loc.strings <- gsub(" ", "", loc.number)
  quality.string <- c('highest','iou/state','universities', 'private', 'citizen')
  
  loc <- word(d, 2, 2, sep = "_")
  qual <- as.numeric(substr(d, nchar(d) - 4, nchar(d) - 4))
  
  location.string <- loc.number[grepl(loc, loc.strings)]
  thres.string <- quality.string[qual]
  
  # location shapefile
  if (sum(grepl(location.string, IOU_areas.sf$region)) == 1) { 
    shp <- filter(IOU_areas.sf, grepl(location.string, IOU_areas.sf$region))
    } else {
    shp <- filter(firewx.sf, grepl(loc, firewx.sf$region))
    }
  
  # repair some invalid region shapes
  sf_use_s2(FALSE)
  shp <- st_make_valid(shp)
  
  # read in delta raster
  r <- raster(paste0(data23, d))
  
  # weather stations in region
  wx_sf <- wx.station %>%
    filter(!is.na(Longitude)) %>%
    filter(Quality <= qual) %>%
    st_as_sf(coords = c('Longitude', 'Latitude')) %>%
    st_set_crs(st_crs(shp)) %>%
    st_filter(shp)
  
  new_wx_sf <- wx_sf %>%
    filter(last.year == 2023)
  
  # plot text
  labtext = paste("Similarity Score For ",location.string)
  subtext = paste('Weather Station Quality Threshold', qual, 'of 5: ', thres.string) 
  captext = 'Pyregence.org [EPC-18-026]'
  
  # create plot
  plt <- tm_shape(r) + tm_raster(palette = 'viridis', title = 'Similarity score\nimprovement', style = 'cont') +
    tm_shape(CA.counties) + tm_borders() + 
    tm_shape(shp) + tm_borders() +
    tm_shape(CA) + tm_borders() + 
    tm_shape(wx_sf) + tm_dots(size = 0.05) +
    tm_shape(new_wx_sf) + tm_dots(size = 0.05, col = 'red') +
    tm_add_legend(type = c('symbol'), labels = c('Existing stations', 'New stations'), col = c('black', 'red'),
                  shape = 16) +
    tm_legend(legend.outside = T) +
    tm_credits(text = captext, position = c('left', 'bottom')) +
    tm_layout(main.title = paste0(labtext, '\n', subtext), main.title.size = 0.9)
  tmap_save(plt, filename = paste0(figs, plotname, "_map.pdf"))
})
