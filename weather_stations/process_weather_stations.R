
# Read in statewide weather stations
setwd('~/research/wildfire')
in.data <- read.csv(file = "statewide_stations.csv")

#Remove all rows with na's in them
in.data <- in.data[rowSums(is.na(in.data)) == 0,]

#Remove stations with incorrect metadata
in.data<-in.data[!(in.data$Station.Name=="CW5652 Two Harbors"),]  #this station has a wrong longitude

#filter to only include recently active stations
in.data$last.year <- as.numeric(format(as.Date(in.data$Last.Data.Date,'%Y-%m-%d'),'%Y'))

library(dplyr)
# revise and revisit later - maybe include stations that had data in last quarter of 2020?
active.stations <- in.data %>% filter(last.year>=2021)

#options to filter based on data quality
data.q<-read.csv(file="data_quality_networks.csv")
data.q.short <- data.q %>%
  select(c("Abbreviation","Quality","Tier"))
colnames(data.q.short)[1] <-"Mesonet"

active.all <- merge(active.stations,data.q.short,by="Mesonet",all=TRUE)

#set level of data quality; 1 highest, 2 iou/state, 3 universities, 4 private, 5 citizen
quality.accept = 3

active.sub <- active.all %>%
  filter(Quality<=quality.accept)

#######
#Create a map of all stations
#######

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


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
library('dplyr')
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


#Save output as its own dataset
setwd('~/research/wildfire/rds')
saveRDS(active.all, file = "wx_station.rds")

### Append updated IOU stations
wx_station <- readRDS('map_data/wx_station.rds')

# Quality tiers
quality_tiers <- wx_station %>% distinct(Mesonet, Quality, Tier)

# read in synoptic csvs, organize colnames, join in quality tiers
new_data <- map_dfr(paste0('weather_stations/', 
                           list.files('weather_stations/')[grepl('synoptic', list.files('weather_stations/'))]),
                    ~read_csv(., col_types = c('ELEV_DEM' = 'c'))) %>%
  dplyr::select(MNET_SHORTNAME, STID, SHORTNAME, ELEVATION, LONGITUDE, LATITUDE, COUNTY, STATUS) %>%
  rename('Mesonet' = MNET_SHORTNAME, 'Station.ID' = STID, 'Station.Name' = SHORTNAME,
         'Elevation' = ELEVATION, 'Longitude' = LONGITUDE, 'Latitude' = LATITUDE, 'County' = COUNTY,
         'Status' = STATUS) %>%
  mutate_at(c('Elevation'), ~as.factor(.)) %>%
  filter(!(Station.ID %in% wx_station$Station.ID)) %>%
  left_join(quality_tiers, by = 'Mesonet')

# append unique stations to wx_station
wx_station_appended <- wx_station %>%
  bind_rows(new_data)

# save file
write_rds(wx_station_appended, 'map_data/wx_station_2023.rds')

