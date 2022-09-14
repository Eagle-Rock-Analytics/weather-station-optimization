## New PG&E stations
## Source: https://mesowest.utah.edu/cgi-bin/droman/meso_station.cgi

library(tidyverse)

## Read in HTML table of observations
table <- read_table('weather_stations/pge.txt',skip = 0, col_names = F, guess_max = 1300)
colnames(table) <- c('STN', 'NAME', 'Name2', 'Name3', 'ST', 'LAT', 'LON', 'ELV', 'MNET')

# Clean up table to get station coordinates
pge_clean <- table %>%
  unite(NAME, Name2, Name3, ST, LAT, LON, ELV, MNET) %>%
  group_by(STN) %>%
  nest() %>%
  mutate(coords = purrr::map(data, ~{
    string <- .
    
    coords <- str_extract_all(., "[0-9]+\\.[0-9]+")
  }),
  lat = map_chr(coords, ~{.[[1]][1]}),
  lon = map_chr(coords, ~{.[[1]][2]})) %>%
  filter(!is.na(lat) & !is.na(lon))

# existing wx stations
readRDS("map_data/wx_station.rds")

# new PGE stations 
new_pge <- pge_clean %>%
  filter(!(STN %in% wx_station$Station.ID)) %>% ## not in existing dataset
  dplyr::select(-data, -coords)
  
# format to existing data table
colnames(new_pge) <- c("Station.ID", "Latitude", "Longitude")
new_pge$Latitude <- as.numeric(new_pge$Latitude)
new_pge$Longitude <- as.numeric(new_pge$Longitude)*-1
new_pge$Mesonet <- "PGE"
new_pge$Station.Name <- NA
new_pge$Elevation <- NA
new_pge$County <- NA
new_pge$First.Data.Date <- NA
new_pge$Last.Data.Date <- NA
new_pge$Status <- NA
new_pge$last.year <- NA
new_pge$Quality <- 2
new_pge$Tier <- 'IOU'

wx_stations_all <- bind_rows(wx_station, new_pge)
write_rds(wx_stations_all, "map_data/wx_station.rds")

