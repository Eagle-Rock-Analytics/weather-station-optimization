# install.packages(c('raster', 'tidyverse', 'geodata', 'sf'))
# install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-1.tar.gz", repos = NULL, type = "source")

library(terra)
library(sf)
library(tidyverse)
library(tigris)
library(geodata)

# Output directory
map_data <- "map_data/"

# Create directories if they do not exist
if (!dir.exists(map_data)) {
  dir.create(map_data, recursive = TRUE)
}
if (!dir.exists(paste0(map_data, "worldclim/bio_11/"))) {
  dir.create(paste0(map_data, "worldclim/bio_11/"), recursive = TRUE)
}
if (!dir.exists(paste0(map_data, "worldclim/bio_12/"))) {
  dir.create(paste0(map_data, "worldclim/bio_12/"), recursive = TRUE)
}

download_path_11 <- paste0(map_data, "worldclim/bio_11/")
download_path_12 <- paste0(map_data, "worldclim/bio_12/")


# Download WorldClim data using geodata package
#bioclim.data.11 <- geodata::worldclim_global(var = "bio", res = 0.5, lon = -125, lat = 38.5, path=download_path_11)
#bioclim.data.12 <- geodata::worldclim_global(var = "bio", res = 0.5, lon = -105, lat = 38.5, path=download_path_12)

# Set the download path
bio_11_path <- "C:/Users/jesse/weather_station/weather-station-optimization/map_data/worldclim/bio_11/climate/wc2.1_10m/"
bio_12_path <- "C:/Users/jesse/weather_station/weather-station-optimization/map_data/worldclim/bio_12/climate/wc2.1_10m/"

# List files in the specified directories
bioclim.11.files <- list.files(bio_11_path, pattern = ".tif$", full.names = TRUE)
print(bioclim.11.files)  # Debugging: Check the files found

if (length(bioclim.11.files) == 0) {
  stop("No files found for bio_11")
}

bioclim.12.files <- list.files(bio_12_path, pattern = ".tif$", full.names = TRUE)
print(bioclim.12.files)  # Debugging: Check the files found

if (length(bioclim.12.files) == 0) {
  stop("No files found for bio_12")
}

# Load each file and check the number of layers
for (file in bioclim.11.files) {
  r <- rast(file)
  print(r)
  print(nlyr(r))  # Number of layers in each file
}

for (file in bioclim.12.files) {
  r <- rast(file)
  print(r)
  print(nlyr(r))  # Number of layers in each file
}

# Load each raster file separately
rasters_11 <- rast(bioclim.11.files)
rasters_12 <- rast(bioclim.12.files)

# Stack the raster files into a single object
#bioclim.data <- c(rasters_11, rasters_12)
bioclim.data <- raster::merge(rasters_11,rasters_12)


#Subset raster stacks to California domain
library(tigris)
states.sf <-states(cb=FALSE)
ca.sf <- states.sf %>%
  filter(STUSPS=='CA')

# Ensure CRS match and mask out data outside CA
ca.sf <- st_transform(ca.sf, crs(bioclim.data))
bioclim.data.CA <- mask(bioclim.data, ca.sf)

# Plot the cropped raster
plot(bioclim.data.CA)

# Specify the output file path and name
outfile <- paste0(map_data, "CA_worldclim_crop.tif")

# Write the cropped raster to a GeoTIFF file
writeRaster(bioclim.data.CA, filename = outfile, overwrite = TRUE)

# Print a message indicating where the file is saved
cat("Raster cropped to California extent saved to:", outfile, "\n")
