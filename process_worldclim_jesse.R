#install.packages(c('raster', 'tidyverse', 'geodata', 'sf'))
#install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-1.tar.gz", repos = NULL, type = "source")

library(terra)
library(sf)
library(tidyverse)
library(tigris)
library(geodata)
library(tigris)

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
all_bioclim_data_path <- "C:/Users/jespi/liberty_station_update/weather-station-optimization/map_data/worldclim/wc2.1_30s_bio/"

# List files in the specified directories
bioclim.all.files <- list.files(all_bioclim_data_path, pattern = ".tif$", full.names = TRUE)
print(bioclim.all.files)  # Debugging: Check the files found

if (length(bioclim.all.files) == 0) {
  stop("No files found for all bioclim files")
}

for (file in bioclim.all.files) {
  r <- rast(file)
  print(r)
  print(nlyr(r))
}

# Load each raster file separately
rasters_bioclim <- rast(bioclim.all.files)

# Stack the raster files into a single object
bioclim.data <- raster::merge(rasters_11,rasters_12)


#Subset raster stacks to California domain
states.sf <-states(cb=FALSE)
ca.sf <- states.sf %>%
  filter(STUSPS=='CA')

# Ensure CRS match and mask out data outside CA
#ca.sf <- st_transform(ca.sf, crs(bioclim.data))
#bioclim.data.CA <- mask(bioclim.data, ca.sf)

ca.sf <- st_transform(ca.sf, crs(rasters_bioclim))
all_bioclim.data.CA <- mask(rasters_bioclim, ca.sf)

# Plot the cropped raster
plot(all_bioclim.data.CA)

# Specify the output file path and name
#outfile <- paste0(map_data, "CA_worldclim_crop.tif")
outfile <- paste0(map_data, "all_CA_worldclim_crop.tif")


# Write the cropped raster to a GeoTIFF file
writeRaster(all_bioclim.data.CA, filename = outfile, overwrite = TRUE)

# Print a message indicating where the file is saved
# cat("Raster cropped to California extent saved to:", outfile, "\n")