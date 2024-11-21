# Load necessary libraries
library(sp)
library(ggplot2)
library(gridExtra)
# Install remotes package if not already installed
if (!require(remotes)) {
  install.packages("remotes")
}

if (!requireNamespace("FNN", quietly = TRUE)) {
  remotes::install_github("cran/FNN")
}
library(FNN)

# Install and load necessary package for boundaries
if (!require(maps)) {
  install.packages("maps")
}
library(maps)

if (!require(raster)) {
  install.packages("raster")
}
library(raster)


# Step 1: Load the .RDS files
old_data_dir <- 'old_liberty/'
new_data_dir <- 'new_liberty/'

files <- c('predictionLibertyUtilities_wxstathresh_2_grace_old.RDS', 
           'predictionLibertyUtilities_wxstathresh_2_jesse_new.RDS')

# Read in the raster data as SpatialPixelsDataFrame objects
old_liberty_stations <- readRDS(file.path(old_data_dir, files[1]))
new_liberty_stations <- readRDS(file.path(new_data_dir, files[2]))

# Check CRS of both datasets
crs_old <- proj4string(old_liberty_stations)
crs_new <- proj4string(new_liberty_stations)

cat("Old Liberty CRS:\n", crs_old, "\n")
cat("New Liberty CRS:\n", crs_new, "\n")

# If the CRS are different, transform the new dataset to match the old dataset
if (crs_old != crs_new) {
  new_liberty_stations <- spTransform(new_liberty_stations, CRS(crs_old))
  cat("CRS of new_liberty_stations transformed to match old_liberty_stations.\n")
}


# Step 2: Convert SpatialPixelsDataFrame to Data Frame
convert_to_df <- function(spdf) {
  df <- as.data.frame(spdf)
  coords <- coordinates(spdf)
  df$lon <- coords[, 1]
  df$lat <- coords[, 2]
  return(df)
}

old_liberty_df <- convert_to_df(old_liberty_stations)
new_liberty_df <- convert_to_df(new_liberty_stations)

# Print summaries of the converted data frames
cat("Summary of Old Liberty Stations Data Frame:\n")
print(summary(old_liberty_df))

cat("\nSummary of New Liberty Stations Data Frame:\n")
print(summary(new_liberty_df))

# Step 3: Plot using ggplot2
plot_data <- function(data, title) {
  ggplot(data, aes(x = lon, y = lat, color = layer)) +
    geom_point() +
    ggtitle(title) +
    xlab("Longitude") +
    ylab("Latitude") +
    scale_color_viridis_c() # Optional: use viridis color scale for better color perception
}

# Create plots
plot_old <- plot_data(old_liberty_df, "Original Liberty Station Similarity Score Analysis") +
  labs(subtitle = "Weather Station Quality Threshold 2 of 5: iou/state") +
  theme(
    plot.title = element_text(size = 16),  # Adjust the title size
    plot.subtitle = element_text(size = 10)  # Adjust the subtitle size
  )

plot_new <- plot_data(new_liberty_df, "2024 Updated Liberty Stations Similarity Score") +
  labs(subtitle = "Weather Station Quality Threshold 2 of 5: iou/state") +
  theme(
    plot.title = element_text(size = 16),  # Adjust the title size
    plot.subtitle = element_text(size = 10)  # Adjust the subtitle size
  )

# Arrange plots side by side
grid.arrange(plot_old, plot_new, ncol = 2)

# Convert to matrices for nearest neighbor search
old_coords <- as.matrix(old_liberty_df[, c("lon", "lat")])
new_coords <- as.matrix(new_liberty_df[, c("lon", "lat")])

# Check if both coordinate matrices are not empty
cat("Number of points in Old Coordinates:", nrow(old_coords), "\n")
cat("Number of points in New Coordinates:", nrow(new_coords), "\n")

# Find the nearest neighbor in the new data for each point in the old data
nearest <- get.knnx(new_coords, old_coords, k = 1)

# Debug: Check the output of nearest neighbors
cat("Nearest neighbor indices:\n")
print(head(nearest$nn.index))

# Debug: Check the distances to nearest neighbors
cat("Distances to nearest neighbors:\n")
print(head(nearest$nn.dist))

# Rename columns in the new_liberty_df to avoid name conflicts
colnames(new_liberty_df) <- paste0(colnames(new_liberty_df), "_new")

# Check the new column names of new_liberty_df
cat("New column names of new_liberty_df:\n")
print(colnames(new_liberty_df))

# Add nearest neighbor indices to old data
old_liberty_df$nearest_new_index <- nearest$nn.index[, 1]

# Merge based on nearest neighbor indices
merged_df <- cbind(old_liberty_df, new_liberty_df[old_liberty_df$nearest_new_index, ])

# Check the column names of the merged data frame
cat("Column names of merged_df after renaming:\n")
print(colnames(merged_df))

# Calculate the difference between the layers as New Layer - Old Layer
merged_df$layer_diff <- merged_df$layer_new - merged_df$layer

# Filter merged_df to include only positive differences
positive_diff_df <- subset(merged_df, layer_diff > 0)

# Print summary of the merged data frame with differences
cat("Summary of Merged Data Frame with Differences (Nearest Neighbor):\n")
print(summary(positive_diff_df))

# Define reasonable coordinate limits
lon_limit <- range(c(positive_diff_df$lon_new, new_liberty_df$lon_new))
lat_limit <- range(c(positive_diff_df$lat_new, new_liberty_df$lat_new))


simple_plot_diff <- plot_data(positive_diff_df, "Similarity Score Difference for Liberty Utilities") +
  labs(subtitle = "Weather Station Quality Threshold 2 of 5: iou/state") +
  theme(
    plot.title = element_text(size = 16),  # Adjust the title size
    plot.subtitle = element_text(size = 10)  # Adjust the subtitle size
  )
# Print the positive difference plot
#print(simple_plot_diff)

# Set a seed for reproducibility
#set.seed(123)

# Randomly select 5 rows from the merged data frame
random_rows <- merged_df[sample(1:nrow(merged_df), 5), ]

# Display the selected rows with relevant columns
cat("Randomly selected spots for verification:\n")
print(random_rows[, c("lon", "lat", "layer", "layer_new", "layer_diff")])

# Verify the subtraction for the random spots
cat("\nVerification of subtraction (New - Old):\n")
print(random_rows$layer_new - random_rows$layer)

# Count positive differences
positive_diff_count <- nrow(subset(merged_df, layer_diff > 0))
# Count negative differences
negative_diff_count <- nrow(subset(merged_df, layer_diff < 0))

# Print the counts
cat("Number of positive layer_diff values:", positive_diff_count, "\n")
cat("Number of negative layer_diff values:", negative_diff_count, "\n")

# Add a binary column for positive/negative layer_diff values
merged_df$layer_diff_binary <- ifelse(merged_df$layer_diff > 0, "Improvement", "No Improvement")

# Define a color scheme for the binary plot
binary_colors <- c("Improvement" = "#5959c0", "No Improvement" = "#cf3030")

# Create a binary difference plot
binary_diff_plot <- ggplot(merged_df, aes(x = lon_new, y = lat_new, color = layer_diff_binary)) +
  geom_point(size = 2) +
  ggtitle("Similarity Score Improvement from Original Analysis - Binary") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_manual(values = binary_colors) +  # Use custom colors
  coord_cartesian(xlim = lon_limit, ylim = lat_limit) +  # Set coordinate limits
  common_theme


dir.create("all_liberty_figs", showWarnings = FALSE)

ggsave("plots/old_liberty_stations_plot.png", plot = plot_old, width = 8, height = 6, dpi = 300)
ggsave("plots/new_liberty_stations_plot.png", plot = plot_new, width = 8, height = 6, dpi = 300)
ggsave("plots/positive_improvement_plot.png", plot = simple_plot_diff, width = 8, height = 6, dpi = 300)
ggsave("plots/binary_difference_plot.png", plot = binary_diff_plot, width = 8, height = 6, dpi = 300)

# Print column names of new_liberty_df
print(colnames(new_liberty_df))

# Print column names of positive_diff_df
print(colnames(positive_diff_df))


# Convert to SpatialPointsDataFrame
new_liberty_spdf <- SpatialPointsDataFrame(coords = new_liberty_df[, c("lon_new", "lat_new")],
                                           data = new_liberty_df,
                                           proj4string = CRS(crs_old))  # Use the CRS from old data

positive_diff_spdf <- SpatialPointsDataFrame(coords = positive_diff_df[, c("lon_new", "lat_new")],
                                             data = positive_diff_df,
                                             proj4string = CRS(crs_old))  # Use the CRS from old data

# Save new_liberty_df as shapefile
shapefile(new_liberty_spdf, filename = "plots/updated_liberty_stations.shp", overwrite = TRUE)

# Save positive_diff_df as shapefile
shapefile(positive_diff_spdf, filename = "plots/similarity_score_difference_between_stations.shp", overwrite = TRUE)
