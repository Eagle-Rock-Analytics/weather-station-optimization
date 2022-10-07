## Write RDS prediction layers to raster .grd files

library(raster)
library(tidyverse)

# Directories
out_dir <- 'data_out/'
raster_dir <- 'pred_rasters/'

# List of model files
model_files <- list.files(out_dir)[grepl('^model_', list.files(out_dir)) & !grepl('potential_sites', list.files(out_dir))]

purrr::map(model_files, ~{
  f <- .
  
  load(paste0(out_dir, f))
  
  filename <- substr(f, 1, nchar(f)-6)
  
  writeRaster(maxent.pred, paste0(raster_dir, filename, "_simscore.grd"), overwrite = T)
  writeRaster(thresh_raster, paste0(raster_dir, filename, "_threshscore.grd"), overwrite = T)
})

# 
# load('data_out/model_PacificGas&ElectricCompany_wxstathresh_2.RData')
# writeRaster(maxent.pred, "predictionPacificGas&ElectricCompany_wxstathresh_2_current_sites.grd")
# 
# load('data_out/model_PacificGas&ElectricCompany_wxstathresh_2_potential_sites.RData')
# writeRaster(maxent.pred, "predictionPacificGas&ElectricCompany_wxstathresh_2_proposed_sites.grd")
