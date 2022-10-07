### Threshold output
### For output similarity scores, calculate threshold and apply to raster

library(tidyverse)
library(sf)
library(raster)

output_dir <- "data_out/"

# read in one prediction raster surface
pred_files <- list.files(output_dir)[grepl('model',list.files(output_dir))]

load(paste0(output_dir, pred_files[1]))

# eval threshold level - maximize sum of sensitivity and specificity

# thresh_out <- threshold(e3), stat = 'spec_sens')
