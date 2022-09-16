## Write RDS prediction layers to raster .grd files

library(raster)

load('data_out/model_PacificGas&ElectricCompany_wxstathresh_2.RData')
writeRaster(maxent.pred, "predictionPacificGas&ElectricCompany_wxstathresh_2_current_sites.grd")

load('data_out/model_PacificGas&ElectricCompany_wxstathresh_2_potential_sites.RData')
writeRaster(maxent.pred, "predictionPacificGas&ElectricCompany_wxstathresh_2_proposed_sites.grd")
