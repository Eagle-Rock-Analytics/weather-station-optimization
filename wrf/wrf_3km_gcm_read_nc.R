## 3km wrf from AE

library(raster)
library(ncdf4)

wrf <- nc_open("crnm-esm2_2015_2065_fire_vars.nc")

temp <- brick("crnm-esm2_2015_2065_fire_vars.nc", varname = "wind")
