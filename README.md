# weather-station-optimization
Weather station optimization work for Pyregence
Authors: Grace Di Cecco & Owen Doherty
Organization: Eagle Rock Analytics

### Overview

This repo contains code to run MaxEnt models to create maps showing estimated similarity scores for grid cells in California, within IOU service territories and fire weather regions, measuring similarity of climate conditions in those cells to climate conditions captured by existing networks of weather stations. These similarity scores can also be used to estimate the similarity of potential new weather station sites to existing monitoring networks, to optimize placement of new stations to capture dissimilar climate conditions.

### Input data

Input mapping files including IOU service territory and fire weather region shapefiles are in `/map_data`

Candidate new weather station locations are in `/weather_stations`

4-km WRF data is stored in `/wrf`, and regridded 4-km WRF data as raster layers is stored in `/wrf-out`

Regridded 3-km WRF data is stored in `/wrf_3km`

### Code

Scripts to run different iterations of MaxEnt are as follows:

*Present day*
- `maxent_v0.8.R` runs MaxEnt models for fire weather regions and IOU service territories for present-day climate conditions (input climate layers from WorldClim at 1-km resolution)
- `statewide_v1.R` runs MaxEnt models for the whole state of California for present-day climate conditions (input climate layers from WorldClim at 1-km resolution)
- `process_worldclim.R` formats WorldClim data into raster layers for inputting into MaxEnt

*Future climate*
- `maxent_wrf4km.R` and `statewide_wrf4km.R` run MaxEnt models for fire weather regions and IOU service territories, and statewide for CA, respectively, using future climate projections downscaled with WRF at 4-km resolution
- `maxent_wrf3km.R` runs MaxEnt models for fire weather regions and IOU service territories using future climate projections downscaled with WRF at 3-km resolution

### Output data

MaxEnt model fit results from the above scripts are stored in `/data_out`

Figures from the above scripts are stored in `/figs`

