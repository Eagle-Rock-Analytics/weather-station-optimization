# -*- coding: utf-8 -*-
"""
Created on Fri Jul  1 15:11:30 2022

@author: Grace Di Cecco
"""

import xarray as xr
import fire_wx_fns as wx

# read in netcdf
fire_yrs = xr.open_dataset("C:/git/weather-station-optimization/crnm-esm2_2015_2065_fire_vars.nc")

# convert rel humid to percent
rh_pct = wx.convert_rh('ratio', fire_yrs['rel_humid'])

# create daily total precip
daily_precip = fire_yrs['total_precip'].resample(time="D").sum(keep_attrs=True)

# create daily max temp
max_temp = fire_yrs['T2'].resample(time="D").max(keep_attrs=True)

max_f = wx.convert_temp('kelvin', max_temp)
wind_mph = wx.convert_windspeed('mph', fire_yrs['wind'])

wind_mean = wind_mph.resample(time='D').mean(keep_attrs=True)

rh_mean = rh_pct.resample(time='D').mean(keep_attrs=True)

ffwi_daily = wx.calc_ffwi(max_f, rh_mean, wind_mean)