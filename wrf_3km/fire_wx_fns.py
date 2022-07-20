# -*- coding: utf-8 -*-
"""
Created on Fri Jul  1 15:17:37 2022

Functions for converting climate to fire weather variables

@author: Grace Di Cecco
"""

# convert rh from ratio [0,1] to percentage [0,100] and vice versa
# provide starting units of humidity value and humidity to convert
def convert_rh(start_units, humidity):
    valid_units = {"percentage", "ratio"}
    
    if start_units not in valid_units:
        raise ValueError("units to convert must be one of %r." % valid_units)
        
    elif start_units == "percentage":
        converted_val = humidity/100
    
    elif start_units == "ratio":
        converted_val = humidity*100
    
    return converted_val

# convert temperature from K to F and vice versa
# provide starting units and temperature value to convert
# would be nice if this did celsius too
def convert_temp(start_units, temperature):
    valid_units = {"kelvin", "fahrenheit"}
    
    if start_units not in valid_units:
        raise ValueError("units to convert must be one of %r." % valid_units)
        
    elif start_units == "kelvin":
        converted_val = (temperature - 273.15)*9/5 + 32
        
    elif start_units == "fahrenheit":
        converted_val = (temperature - 32)*5/9 + 273.15
    
    return converted_val

# convert wind speed from m/s to mph and vice versa
# provide starting units and wind value to convert

def convert_windspeed(start_units, wind):
    valid_units = {"m/s", "mph"}
    
    if start_units not in valid_units:
        raise ValueError("units to convert must be one of %r." % valid_units)
        
    elif start_units == "m/s":
        converted_val = wind*2.2369
        
    elif start_units == "mph":
        converted_val = wind/2.2369
    
    return converted_val

# calculate equilibrium moisture content
# fn of temperature in F and relative humidity (0-100)
def calc_emc(T, H):
    if H < 10:
        emc = 0.03229 + 0.281073*H - 0.000578*T*H

    elif H > 50:
        emc = 21.0606 + 0.005565*(H**2.0) - 0.00035*H*T - 0.483199*H

    else: 
        emc = 2.22749 + 0.160107*H - 0.014784*T

    return emc

# Calculate VPD
# Fn inputs: temperature in F and relative humidity [0,100], is temperature in F
# convert temperature to Celsius
# SVP = 610.78 x e^(T / (T +238.3) x 17.2694))
# SVP x (1 – RH/100) = VPD

def calc_vpd(temperature, rh, temp_in_f):
    
    # would be nice if this checked for appropriate units and converted if needed
    
    from math import e
    
    if temp_in_f == True:
        t = (temperature - 32)*5/9
        svp = 610.78*e**(t/(t+238.2)*17.2694)
        vpd = svp*(1-rh/100)
    
    elif temp_in_f == False:
        raise ValueError("temperature needs to be in fahrenheit")
    
    return vpd

# Calculate FFWI
# Fn of daily max temp, relative humidity, wind speed
# Needs temp in units of F, RH in percentage, wind speed in mph
def calc_ffwi(tmax, rh, spd):
    
    # would be nice if this checked for appropriate units
    
    c1 = 1.0
    c2 = 1.5
    c3 = -0.5

    em0 = 0.72
    em = 0.000002

    bf = 1. / 0.3002

    emc = calc_emc(tmax, rh)

    eta = 1 - 2 * (emc / 30) + c2 * (emc / 30) ^ 2 + c3 * (emc / 30) ^ 3

    FFWI = bf * eta * (1 + spd^2.)^0.5
    return FFWI