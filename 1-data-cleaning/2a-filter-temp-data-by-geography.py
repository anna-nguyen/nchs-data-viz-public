############################################
# Filter global TerraClimate data to only US
############################################

import netCDF4 as nc
import numpy as np
import os
import urllib.request
import xarray as xr

def subset_data(filename, temp_type): 
    rawdata = xr.open_dataset(os.path.join('..', 'data', 'temp_data', 'raw', filename))
    
    lon_bnds = [-126.7, -63.6]
    lat_bnds = [24.1, 49.4]

    temp = rawdata[temp_type]
    filtered = temp.where((temp.lon > lon_bnds[0]) & (temp.lon < lon_bnds[1]) & 
                   (temp.lat > lat_bnds[0]) & (temp.lat < lat_bnds[1]), 
                drop=True)

    new_filename = 'filtered_' + filename

    filtered.to_netcdf(os.path.join('..', 'data', 'temp_data', 'filtered', new_filename))

for year in np.arange(2015, 2021):
    subset_data("TerraClimate_tmax_" + str(year) + ".nc", "tmax")
    subset_data("TerraClimate_tmin_" + str(year) + ".nc", "tmin")
